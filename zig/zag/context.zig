const std = @import("std");
const zag = @import("zag.zig");
const config = zag.config;
const tailCall = config.tailCall;
const trace = config.trace;
const checkEqual = zag.utilities.checkEqual;
const Process = zag.Process;
const object = zag.object;
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const heap = zag.heap;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObject = heap.HeapObject;
const HeapHeader = heap.HeapHeader;
const Format = heap.Format;
const Age = heap.Age;
//const class = zag.class;
const execute = zag.execute;
const SendCache = execute.SendCache;
const Code = execute.Code;
const PC = execute.PC;
const SP = execute.SP;
const Extra = execute.Extra;
const Result = execute.Result;
const Execution = execute.Execution;
const ThreadedFn = execute.ThreadedFn;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const Sym = zag.symbol.symbols;
pub const ContextPtr = *Context;
pub var nullContext = Context.init();
const Self = @This();
const Context = Self;
header: HeapHeader,
method: CompiledMethodPtr,
tpc: PC, // threaded PC
npc: ThreadedFn, // native PC - in Continuation Passing Style
prevCtxt: ?ContextPtr,
trapContextNumber: u64,
temps: [nLocals]Object,
const nLocals = 1;
const baseSize = @sizeOf(Self) / @sizeOf(Object) - nLocals;
pub fn init() Self {
    var result: Self = undefined;
    const end = &execute.endMethod;
    const pc = PC.init(&end.code[0]);
    result.header = comptime HeapHeader.calc(.Context, baseSize + nLocals, 0, .static, null, Object, false) catch unreachable;
    result.method = @constCast(end);
    result.npc = pc.asThreadedFn();
    result.tpc = pc.next();
    result.prevCtxt = null;
    result.trapContextNumber = 0;
    return result;
}
pub fn format(
    self: *const Context,
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;

    try writer.print("context: {{", .{});
//    try writer.print(".header: {}", .{self.header});
//    try writer.print(".method: {}", .{self.method});
    try writer.print(".npc: {}", .{self.npc});
    try writer.print(".tpc: {}", .{self.tpc});
    try writer.print(".trapContextNumber: {}", .{self.trapContextNumber});
    if (self.prevCtxt) |ctxt|
        try writer.print(" prev: 0x{x}", .{@intFromPtr(ctxt)});
    if (false) {
        @setRuntimeSafety(false);
        try writer.print(" temps: {any}", .{self.temps[0..self.size]});
    }
    try writer.print("}}",.{});
}
inline fn headerOf(self: *const Context) *HeapHeader {
    return @as(HeapObjectPtr, @constCast(@ptrCast(self))).headerPtr();
}
pub inline fn pop(self: *Context, process: *Process) struct { sp: SP, ctxt: ContextPtr } {
    _ = process;
    const wordsToDiscard = self.header.hash16();
    trace("\npop: 0x{x} {} sp=0x{x} {}", .{ @intFromPtr(self), self.header, @intFromPtr(self.asNewSp().unreserve(wordsToDiscard + 1)), wordsToDiscard });
    if (self.isOnStack())
        return .{ .sp = self.asNewSp().unreserve(wordsToDiscard), .ctxt = self.previous() };
    trace("\npop: {*}", .{self});
    @panic("incomplete");
    // const itemsToKeep = self.temps[wordsToDiscard-baseSize..self.size];
    // const newSp = process.endOfStack() - itemsToKeep.len;
    // for (itemsToKeep,0..) | obj,index | {
    //     newSp[index] = obj;
    // }
    // return .{.sp=newSp,.ctxt=self.previous()};
}
pub fn push(self: ContextPtr, sp: SP, process: *Process, method: CompiledMethodPtr) ContextPtr {
    const stackStructure = method.stackStructure;
    const locals = stackStructure.f1;
    const maxStackNeeded = stackStructure.f2;
    const reserve = baseSize + 1 + locals + maxStackNeeded;
    const newSp = (process.allocStackSpace(sp, reserve) catch {
        var contextMutable = self;
        const newerSp = process.spillStack(sp, &contextMutable);
        return contextMutable.push(newerSp, process, method);
    }).unreserve(maxStackNeeded);
    const ctxt = @as(*align(@alignOf(Self)) Context, @ptrCast(@alignCast(newSp.unreserve(1))));
    ctxt.prevCtxt = self;
    ctxt.trapContextNumber = process.header().trapContextNumber;
    ctxt.method = method;
    {
        @setRuntimeSafety(false);
        for (ctxt.temps[0..locals]) |*local| {
            local.* = Nil;
        }
    }
    const selfOffset = stackStructure.f3;
    ctxt.header = HeapHeader.contextHeaderOnStack(baseSize + selfOffset);
    return ctxt;
}
pub fn moveToHeap(self: *const Context, sp: SP, process: *Process) ContextPtr {
    _ = self;
    _ = sp;
    _ = process;
    unreachable;
    // if (self.isIncomplete()) {
    //     self.header = heap.header(4, Format.bothAP, class.Context_C,0,Age.stack);
    //     self.size = self.prevCtxt.calculatedSize(process);
    //     self.addr = @ptrCast(*Object,&self.temps);
    //     self.prevCtxt.moveToHeap(sp, process);
    // }
}
pub inline fn isOnStack(self: *const Self) bool {
    return self.header.isOnStack();
}
pub inline fn endOfStack(self: *const Context, process: *const Process) SP {
    if (!self.isOnStack()) return process.endOfStack();
    return @ptrCast(@constCast(self));
}
inline fn tempSize(self: *const Context, process: *const Process) usize {
    return (@intFromPtr(self.previous().endOfStack(process)) - @intFromPtr(&self.temps)) / @sizeOf(Object) - 1;
}
pub fn stack(self: *const Self, sp: SP, process: *const Process) []Object {
    if (self.isOnStack())
        return sp.slice((@intFromPtr(self.endOfStack(process)) - @intFromPtr(sp)) / @sizeOf(Object) - 1);
    return process.getStack(sp);
}
pub inline fn allLocals(self: *const Context, process: *const Process) []Object {
    const size = self.tempSize(process);
    @setRuntimeSafety(false);
    return @constCast(self.temps[0..size]);
}
pub inline fn getTPc(self: *const Context) PC {
    return self.tpc;
}
pub inline fn setReturnBoth(self: ContextPtr, npc: ThreadedFn, tpc: PC) void {
    trace("\nsetReturnBoth: {} {}", .{ npc, tpc });
    self.npc = npc;
    self.tpc = tpc;
}
pub inline fn setReturn(self: ContextPtr, tpc: PC) void {
    trace("\nsetReturn: {}", .{tpc});
    self.setReturnBoth(tpc.asThreadedFn(), tpc.next());
}
pub inline fn getNPc(self: *const Context) ThreadedFn.Fn {
    return self.npc.f;
}
pub inline fn setNPc(self: *Context, npc: ThreadedFn) void {
    self.npc = npc;
}
pub inline fn setTPc(self: *Context, tpc: PC) void {
    self.tpc = tpc;
}
pub inline fn getSelf(self: *const Context) Object {
    const wordsToDiscard = self.header.hash16();
    return self.asObjectPtr()[wordsToDiscard];
}
pub inline fn setResult(self: *const Context, value: Object) void {
    const wordsToDiscard = self.asHeapObjectPtr().hash16();
    self.asObjectPtr()[wordsToDiscard] = value;
}
pub inline fn getLocal(self: *const Context, n: usize) Object {
    @setRuntimeSafety(false);
    return self.temps[n];
}
pub inline fn setLocal(self: ContextPtr, n: usize, v: Object) void {
    @setRuntimeSafety(false);
    self.temps[n] = v;
}
pub inline fn previous(self: *const Context) ContextPtr {
    return self.prevCtxt orelse @panic("0 prev");
}
pub inline fn asHeapObjectPtr(self: *const Context) HeapObjectPtr {
    return &self.header;
}
pub inline fn asObjectPtr(self: *const Context) [*]Object {
    return @ptrCast(@constCast(self));
}
pub inline fn asNewSp(self: *const Context) SP {
    return @as(SP, @ptrCast(@constCast(self))).reserve(1);
}
pub inline fn cleanAddress(self: *const Context) u64 {
    return @intFromPtr(self);
}
inline fn fromObjectPtr(op: [*]Object) ContextPtr {
    return @as(ContextPtr, @ptrCast(op));
}
pub fn print(self: *const Context, process: *const Process) void {
    const pr = std.debug.print;
    pr("Context: {*} {} {any}\n", .{ self, self.header, self.allLocals(process) });
    if (self.prevCtxt) |ctxt| {
        ctxt.print(process);
    }
}
const e = struct {
    usingnamespace execute.controlPrimitives;
};
test "init context" {
    if (true) return error.SkipZigTest;
    //    const expectEqual = std.testing.expectEqual;
    //    const objs = comptime [_]Object{True,Object.from(42)};
    std.debug.print("init: 1\n", .{});
    var result = execute.Execution.new();
    std.debug.print("init: 2\n", .{});
    var c = result.ctxt;
    std.debug.print("init: 3\n", .{});
    var process = &result.process;
    std.debug.print("init: 4\n", .{});
    //c.print(process);
    std.debug.print("init: 5\n", .{});
    //    try expectEqual(result.o()[3].u(),4);
    //    try expectEqual(result.o()[6],True);
    const sp = process.endOfStack();
    std.debug.print("init: 6\n", .{});
    const newC = c.moveToHeap(sp, process);
    std.debug.print("init: 7\n", .{});
    newC.print(process);
    std.debug.print("init: 8\n", .{});
}
pub const threadedFunctions = struct {
    const tf = zag.threadedFn.Enum;
    const expect = std.testing.expect;
    pub const makeImmediateClosure = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) Result {
            _ = .{ pc, sp, process, context, signature, unreachable };
        }
    };
    pub const popLocal = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            context.setLocal(pc.uint(), sp.top);
            const newSp = sp.drop();
            return @call(tailCall, process.check(pc.prim2()), .{ pc.skip(2), newSp, process, context, extra });
        }
    };
    pub const pushContext = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            const method = extra.method;
            const ctxt = context.push(sp, process, method);
            const newSp = ctxt.asNewSp();
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, ctxt, extra });
        }
        test "pushContext" {
            var exe = Execution.initTest("pushContext", .{
                tf.pushContext,
                tf.pushLiteral,
                42,
            });
            try exe.execute(&[_]Object{Object.from(17)});
            try exe.matchStack(&[_]Object{Object.from(42)});
            try expect(exe.getContext() != &exe.ctxt);
        }
    };
    pub const pushLocal = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            const newSp = sp.push(context.getLocal(pc.uint()));
            trace("\npushLocal: {any} {any}", .{ context.stack(newSp, process), context.allLocals(process) });
            return @call(tailCall, process.check(pc.next().prim2()), .{ pc.next2(), newSp, process, context, extra });
        }
    };
    pub const pushLocalData = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            const ref = pc.uint();
            const local = context.getLocal(ref & 0xfff);
            const newSp = sp.push(local.getField(ref >> 12));
            trace("\npushLocalData: {} {} {any} {any}", .{ ref, local, context.stack(newSp, process), context.allLocals(process) });
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
        }
    };
    pub const pushLocalField = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            const ref = pc.uint();
            const local = context.getLocal(ref & 0xff);
            const newSp = sp.push(local.getField(ref >> 12));
            trace("\npushLocalField: {} {} {any} {any}", .{ ref, local, context.stack(newSp, process), context.allLocals(process) });
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
        }
    };
    pub const pushThisContext = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            const newSp = sp.push(Object.from(context));
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, context, extra });
        }
    };
    pub const storeLocal = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            context.setLocal(pc.uint(), sp.top);
            return @call(tailCall, process.check(pc.prim2()), .{ pc.skip(2), sp, process, context, extra });
        }
    };
};
    
