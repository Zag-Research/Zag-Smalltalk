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
const CompiledMethod = execute.CompiledMethod;
const Sym = zag.symbol.symbols;
pub const ContextPtr = *Context;
pub var nullContext = Context.init();
const Self = @This();
const Context = Self;
header: HeapHeader,
method: *const CompiledMethod,
tpc: PC, // threaded PC
npc: ThreadedFn, // native PC - in Continuation Passing Style
prevCtxt: ?ContextPtr,
trapContextNumber: u64,
contextData: *ContextData,
const baseSize = @sizeOf(Self) / @sizeOf(Object);
pub const ContextData = struct {
    header: HeapHeader,
    contextData: [1]Object,
    fn init(self: *ContextData, locals: u11, selfOffset: u11, length: u11) void {
        self.header = HeapHeader.headerOnStack(.ContextData, selfOffset, length);
        @setRuntimeSafety(false);
        for (self.contextData[0..locals]) |*local| {
            local.* = Nil;
        }
    }
    fn objects(self: *ContextData) [*]Object {
        return @ptrFromInt(@intFromPtr(self));
    }
    fn slice(self: *ContextData) []Object {
        return self.objects()[1 .. self.header.length + 1];
    }
    fn localAddress(self: *ContextData, indices: Object) *Object {
        var ref: u64 = indices.toNatNoCheck();
        var objs = self.objects();
        for (0..4) |i| {
            const result = &objs[ref & 0x7ff];
            if (ref <= 0x7ff) return result;
            if (i < 3) {
                ref = ref >> 11;
                objs = result.*.to([*]Object);
            }
        }
        unreachable;
    }
};
pub fn init() Self {
    var result: Self = undefined;
    const end = &execute.endMethod;
    const pc = PC.init(&end.code[0]);
    result.header = comptime HeapHeader.calc(.Context, baseSize, 0, .static, null, Object, false) catch unreachable;
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
    try writer.print("}}", .{});
}
inline fn headerOf(self: *const Context) *HeapHeader {
    return @as(HeapObjectPtr, @constCast(@ptrCast(self))).headerPtr();
}
pub inline fn popTargetContext(self: *Context, sp: SP, target: *Context, process: *Process, result: Object) struct { sp: SP, ctxt: *Context } {
    _ = .{ self, target, process };
    const newSp = sp;
    newSp.top = result;
    return .{ .sp = newSp, .ctxt = unreachable };
}
pub inline fn pop(self: *Context, process: *Process) struct { sp: SP, ctxt: *Context } {
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
pub fn push(self: ContextPtr, sp: SP, process: *Process, method: *const CompiledMethod) ContextPtr {
    const stackStructure = method.stackStructure;
    const locals = stackStructure.locals;
    const spForLocals = sp.reserve(locals + 1);
    const contextData: *ContextData = @ptrCast(spForLocals);
    const length: u11 = @truncate(spForLocals.delta(self.endOfStack() orelse process.endOfStack()) - 1);
    contextData.init(locals, stackStructure.selfOffset, length);
    const newSp = spForLocals.reserve(baseSize);
    const ctxt = @as(*align(@alignOf(Self)) Context, @ptrCast(@alignCast(newSp)));
    ctxt.prevCtxt = self;
    ctxt.trapContextNumber = process.header().trapContextNumber;
    ctxt.method = method;
    ctxt.contextData = contextData;
    ctxt.header = HeapHeader.headerOnStack(.Context, 0, baseSize);
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
pub inline fn endOfStack(self: *const Context) ?SP {
    if (!self.isOnStack()) return null;
    return @ptrCast(@constCast(self));
}
pub inline fn callerStack(self: *const Context) SP {
    return @constCast(@ptrCast(self.contextData.objects()));
}
inline fn tempSize(self: *const Context, process: *const Process) usize {
    return (@intFromPtr(self.previous().endOfStack() orelse process.endOfStack()) - @intFromPtr(self.contextData)) / @sizeOf(Object) - 1;
}
pub fn stack(self: *const Self, sp: SP, process: *const Process) []Object {
    if (self.isOnStack())
        return sp.slice((@intFromPtr(self.endOfStack() orelse process.endOfStack()) - @intFromPtr(sp)) / @sizeOf(Object) - 1);
    return process.getStack(sp);
}
// pub inline fn allLocals(self: *const Context, process: *const Process) []Object {
//     const size = self.tempSize(process);
//     @setRuntimeSafety(false);
//     return @constCast(self.temps[0..size]);
// }
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
pub inline fn getLocal(self: *const Context, indices: Object) Object {
    return self.contextData.localAddress(indices).*;
}
pub inline fn setLocal(self: ContextPtr, indices: Object, v: Object) void {
    self.contextData.localAddress(indices).* = v;
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
pub const threadedFunctions = struct {
    const tf = zag.threadedFn.Enum;
    const expect = std.testing.expect;
    pub const makeImmediateClosure = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) Result {
            _ = .{ pc, sp, process, context, signature, unreachable };
        }
    };
    pub const popLocal = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
            const contextData = context.contextData;
            contextData.localAddress(pc.object()).* = sp.top;
            const newSp = sp.drop();
            return @call(tailCall, process.check(pc.prim2()), .{ pc.skip(2), newSp, process, context, Extra{ .contextData = contextData } });
        }
    };
    pub const popLocalViaExtra = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            const contextData = extra.contextData;
            contextData.localAddress(pc.object()).* = sp.top;
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
        // test "init context" {
        //     //    const expectEqual = std.testing.expectEqual;
        //     //    const objs = comptime [_]Object{True,Object.from(42)};
        //     std.debug.print("init: 1\n", .{});
        //     var result = execute.Execution.initTest("init context",.{});
        //     std.debug.print("init: 2\n", .{});
        //     var c = result.ctxt;
        //     std.debug.print("init: 3\n", .{});
        //     var process = &result.process;
        //     std.debug.print("init: 4\n", .{});
        //     //c.print(process);
        //     std.debug.print("init: 5\n", .{});
        //     //    try expectEqual(result.o()[3].u(),4);
        //     //    try expectEqual(result.o()[6],True);
        //     const sp = process.endOfStack();
        //     std.debug.print("init: 6\n", .{});
        //     const newC = c.moveToHeap(sp, process);
        //     std.debug.print("init: 7\n", .{});
        //     newC.print(process);
        //     std.debug.print("init: 8\n", .{});
        // }
    };
    pub const pushLocal = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
            const contextData = context.contextData;
            const newSp = sp.push(contextData.localAddress(pc.object()).*);
            return @call(tailCall, process.check(pc.next().prim2()), .{ pc.next2(), newSp, process, context, Extra{ .contextData = contextData } });
        }
    };
    pub const pushLocalViaExtra = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            const contextData = extra.contextData;
            const newSp = sp.push(contextData.localAddress(pc.object()).*);
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
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
            const contextData = context.contextData;
            contextData.localAddress(pc.object()).* = sp.top;
            return @call(tailCall, process.check(pc.prim2()), .{ pc.skip(2), sp, process, context, Extra{ .contextData = contextData } });
        }
    };
    pub const storeLocalViaExtra = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            const contextData = extra.contextData;
            contextData.localAddress(pc.object()).* = sp.top;
            return @call(tailCall, process.check(pc.prim2()), .{ pc.skip(2), sp, process, context, extra });
        }
    };
};
