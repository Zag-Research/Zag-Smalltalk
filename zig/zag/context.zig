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
const ThreadedFn = execute.ThreadedFn;
const CompiledMethodPtr = execute.CompiledMethodPtr;
pub const ContextPtr = *Context;
pub var nullContext = Context.init();
const Self = @This();
const Context = Self;
header: HeapHeader,
method: CompiledMethodPtr,
tpc: PC, // threaded PC
npc: ThreadedFn, // native PC - in Continuation Passing Style
prevCtxt: ?ContextPtr, // note this is not an Object, so access and GC need to handle specially
trapContextNumber: u64,
temps: [nLocals]Object,
const nLocals = 1;
const baseSize = @sizeOf(Self) / @sizeOf(Object) - nLocals;
pub fn init() Self {
    var result: Self align(@alignOf(Context)) = undefined;
    const s: *Context = @ptrCast(&result);
    s.header = comptime HeapHeader.calc(.Context, baseSize + nLocals, 0, .static, null, Object, false) catch unreachable;
    s.npc = .{ .f = Code.end }; //@constCast(@ptrCast(&Code.end));
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

    try writer.print("context: {}", .{self.header});
    if (self.prevCtxt) |ctxt|
        try writer.print(" prev: 0x{x}", .{@intFromPtr(ctxt)});
    if (false) {
        @setRuntimeSafety(false);
        try writer.print(" temps: {any}", .{self.temps[0..self.size]});
    }
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
pub fn push(self: ContextPtr, sp: SP, process: *Process, method: CompiledMethodPtr, locals: u16, maxStackNeeded: u16, selfOffset: u16) ContextPtr {
    const newSp = (process.allocStackSpace(sp, baseSize + 1 + locals + maxStackNeeded) catch {
        var contextMutable = self;
        const newerSp = process.spillStack(sp, &contextMutable);
        return contextMutable.push(newerSp, process, method, locals, maxStackNeeded, selfOffset);
    }).unreserve(maxStackNeeded);
    trace("\npush: {} {} {} {}", .{ baseSize, locals, maxStackNeeded, selfOffset });
    trace("\npush: {} sp={*} newSp={*}", .{ method.signature, sp, newSp });
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
    ctxt.header = HeapHeader.contextHeaderOnStack(baseSize + selfOffset);
    trace("\npush: {}", .{ctxt.header});
    if (process.needsCheck()) @panic("process needsCheck");
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
    // TODO: account for BlockClosures and ContextData objects on stack
    return @ptrCast(@constCast(self));
}
inline fn tempSize(self: *const Context, process: *const Process) usize {
    return (@intFromPtr(self.previous().endOfStack(process)) - @intFromPtr(&self.temps)) / @sizeOf(Object) - 1;
}
pub fn stack(self: *const Self, sp: SP, process: *Process) []Object {
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
pub fn call(oldPc: [*]const Code, sp: SP, process: *Process, self: ContextPtr, selector: Object) SP {
    self.tpc = oldPc + 1;
    self.npc = oldPc[0].prim;
    trace("\ncall: N={} T={} {any}", .{ self.getNPc(), self.getTPc(), self.stack(sp, process) });
    const method = @as(CompiledMethodPtr, @ptrFromInt(@as(u64, @bitCast(selector))));
    const pc = @as([*]const Code, @ptrCast(&method.code));
    _ = .{ pc, oldPc, sp, process, selector, @panic("call unimplemented") };
    //        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,self,method.selector});
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
    pub const pushThisContext = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Self, extra: Extra) SP {
            const newSp = sp.push(Object.from(context));
            return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, extra });
        }
    };
};
