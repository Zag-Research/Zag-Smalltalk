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
            local.* = Nil();
        }
    }
    fn objects(self: *ContextData) [*]Object {
        return @ptrFromInt(@intFromPtr(self));
    }
    fn slice(self: *ContextData) []Object {
        return self.objects()[1 .. self.header.length + 1];
    }
    fn localAddress(self: *ContextData, r: usize) [*]Object {
        return self.objects() + r;
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
pub inline fn popTargetContext(target: *Context, process: *Process, result: Object) struct { SP, *Context } {
    //TODO: check if result is on the stack and ?copy to heap if so?
    const newSp, const newTarget = target.pop(process);
    newSp.top = result;
    return .{ newSp, newTarget };
}
pub inline fn pop(self: *Context, process: *Process) struct { SP, *Context } {
    _ = process;
    const wordsToDiscard = self.header.hash16();
    trace("\npop: 0x{x} {} {}", .{ @intFromPtr(self), self.header, wordsToDiscard });
    if (self.isOnStack())
        return .{ @as(SP, @ptrCast(self)).unreserve(wordsToDiscard - 1), self.previous() };
    trace("\npop: {*}", .{self});
    @panic("incomplete");
    // const itemsToKeep = self.temps[wordsToDiscard-baseSize..self.size];
    // const newSp = process.endOfStack() - itemsToKeep.len;
    // for (itemsToKeep,0..) | obj,index | {
    //     newSp[index] = obj;
    // }
    // return .{.sp=newSp,.ctxt=self.previous()};
}
pub fn push(self: ContextPtr, sp: SP, process: *Process, method: *const CompiledMethod) struct { SP, *Context } {
    const stackStructure = method.stackStructure;
    const locals = stackStructure.locals;
    const spForLocals = sp.safeReserve(locals + 1);
    const contextData: *ContextData = @ptrCast(spForLocals);
    const length: u11 = @truncate(spForLocals.delta(self.endOfStack() orelse process.endOfStack()) - 1);
    contextData.init(locals, stackStructure.selfOffset, length);
    const newSp = spForLocals.safeReserve(baseSize);
    const ctxt = @as(*align(@alignOf(Self)) Context, @ptrCast(@alignCast(newSp)));
    ctxt.prevCtxt = self;
    ctxt.trapContextNumber = process.header().trapContextNumber;
    ctxt.method = method;
    ctxt.contextData = contextData;
    ctxt.header = HeapHeader.headerOnStack(.Context, 0, baseSize);
    return .{ newSp.safeReserve(1), ctxt };
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
pub inline fn getLocalX(self: *const Context, indices: Object) Object {
    return self.contextData.localAddress(indices).*;
}
pub inline fn setLocalX(self: ContextPtr, indices: Object, v: Object) void {
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
inline fn asNewSpX(self: *const Context) SP {
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
    pr("Context: {*} {f} {any}\n", .{ self, self.header, self.allLocals(process) });
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
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            if (extra.noContext())
                return @call(tailCall, pushContext.threadedFn, .{ pc.prev(), sp, process, context, extra });
            const address = getAddress(pc.object(), sp, extra);
            address.* = sp.top;
            const newSp = sp.drop();
            return @call(tailCall, process.check(pc.prim2()), .{ pc.skip(2), newSp, process, context, extra });
        }
    };
    pub const pushContext = struct {
        /// any inlinePrimitive or threadedFn that needs a context can call this to create one
        /// and then re-execute the original operation; simply:
        /// return @call(tailCall, pushContext.threadedFn, .{ pc.prev(), sp, process, context, extra });
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            if (extra.getMethod()) |method| {
                const newSp, const ctxt = context.push(sp, process, method);
                return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, ctxt, Extra.fromContextData(ctxt.contextData) });
            }
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp, process, context, extra });
        }
        test "pushContext" {
            var exe = Execution.initTest("pushContext", .{
                tf.pushContext,
                tf.pushLiteral,
                42,
            });
            try exe.execute(&[_]Object{Object.from(17, null)});
            try exe.matchStack(&[_]Object{Object.from(42, null)});
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
    fn getAddress(obj: Object, sp: SP, extra: Extra) *Object {
        if (obj.nativeU()) |r| {
            var objs: [*]Object =
                if (extra.addressIfNoContext((r >> 8) & 0xff, sp)) |stackOffsetAddress| stackOffsetAddress else extra.getContextData().localAddress(r & 0xff);
            var ref = r >> 16;
            while (ref > 0) {
                objs = objs[ref & 0x3ff].to([*]Object);
                ref = ref >> 10;
            }
            return &objs[0];
        }
        unreachable;
    }
    pub const pushLocal = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            const address = getAddress(pc.object(), sp, extra);
            const value = address.*;
            if (sp.push(value)) |newSp| {
                return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
            } else {
                const newSp, const newContext, const newExtra = process.spillStackAndPush(value, sp, context, extra);
                return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, newContext, newExtra });
            }
        }
        test "pushStack" {
            if (true) return error.UnimplementedTest;
            try Execution.runTest(
                "pushStack",
                .{ tf.pushLocal, 1, tf.pushLocal, 4 },
                &[_]Object{
                    Object.from(42, null),
                    Object.from(17, null),
                    Object.from(2, null),
                    Object.from(3, null),
                },
                &[_]Object{
                    Object.from(3, null),
                    Object.from(17, null),
                    Object.from(42, null),
                    Object.from(17, null),
                    Object.from(2, null),
                    Object.from(3, null),
                },
            );
        }
    };
    pub const pushThisContext = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            if (extra.noContext())
                return @call(tailCall, pushContext.threadedFn, .{ pc.prev(), sp, process, context, extra });
            const value = Object.from(context, null);
            if (sp.push(value)) |newSp| {
                return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, context, extra });
            } else {
                const newSp, const newContext, const newExtra = process.spillStackAndPush(value, sp, context, extra);
                return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, newContext, newExtra });
            }
        }
    };
    pub const storeLocal = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            if (extra.noContext())
                return @call(tailCall, pushContext.threadedFn, .{ pc.prev(), sp, process, context, extra });
            const address = getAddress(pc.object(), sp, extra);
            address.* = sp.top;
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), sp, process, context, extra });
        }
    };
};
