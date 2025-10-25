const std = @import("std");
const zag = @import("zag.zig");
const config = zag.config;
const tailCall = config.tailCall;
const trace = config.trace;
const checkEqual = zag.utilities.checkEqual;
const Process = zag.Process;
const object = zag.object;
const Object = object.Object;
const o0 = object.testObjects[0];
const o1 = object.testObjects[1];
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
const Code = execute.Code;
const PC = execute.PC;
const SP = Process.SP;
const Result = execute.Result;
const Execution = execute.Execution;
const CompiledMethod = execute.CompiledMethod;
const Sym = zag.symbol.symbols;
pub var nullContext = Context.init();
const Self = @This();
const Context = Self;
header: HeapHeader,
trapContextNumber: u64,
method: *const CompiledMethod,
tpc: PC, // threaded PC
npc: *const fn (PC, SP, *Process, *Context, Extra) Result, // native PC - in Continuation Passing Style
prevCtxt: ?*Context,
contextData: *ContextData,
const contextSize = @sizeOf(Self) / @sizeOf(Object);
pub const Static = ContextOnStack;
const ContextOnStack = struct {
    spOffset: u64,
    trapContextNumber: u64,
    method: *const CompiledMethod,
    tpc: usize,
    npc: *const fn (PC, SP, *Process, *Context, Extra) Result,
    prevCtxt: ?*Context,
    contextData: *ContextData,
    selfOffset: u64,
    locals: Object,
    inline fn set(self: *ContextOnStack, method: *const CompiledMethod, context: *Context, selfOffset: u64, numLocals: u64) void {
        self.spOffset = 0;
        self.trapContextNumber = 0;
        self.method = method;
        self.tpc = undefined;
        self.npc = undefined;
        self.prevCtxt = context;
        self.contextData = undefined;
        self.selfOffset = selfOffset - 1;
        const locals: [*]Object = @ptrCast(&self.locals);
        for (locals[0..numLocals]) |*l| {
            l.* = Nil();
        }
    }
    pub fn initStatic(self: *ContextOnStack) void {
        self.spOffset = @bitCast(HeapHeader.headerStatic(.Context, contextSize, contextSize - 1));
        self.trapContextNumber = 0;
        self.method = undefined;
        self.tpc = undefined;
        self.npc = undefined;
        self.prevCtxt = null;
        self.contextData = @ptrCast(&self.selfOffset);
        self.selfOffset = undefined;
        self.locals = undefined;
        self.contextData.initStatic(1);
    }
    // pub fn new() ContextOnStack {
    //     return undefined;
    // }
    pub inline fn selfAddress(self: *const ContextOnStack) ?[*]Object {
        const locals: [*]Object = @ptrCast(@constCast(&self.locals));
        return @ptrCast(&locals[self.selfOffset]);
    }
    inline fn contextDataPtr(self: *ContextOnStack) *ContextData {
        return @ptrCast(&self.selfOffset);
    }
    inline fn endOfStack(self: *const ContextOnStack) SP {
        return @ptrFromInt(@intFromPtr(self) - self.spOffset);
    }
    inline fn alloc(self: *ContextOnStack, sp: SP, size: usize) struct { [*]Object, SP } {
        _ = .{ self, sp, size, unreachable };
    }
    fn reify(self: *ContextOnStack) void {
        _ = .{ self, unreachable };
    }
};
const sizeOnStack = @sizeOf(ContextOnStack) / @sizeOf(Object) - 1;
fn ifOnStack(context: *const Context, sp: SP) ?*const ContextOnStack {
    if (sp.contains(context)) {
        return @ptrCast(context);
    }
    return null;
}
pub const Extra = packed struct {
    addr: u48,
    stack_offset: u16 = 0,
    const stack_mask = Process.stack_mask;
    const stack_size_type = zag.utilities.largeEnoughType(stack_mask);
    const is_encoded: u16 = 0x8000;
    pub const none: Extra = .{ .addr = 0, .stack_offset = 0 };
    // Three states:
    //  - method is not encoded - is_encoded will not be set and low bits not zero
    //  - method is encoded - is_encoded will be set and low bits not zero
    //  - contextData - low bits zero
    pub fn forMethod(method: *const CompiledMethod, sp: SP) Extra {
        // guaranteed that the low bits of sp are not zero by design in Process
        const stackOffset: stack_size_type = @truncate(@intFromPtr(sp));
        return .{ .addr = @truncate(@intFromPtr(method)), .stack_offset = stackOffset };
    }
    pub fn fromContextData(contextData: *const ContextData) Extra {
        _ = @as(*const ContextData, @ptrFromInt(@intFromPtr(contextData)));
        return .{ .addr = @truncate(@intFromPtr(contextData)) };
    }
    pub fn contextDataPtr(self: Extra) *ContextData {
        return @ptrFromInt(self.addr);
    }
    pub fn noContext(self: Extra) bool {
        return self.stack_offset != 0;
    }
    pub fn getMethod(self: Extra) ?*const CompiledMethod {
        if (self.noContext()) {
            return @ptrFromInt(self.addr);
        }
        return null;
    }
    pub inline fn selfAddress(self: Extra, sp: SP) ?[*]Object {
        const offset = self.stack_offset;
        if (offset != 0) {
            return @ptrFromInt((@intFromPtr(sp) & ~stack_mask) | offset);
        }
        return null;
    }
    /// Install a context
    /// used by any threaded function that needs a context
    pub fn installContextIfNone(self: Extra, sp: SP, process: *Process, context: *Context) ?NewContext {
        if (self.getMethod()) |method| {
            const newSp, const ctxt = context.push(sp, process, method, self);
            return .{ .context = @ptrCast(ctxt), .extra = fromContextData(ctxt.contextDataPtr()), .sp = newSp };
        }
        return null;
    }
    const NewContext = struct {
        context: *Context,
        extra: Extra,
        sp: SP,
    };
    pub fn encoded(self: Extra) Extra {
        return .{ .addr = self.addr, .stack_offset = self.stack_offset | is_encoded };
    }
    pub fn decoded(self: Extra) Extra {
        return .{ .addr = self.addr, .stack_offset = self.stack_offset & ~is_encoded };
    }
    pub fn isEncoded(self: Extra) bool {
        return self.stack_offset & is_encoded != 0;
    }
    pub fn primitiveFailed(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        trace("primitiveFailed: {f} {f}\n", .{ extra, pc });
        return @call(tailCall, process.check(pc.prev().prim()), .{ pc, sp, process, context, extra.encoded() });
    }
    pub fn inlinePrimitiveFailed(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        trace("inlinePrimitiveFailed: {f} {f}\n", .{ extra, pc });
        _ = .{ sp, process, context, std.debug.panic("inlinePrimitiveFailed: {f} {f}\n", .{ extra, pc }) };
        //return @call(tailCall, process.check(pc.prev().prim()), .{ pc, sp, process, context, extra.encoded() });
    }
    pub fn format(
        self: Extra,
        writer: anytype,
    ) !void {
        if (self.getMethod()) |method| {
            try writer.print("Extra{{.stack_offset = {x}, .method = {*}}}", .{ self.stack_offset, method });
        } else {
            try writer.print("Extra{{.contextData = {*}}}", .{self.contextDataPtr()});
        }
    }
};
pub const ContextData = struct {
    header: HeapHeader,
    contextData: [1]Object,
    fn initStatic(self: *ContextData, comptime locals: u11) void {
        trace("ContextData.init {}", .{locals});
        self.header = HeapHeader.headerStatic(.ContextData, locals - 1, locals);
        @setRuntimeSafety(false);
        for (self.contextData[0..locals]) |*local| {
            local.* = Nil();
        }
    }
    fn objects(self: *ContextData) [*]Object {
        return @ptrCast(self);
    }
    fn localAddress(self: *ContextData, r: usize) [*]Object {
        return self.objects() + r;
    }
};
pub fn init() Self {
    var result: Self = undefined;
    const end = &execute.endMethod;
    const pc = PC.init(&end.code[0]);
    result.header = comptime HeapHeader.calc(.Context, contextSize, 0, .static, null, Object, false) catch unreachable;
    result.method = @constCast(end);
    result.npc = pc.asThreadedFn();
    result.tpc = pc.next();
    result.prevCtxt = null;
    result.trapContextNumber = 0;
    return result;
}
pub fn format(
    self: *const Context,
    writer: anytype,
) !void {
    try writer.print("Context{{", .{});
    //    try writer.print(".header: {}", .{self.header});
    try writer.print(".method={f}", .{self.method});
    try writer.print(", .npc={x}", .{@intFromPtr(self.npc)});
    try writer.print(", .tpc={f}", .{self.tpc});
    try writer.print(", .trapContextNumber={}", .{self.trapContextNumber});
    if (self.prevCtxt) |ctxt|
        try writer.print(" prev: 0x{x}", .{@intFromPtr(ctxt)});
    try writer.print("}}", .{});
}
inline fn headerOf(self: *const Context) *HeapHeader {
    return @as(HeapObjectPtr, @ptrCast(@constCast(self))).headerPtr();
}
pub inline fn popTargetContext(target: *Context, process: *Process, result: Object) struct { SP, *Context } {
    //TODO: check if result is on the stack and ?copy to heap if so?
    const newSp, const newTarget = target.pop(process);
    newSp.top = result;
    return .{ newSp, newTarget };
}
pub inline fn pop(self: *Context, _: *Process, sp: SP) struct { SP, *Context } {
    if (self.ifOnStack(sp)) |ctxt| {
        const newSp = ctxt.selfAddress();
        trace("popContext: {*}, {*}\n", .{ self, newSp });
        return .{ @ptrCast(newSp), self.previous() };
    }
    trace("popContext: not on stack, {*}, {*}\n", .{ self, sp });
    @panic("incomplete");
}
pub fn push(self: *Context, sp: SP, process: *Process, method: *const CompiledMethod, extra: Extra) struct { SP, *ContextOnStack } {
    const locals = method.stackStructure.locals;
    if (sp.reserve(locals + sizeOnStack)) |newSp| {
        const selfOffset = method.stackStructure.selfOffset;
        const selfAddress = extra.selfAddress(sp).?;
        const sizeToMove = selfOffset - locals;
        const contextAddr = selfAddress - selfOffset - (sizeOnStack - 1);
        trace("context push: selfAddress={*} contextAddr={*} newSp={*} sp={*} sizeToMove={} stackStructure={} selfOffset={}\n", .{ selfAddress, contextAddr, newSp, sp, sizeToMove, method.stackStructure, selfOffset });
        std.debug.assert(contextAddr == newSp.array() + sizeToMove);
        for (newSp.array()[0..sizeToMove], sp.array()[0..sizeToMove]) |*target, *source| {
            target.* = source.*;
        }
        const ctxt: *align(@alignOf(Self)) ContextOnStack = @ptrCast(contextAddr);
        ctxt.set(method, self, selfOffset, locals);
        return .{ newSp, ctxt };
    }
    const newSp, const newContext, const newExtra = sp.spillStack(self, extra);
    return newContext.push(newSp, process, method, newExtra);
}
pub fn moveToHeap(self: *const Context, sp: SP, process: *Process) *Context {
    _ = .{ self, sp, process, unreachable };
    //        HeapHeader.headerOnStack(.Context, stackStructure.selfOffset, sizeForHeader);
    //        process.header().trapContextNumber;
    //        ctxt.contextData = contextData;
    // const contextData: *ContextData = @ptrCast(contextDataAddr);
    // const length: u11 = @truncate((@intFromPtr(self.endOfStack() orelse process.endOfStack()) - @intFromPtr(contextDataAddr)) / @sizeOf(Object) - 1);
    // contextData.init(locals, stackStructure.selfOffset, length);
    // if (self.isIncomplete()) {
    //     self.header = heap.header(4, Format.bothAP, class.Context_C,0,Age.stack);
    //     self.size = self.prevCtxt.calculatedSize(process);
    //     self.addr = @ptrCast(*Object,&self.temps);
    //     self.prevCtxt.moveToHeap(sp, process);
    // }
}
pub inline fn contextDataPtr(self: *const Context, sp: SP) *ContextData {
    if (self.ifOnStack(sp)) |ctxt| {
        return @ptrCast(@constCast(&ctxt.selfOffset));
    }
    return self.contextData;
}
pub inline fn isOnStack(self: *const Self, sp: SP) bool {
    return sp.contains(self);
}
pub inline fn endOfStack(self: *const Context, sp: SP) ?SP {
    if (self.ifOnStack(sp)) |ctxt| {
        return ctxt.endOfStack();
    }
    return null;
}
pub fn stack(self: *const Self, sp: SP) []Object {
    return sp.slice((@intFromPtr(self.endOfStack(sp) orelse sp.endOfStack()) - @intFromPtr(sp)) / @sizeOf(Object));
}
// pub inline fn allLocals(self: *const Context, process: *const Process) []Object {
//     const size = self.tempSize(process);
//     @setRuntimeSafety(false);
//     return @constCast(self.temps[0..size]);
// }
pub inline fn getTPc(self: *const Context) PC {
    return self.tpc;
}
pub inline fn setReturnBoth(self: *Context, npc: *const fn (PC, SP, *Process, *Context, Extra) Result, tpc: PC) void {
    self.npc = npc;
    self.tpc = tpc;
}
pub inline //
fn setReturn(self: *Context, tpc: PC) void {
    self.setReturnBoth(tpc.asThreadedFn(), tpc.next());
}
pub inline fn getNPc(self: *const Context) *const fn (PC, SP, *Process, *Context, Extra) Result {
    return self.npc;
}
pub inline fn setNPc(self: *Context, npc: *const fn (PC, SP, *Process, *Context, Extra) Result) void {
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
pub inline fn previous(self: *const Context) *Context {
    return self.prevCtxt orelse @panic("0 prev");
}
pub inline fn asHeapObjectPtr(self: *const Context) HeapObjectPtr {
    return &self.header;
}
pub inline fn asObjectPtr(self: *const Context) [*]Object {
    return @ptrCast(@constCast(self));
}
pub inline fn cleanAddress(self: *const Context) u64 {
    return @intFromPtr(self);
}
inline fn fromObjectPtr(op: [*]Object) *Context {
    return @as(*Context, @ptrCast(op));
}
pub fn print(self: *const Context, process: *const Process) void {
    const pr = std.debug.print;
    pr("Context: {*} {any}\n", .{ self, self.header });
    if (self.prevCtxt) |ctxt| {
        ctxt.print(process);
    }
}
pub const Variable = packed struct {
    lowBits: u8,
    localIndex: u7,
    isLocal: bool,
    stackOffset: u8,
    objectIndices: u40,
    pub fn format(self: Variable, writer: anytype) !void {
        try writer.print("Variable {{ localIndex: {}, isLocal: {}, stackOffset: {}, objectIndices: {} }}", .{
            self.localIndex,
            self.isLocal,
            self.stackOffset,
            self.objectIndices,
        });
    }
    fn make(stackOffset: u8, localIndex: u7, options: Options, indices: []u10) Variable {
        var oi: usize = 0;
        for (indices, 0..) |index, shift| {
            oi = oi | @as(usize, index) << @truncate(10 * shift);
        }
        return Variable{
            .lowBits = Object.intTag,
            .localIndex = localIndex,
            .isLocal = options == .Local,
            .stackOffset = stackOffset,
            .objectIndices = @truncate(oi),
        };
    }
    pub inline fn getSimpleAddress(v: Variable, sp: SP, extra: Extra) [*]Object {
        return if (extra.selfAddress(sp)) |selfAddress|
                selfAddress - v.stackOffset
            else
                extra.contextDataPtr().localAddress(v.localIndex);
    }
    pub inline fn getAddress(v: Variable, sp: SP, extra: Extra) [*]Object {
        var objs: [*]Object =
            if (extra.selfAddress(sp)) |selfAddress|
                selfAddress - v.stackOffset
            else
                extra.contextDataPtr().localAddress(v.localIndex);
        var ref = v.objectIndices;
        while (ref > 0) {
            objs = objs[ref & 0x3ff].to([*]Object);
            ref = ref >> 10;
        }
        return objs;
    }
    const Options = enum { Local, Parameter };
};
pub const makeVariable = Variable.make;

pub const threadedFunctions = struct {
    const tf = zag.threadedFn.Enum;
    const expect = std.testing.expect;
    pub const pushContext = struct {
        /// any inlinePrimitive or threadedFn that needs a context can call this to create one
        /// and then re-execute the original operation; simply:
        /// return @call(tailCall, pushContext.threadedFn, .{ pc.prev(), sp, process, context, extra });
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            if (extra.installContextIfNone(sp, process, context)) |new| {
                return @call(tailCall, process.check(pc.prim()), .{ pc.next(), new.sp, process, new.context, new.extra });
            }
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp, process, context, extra });
        }
        test "pushContext" {
            var exe = Execution.initTest("pushContext", .{
                tf.pushContext,
                tf.pushLiteral,
                o0,
            });
            exe.execute(&[_]Object{Object.from(17, exe.process.getSp(), exe.process.getContext())});
            try exe.matchStack(&[_]Object{Object.from(42, exe.process.getSp(), exe.process.getContext())});
        }
        // test "init context" {
        //     //    const expectEqual = std.testing.expectEqual;
        //     //    const objs = comptime [_]Object{True,Object.from(42)};
        //     trace("init: 1\n", .{});
        //     var result = execute.Execution.initTest("init context",.{});
        //     trace("init: 2\n", .{});
        //     var c = result.ctxt;
        //     trace("init: 3\n", .{});
        //     var process = &result.process;
        //     trace("init: 4\n", .{});
        //     //c.print(process);
        //     trace("init: 5\n", .{});
        //     //    try expectEqual(result.o()[3].u(),4);
        //     //    try expectEqual(result.o()[6],True);
        //     const sp = process.endOfStack();
        //     trace("init: 6\n", .{});
        //     const newC = c.moveToHeap(sp, process);
        //     trace("init: 7\n", .{});
        //     newC.print(process);
        //     trace("init: 8\n", .{});
        // }
    };
    pub const pushThisContext = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            if (extra.noContext())
                return @call(tailCall, pushContext.threadedFn, .{ pc.prev(), sp, process, context, extra });
            const value = Object.fromAddress(context);
            if (true) unreachable;
            if (sp.push(value)) |newSp| {
                return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, context, extra });
            } else {
                const newSp, const newContext, const newExtra = process.spillStackAndPush(value, sp, context, extra);
                return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, newContext, newExtra });
            }
        }
    };
};
