const std = @import("std");
const assert = std.debug.assert;
const zag = @import("zag.zig");
const config = zag.config;
const tailCall = config.tailCall;
const trace = config.trace;
const checkEqual = zag.utilities.checkEqual;
const structLength = zag.utilities.structLength;
const Process = zag.Process;
const object = zag.object;
const Object = object.Object;
const o0 = object.testObjects[0];
const o1 = object.testObjects[1];
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const ClassIndex = object.ClassIndex;
const heap = zag.heap;
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
prevCtxt: ?*Context,
method: *const CompiledMethod,
trapContextNumber: u64,
tpc: PC, // threaded PC
npc: *const fn (PC, SP, *Process, *Context, Extra) Result, // native PC - in Continuation Passing Style
contextData: *ContextData,
const contextSize = @sizeOf(Self) / @sizeOf(Object);
const sizeOnStack = @sizeOf(ContextOnStack) / @sizeOf(Object) - 1;
const ContextOnStack = struct {
    spAndSelfOffset: u64,
    prevCtxt: ?*Context,
    method: *const CompiledMethod,
    trapContextNumber: u64,
    tpc: usize,
    npc: *const fn (PC, SP, *Process, *Context, Extra) Result,
    contextData: *ContextData,
    contextDataHeader: HeapHeader,
    locals: Object,
    inline fn set(self: *ContextOnStack, method: *const CompiledMethod, context: *Context, selfOffset: u64, numLocals: u64) void {
        self.spAndSelfOffset = selfOffset - 1;
        self.prevCtxt = context;
        self.method = method;
        self.trapContextNumber = undefined;
        self.tpc = undefined;
        self.npc = undefined;
        self.contextData = undefined;
        self.contextDataHeader = undefined;
        const locals: [*]Object = @ptrCast(&self.locals);
        for (locals[0..numLocals]) |*l| {
            l.* = Nil();
        }
    }
    fn reify(self: *ContextOnStack, sp: SP) ?*Context {
        const dataLength = (@intFromPtr(self.prevCtxt.?.endOfStack(sp)) - @intFromPtr(&self.locals)) >> 3;
        const spAndSelfOffset = self.spAndSelfOffset;
        HeapHeader.objectOnStackWithHash(.ContextData, .directIndexed, @intCast(dataLength), @truncate(spAndSelfOffset)).at(&self.contextDataHeader);
        // and then overwrite it with the Context header
        HeapHeader.objectOnStackWithHash(.Context, .special, structLength(Context), @intCast(spAndSelfOffset >> 24)).at(self);
        self.trapContextNumber = sp.trapContextNumber();
        self.contextData = @ptrCast(&self.contextDataHeader);
        return self.prevCtxt;
    }
    pub inline fn selfAddress(self: *const ContextOnStack) [*]Object {
        const locals: [*]Object = @ptrCast(@constCast(&self.locals));
        return @ptrCast(&locals[self.spAndSelfOffset & Process.stack_mask]);
    }
    inline fn contextDataPtr(self: *ContextOnStack) *ContextData {
        return @ptrCast(&self.contextDataHeader);
    }
    fn endOfStack(self: *const ContextOnStack) SP {
        return @ptrFromInt(@intFromPtr(self) - (self.spAndSelfOffset >> Process.stack_mask_shift));
    }
    inline fn bumpEndOfStack(self: *Context, _: SP, size: usize) void {
        self.spAndSelfOffset += size << 24;
    }
    inline fn alloc(self: *ContextOnStack, sp: SP, size: usize) struct { [*]Object, SP } {
        _ = .{ self, sp, size, unreachable };
    }
    inline fn callerStack(self: *const ContextOnStack) SP {
        return @constCast(@ptrCast(&self.locals));
    }
};
pub fn reify(self: *Context, sp: SP) void {
    var context = self;
    while (context.ifOnStack(sp)) |contextOnStack| {
        if (@constCast(contextOnStack).reify(sp)) |ctxt| {
            context = ctxt;
        } else break;
    }
}
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
    const is_encoded: u16 = 0x8000;
    comptime {
        assert(stack_mask < is_encoded);
    }
    pub const none: Extra = .{ .addr = 0, .stack_offset = 0 };
    // Three states:
    //  - method is not encoded - is_encoded will not be set and stack_offset not zero
    //  - method is encoded - is_encoded will be set and stack_offset not zero
    //  - contextData - stack_offset zero
    pub fn forMethod(method: *const CompiledMethod, sp: SP) Extra {
        // guaranteed that the low bits of sp are not zero by design in Process
        return .{ .addr = @truncate(@intFromPtr(method)), .stack_offset = @truncate(@intFromPtr(sp)) };
    }
    pub fn fromContextData(contextData: *const ContextData) Extra {
        _ = @as(*const ContextData, @ptrFromInt(@intFromPtr(contextData)));
        return .{ .addr = @intCast(@intFromPtr(contextData)) };
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
            const newSp, const contextOnStack = context.push(sp, process, method, self);
            return .{ .context = @ptrCast(contextOnStack), .extra = fromContextData(contextOnStack.contextDataPtr()), .sp = newSp };
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
        trace("primitiveFailed: {f} {f}", .{ extra, pc });
        return @call(tailCall, process.check(pc.prev().prim()), .{ pc, sp, process, context, extra.encoded() });
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
    fn objects(self: *ContextData) [*]Object {
        return @ptrCast(self);
    }
    fn localAddress(self: *ContextData, r: usize) [*]Object {
        return self.objects() + r;
    }
};
var theStaticContextData = ContextData{
    .header = HeapHeader.headerStatic(.ContextData, 0, 1),
    .contextData = undefined,
};
pub fn initStatic(self: *Context) void {
    self.header = HeapHeader.headerStatic(.Context, @truncate(@intFromPtr(self)), contextSize);
    const end = &execute.endMethod;
    const pc = PC.init(&end.code[0]);
    self.method = @constCast(end);
    self.npc = pc.asThreadedFn();
    self.tpc = pc.next();
    self.prevCtxt = null;
    self.trapContextNumber = 0;
    self.contextData = &theStaticContextData;
}
pub fn format(
    self: *const Context,
    writer: anytype,
) !void {
    try writer.print("Context{{", .{});
    try writer.print(".header: {f}", .{self.header});
    try writer.print(", .method={*}", .{self.method});
    try writer.print(", .npc={x}", .{@intFromPtr(self.npc)});
    try writer.print(", .tpc={f}", .{self.tpc});
    try writer.print(", .trapContextNumber={}", .{self.trapContextNumber});
    if (self.prevCtxt) |ctxt|
        try writer.print(", prev: 0x{x}", .{@intFromPtr(ctxt)});
    try writer.print("}}", .{});
}
pub fn getCurrentMethod(self: *Context, extra: Extra) *const CompiledMethod {
    if (extra.getMethod()) |method| {
        return method;
    }
    return self.method;
}
inline fn headerOf(self: *const Context) *HeapHeader {
    return @as(*HeapObject, @ptrCast(@constCast(self))).headerPtr();
}
pub inline fn popTargetContext(target: *Context, sp: SP, result: Object) struct { SP, *Context } {
    //TODO: check if result is on the stack and ?copy to heap if so?
    const newSp, const newTarget = target.pop(sp);
    newSp.top = result;
    return .{ newSp, newTarget };
}
pub inline fn pop(self: *Context, sp: SP) struct { SP, *Context } {
    if (self.ifOnStack(sp)) |contextOnStack| {
        const newSp: SP = @ptrCast(contextOnStack.selfAddress());
        trace("popContext: {*}, {*}", .{ self, newSp });
        return .{ newSp, self.previous() };
    }
    trace("popContext: not on stack, {*}, {*}", .{ self, sp });
    @panic("incomplete");
}
pub fn push(self: *Context, sp: SP, process: *Process, method: *const CompiledMethod, extra: Extra) struct { SP, *ContextOnStack } {
    const locals = method.stackStructure.locals;
    if (sp.reserve(locals + sizeOnStack)) |newSp| {
        const selfOffset = method.stackStructure.selfOffset;
        const selfAddr = extra.selfAddress(sp).?;
        const sizeToMove = selfOffset - locals;
        const contextAddr = selfAddr - selfOffset - (sizeOnStack - 1);
        std.debug.print("pushContext: selfAddr={*}, contextAddr={*}, newSp.array = {*}, sizeToMove={}\n",
            .{ selfAddr, contextAddr, newSp.array(), sizeToMove });
        if (contextAddr != newSp.array() + sizeToMove) @panic("pushContext: contextAddr != newSp.array() + sizeToMove");
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
    if (self.ifOnStack(sp)) |contextOnStack| {
        return @ptrCast(@constCast(&contextOnStack.contextDataHeader));
    }
    return self.contextData;
}
pub inline fn isOnStack(self: *const Self, sp: SP) bool {
    return sp.contains(self);
}
pub //inline
fn endOfStack(self: *const Context, sp: SP) SP {
    if (self.ifOnStack(sp)) |contextOnStack| {
        return contextOnStack.endOfStack();
    }
    return sp.endOfStack();
}
pub inline fn bumpEndOfStack(self: *Context, sp: SP, size: usize) void {
    if (self.ifOnStack(sp)) |contextOnStack| {
        contextOnStack.bumpEndOfStack(size);
    } else {
        self.header.hash += size;
    }
}
pub fn stack(self: *const Self, sp: SP) []Object {
    return sp.slice((@intFromPtr(self.endOfStack(sp)) - @intFromPtr(sp)) / @sizeOf(Object));
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
pub inline fn callerStack(self: *const Context, sp: SP) ?SP {
    if (self.ifOnStack(sp)) |contextOnStack|
        return contextOnStack.callerStack();
    return null;
}
pub //inline
fn selfAddress(self: *const Context, sp: SP) [*]Object {
    if (self.ifOnStack(sp)) |contextOnStack|
        return contextOnStack.selfAddress();
    const wordsToDiscard = self.header.hash16();
    std.debug.print("wordsToDiscard: {} context: {*} contextData: {*}\n", .{wordsToDiscard, self, self.contextData});
    _ = .{ wordsToDiscard, @panic("not on stack") };
    //return @ptrCast(@constCast(&self.asObjectPtr()[wordsToDiscard]));
}
pub inline fn previous(self: *const Context) *Context {
    return self.prevCtxt orelse @panic("0 prev");
}
pub inline fn asHeapObjectPtr(self: *const Context) *HeapObject {
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
    const pr = std.log.err;
    pr("Context: {*} {any}\n", .{ self, self.header });
    if (self.prevCtxt) |ctxt| {
        ctxt.print(process);
    }
}
pub fn initStackClosure(self: *const Context, sp: SP, size: usize, class: ClassIndex) ?*HeapObject {
    if (!self.isOnStack(sp)) return null; // only allocate on the stack if the context is on the stack
    _ = .{ size, class, @panic("Context.initStackClosure unimplemented") };
    // return @ptrCast(context.endOfStack(newSp));
}
pub fn initHeapClosure(self: *const Context, sp: SP, size: usize, class: ClassIndex) ?*HeapObject {
    if (self.isOnStack(sp)) return null; // initStackClosure must have failed
    if (sp.reserve(1) == null) return null; // make sure there is enough space to push the result for the closure
    _ = .{ size, class, @panic("Context.initHeapClosure unimplemented") };
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
            oi = oi | @as(usize, index) << @intCast(10 * shift);
        }
        return Variable{
            .lowBits = Object.intTag,
            .localIndex = localIndex,
            .isLocal = options == .Local,
            .stackOffset = stackOffset,
            .objectIndices = @intCast(oi),
        };
    }
    pub inline fn getSimpleAddress(v: Variable, sp: SP, extra: Extra) [*]Object {
        return if (extra.selfAddress(sp)) |selfAddr|
            selfAddr - v.stackOffset
        else
            extra.contextDataPtr().localAddress(v.localIndex);
    }
    pub inline fn getAddress(v: Variable, sp: SP, extra: Extra) [*]Object {
        var objs: [*]Object =
            if (extra.selfAddress(sp)) |ptr|
                ptr - v.stackOffset
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
        //     trace("init: 1", .{});
        //     var result = execute.Execution.initTest("init context",.{});
        //     trace("init: 2", .{});
        //     var c = result.ctxt;
        //     trace("init: 3", .{});
        //     var process = &result.process;
        //     trace("init: 4", .{});
        //     //c.print(process);
        //     trace("init: 5", .{});
        //     //    try expectEqual(result.o()[3].u(),4);
        //     //    try expectEqual(result.o()[6],True);
        //     const sp = process.endOfStack();
        //     trace("init: 6", .{});
        //     const newC = c.moveToHeap(sp, process);
        //     trace("init: 7", .{});
        //     newC.print(process);
        //     trace("init: 8", .{});
        // }
    };
    pub const pushThisContext = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            if (extra.noContext())
                return @call(tailCall, pushContext.threadedFn, .{ pc.prev(), sp, process, context, extra });
            const value = Object.fromAddress(context);
            if (true) @panic("unreachable");
            if (sp.push(value)) |newSp| {
                return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, context, extra });
            } else {
                const newSp, const newContext, const newExtra = process.spillStackAndPush(value, sp, context, extra);
                return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, newContext, newExtra });
            }
        }
    };
};
