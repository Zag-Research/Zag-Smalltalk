const std = @import("std");
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const zag = @import("zag.zig");
const config = zag.config;
const trace = config.trace;
const tailCall = config.tailCall;
const object = zag.object;
const Object = object.Object;
const True = object.True;
const False = object.False;
const ClassIndex = object.ClassIndex;
const execute = zag.execute;
const PC = execute.PC;
const SP = Process.SP;
const Result = execute.Result;
const Signature = execute.Signature;
const Execution = execute.Execution;
const CompiledMethod = execute.CompiledMethod;
const Process = zag.Process;
const Context = zag.Context;
const Extra = Context.Extra;
const globalArena = zag.globalArena;
const symbol = zag.symbol;
const symbols = symbol.symbols;
const HeapHeader = zag.heap.HeapHeader;
const smallestPrimeAtLeast = @import("utilities.zig").smallestPrimeAtLeast;
// note that self and other could become invalid after any method call if they are heap objects, so will need to be re-loaded from context.fields if needed thereafter

pub const lookupMethodForClass = DispatchHandler.lookupMethodForClass;
pub const addMethod = DispatchHandler.addMethod;
const DispatchHandler = struct {
    const loadFactor = 70; // hashing load factor
    var dispatches = [_]*Dispatch{&Dispatch.empty} ** config.max_classes;
    inline //
    fn lookupMethodForClass(ci: ClassIndex, signature: Signature) *const CompiledMethod {
        if (dispatches[@intFromEnum(ci)].lookupMethod(signature)) |method|
            return method;
        return loadMethodForClass(ci, signature);
    }
    fn loadMethodForClass(ci: ClassIndex, signature: Signature) *const CompiledMethod {
        if (defaultForTest != void)
            return defaultForTest.loadMethodForClass(ci, signature);
        std.log.err("loadMethodForClass({} {b} {f} {})\n", .{ ci, @as(u64, @bitCast(signature)), signature, signature.fullHash() });
        @panic("Method not found");
    }
    fn stats(index: ClassIndex) Dispatch.Stats {
        return dispatches[@intFromEnum(index)].stats();
    }
    fn methodSlice(index: ClassIndex) []DispatchElement {
        return dispatches[@intFromEnum(index)].methodSlice();
    }
    fn addMethod(method: *const CompiledMethod) void {
        const index = method.signature.getClassIndex();
        trace("addMethod({b} {f} {}) {} {*}\n",
            .{ @as(u64, @bitCast(method.signature)), method.signature, method.signature.fullHash(), index, dispatches[index] });
        if (dispatches[index].addIfAllocated(method)) return;
        while (true) {
            if (dispatches[index].lock()) |dispatch| {
                defer {
                    dispatch.state = if (dispatch != &Dispatch.empty) .dead else .clean;
                }
                var numMethods: usize = 3;
                while (true) {
                    numMethods = @max(numMethods, dispatch.nMethods + 1) * 100 / loadFactor;
                    const newDispatch = alloc(numMethods);
                    if (dispatch.addMethodsTo(newDispatch, method)) {
                        dispatches[index] = newDispatch;
                        // for (newDispatch.methodsAllocatedSlice(), 0..) |*ptr,idx| {
                        //     trace("[{}]: {*}", .{idx, ptr.method});
                        // }
                        return;
                    }
                }
            }
        }
    }
    fn alloc(words: usize) *Dispatch {
        const nMethods = smallestPrimeAtLeast(words);
        const nInstVars = (nMethods * @sizeOf(DispatchElement) + @offsetOf(Dispatch, "matches")) / @sizeOf(Object) - 1;
        const aR = globalArena.aHeapAllocator().alloc(.CompiledMethod, @intCast(nInstVars), null, Object, false);
        const newDispatch: *Dispatch = @ptrCast(@alignCast(aR.allocated));
        newDispatch.initialize(nMethods);
        return newDispatch;
    }
};
const Dispatch = struct {
    header: HeapHeader,
    nMethods: u64,
    state: DispatchState,
    matches: DispatchMatch, // this is just the empty size... normally a larger array
    comptime {
        // @compileLog(@sizeOf(Self));
        // std.debug.assert(@as(usize, 1) << @ctz(@as(u62, @sizeOf(Self))) == @sizeOf(Self));
        std.debug.assert(@offsetOf(Self, "header") == 0);
        //        std.debug.assert(@offsetOf(Self, "methods") & 0xf == 0);
    }
    const Self = @This();
    const matchSize = DispatchMatch.matchSize;
    const overAllocate = matchSize - 1;
    const DispatchState = enum(u64) { clean, beingUpdated, dead };
    var empty = Self{
        // don't count header, but do count one element of methods
        .header = HeapHeader.staticHeaderWithClassStructHash(ClassIndex.Dispatch, Self, 0),
        .nMethods = 0,
        .state = .clean,
        .matches = DispatchMatch.empty,
    };
    const Stats = struct {
        total: usize,
        active: usize,
        nMethods: usize,
        percent: usize,
    };
    fn stats(self: *Self) Stats {
        var total: usize = 0;
        var active: usize = 0;
        for (self.methodsAllocatedSlice()) |de| {
            total += 1;
            if (!de.isEmpty()) active += 1;
        }
        return .{ .total = total, .active = active, .nMethods = self.nMethods, .percent = active * 100 / @max(total, 1) };
    }
    fn initialize(self: *Self, nMethods: usize) void {
        self.state = .clean;
        self.nMethods = nMethods;
        for (self.methodsAllocatedSlice()) |*ptr|
            ptr.initUpdateable();
    }
    fn allocationSize(nMethods: usize) usize { // includes the header, so may need to subtract 1
        return @divExact(@sizeOf(Self) +
            @sizeOf(DispatchElement) * (smallestPrimeAtLeast(@max(5, nMethods)) + overAllocate - 1), @sizeOf(Object)); // extra -1 is for `start` field
    }
    inline //
    fn methods(self: *const Self) [*]DispatchElement {
        return @as([*]DispatchElement, @ptrCast(@alignCast(@constCast(&self.matches))));
    }
    inline //
    fn methodSlice(self: *Self) []DispatchElement {
        return self.methods()[0..self.nMethods];
    }
    inline //
    fn methodsAllocatedSlice(self: *Self) []DispatchElement {
        return self.methods()[0 .. self.nMethods + overAllocate];
    }
    fn addMethodsTo(self: *Self, newDispatch: *Self, method: *const CompiledMethod) bool {
        for (self.methodSlice()) |de| {
            if (de.activeMethod()) |ptr|
                if (!newDispatch.add(ptr)) return false;
        }
        return newDispatch.add(method);
    }
    inline //
    fn lookupMethod(self: *const Self, signature: Signature) ?*const CompiledMethod {
        const dm = self.dispatchMatch(signature);
        return dm.match(signature);
    }
    inline //
    fn dispatchMatch(self: *const Self, signature: Signature) *DispatchMatch {
        const index = getIndex(signature, self.nMethods);
        return @ptrCast(self.methods() + index);
    }
    inline //
    fn getIndex(signature: Signature, size: u64) u64 {
        return signature.fullHash() * size >> 32;
    }
    fn lock(self: *Self) ?*Self {
        if (@cmpxchgWeak(DispatchState, &self.state, .clean, .beingUpdated, .seq_cst, .seq_cst)) |notClean| {
            if (notClean == .dead) @panic("DeadDispatch");
            return null;
        }
        return self;
    }
    fn addIfAllocated(self: *Self, cmp: *const CompiledMethod) bool {
        if (self.nMethods == 0) return false;
        return self.add(cmp);
    }
    fn add(self_: *Self, cmp: *const CompiledMethod) bool {
        const signature = cmp.signature;
        if (self_.lock()) |self| {
            defer {
                self.state = .clean;
            }
            for (&self.dispatchMatch(signature).elements) |*element| {
                if (element.match(signature)) |_| {
                    element.storeMethod(cmp); // replace this
                    return true;
                } else if (element.isEmpty()) {
                    element.storeMethod(cmp);
                    return true;
                }
            }
        }
        return false;
    }
    fn fail(programCounter: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        _ = .{ programCounter, sp, process, context, extra };
        if (programCounter.uint() == 0)
            @panic("called fail function");
        @panic("fail with non-zero next");
    }
    fn testDnu(programCounter: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        _ = .{ programCounter, sp, process, context, extra, @panic("testDnu") };
    }
    fn testGrow(programCounter: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        _ = .{ programCounter, sp, process, context, extra, @panic("testGrow") };
    }
    fn testIncrement(programCounter: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        _ = .{ process, context, extra };
        @as(*usize, @ptrFromInt(programCounter.uint())).* += 1;
        return sp;
    }
};
fn dummyCompiledMethod(signature: Signature) CompiledMethod {
    return .{
        .header = undefined,
        .stackStructure = undefined,
        .executeFn = undefined,
        .jitted = undefined,
        .code = undefined,
        .signature = signature,
    };
}
pub const nullMethod = dummyCompiledMethod(Signature.empty);
const defaultForTest = if (config.is_test) struct {
    var called: bool = false;
    const dummyMethod = dummyCompiledMethod(Signature.fromNameClass(symbols.value, ClassIndex.Object));
    fn loadMethodForClass(ci: ClassIndex, signature: Signature) *const CompiledMethod {
        called = true;
        _ = .{ ci, signature };
        return &dummyMethod;
    }
    fn reset() void {
        called = false;
    }
} else void;
test "add/lookup" {
    const selector = symbols.@"value:";
    const class = ClassIndex.Object;
    const sig = Signature.fromNameClass(selector, class);
    const emptyMethod = dummyCompiledMethod(sig);
    addMethod(&emptyMethod);
    try std.testing.expectEqual(class.lookupMethodForClass(sig), &emptyMethod);
    const altMethod = dummyCompiledMethod(Signature.fromNameClass(selector, class));
    addMethod(&altMethod);
    try std.testing.expectEqual(class.lookupMethodForClass(sig), &altMethod);
    const stats = DispatchHandler.stats(class);
    try std.testing.expectEqual(1, stats.active);
    try std.testing.expectEqual(5, stats.nMethods);
    try std.testing.expectEqual(7, stats.total);
    defaultForTest.called = false;
    try std.testing.expectEqual(class.lookupMethodForClass(Signature.fromNameClass(symbols.@"new:", class)), &defaultForTest.dummyMethod);
    try std.testing.expectEqual(true, defaultForTest.called);
}
pub const threadedFunctions = struct {
    const tf = zag.threadedFn.Enum;
    pub const returnSelf = struct {
        pub fn threadedFn(_: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            sp.traceStack("returnSelf", context, extra);
            trace("returnSelf extra=0x{x:0>16}", .{ @as(u64, @bitCast(extra)) });
            if (extra.selfAddress(sp)) |address| {
                const newSp: SP = @ptrCast(address);
                newSp.traceStack("returnSelf after", context, extra);
                return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
            }
            const newSp, const callerContext = context.pop(sp);
            newSp.traceStack("returnSelf after pop", context, extra);
            return @call(tailCall, process.branchCheck(callerContext.getNPc()), .{ callerContext.getTPc(), newSp, process, callerContext, Extra.fromContextData(callerContext.contextData) });
        }
        test "returnSelf" {
            if (true) return error.NotImplemented;
            var exe = Execution.initTest("returnSelf", .{
                tf.pushLiteral,
                91,
                tf.pushLiteral,
                17,
                tf.returnSelf,
                2,
                tf.pushLiteral,
                99,
            });
            try exe.runTest(
                &[_]Object{exe.object(42)},
                &[_]Object{exe.object(42)},
            );
        }
    };
    pub const returnTop = struct {
        pub fn threadedFn(_: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            const top = sp.top;
            sp.traceStack("returnTop", context, extra);
            if (extra.selfAddress(sp)) |address| {
                const newSp: SP = @ptrCast(address);
                newSp.top = top;
                newSp.traceStack("returnTop after", context, extra);
                return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
            }
            const newSp, const callerContext = context.pop(sp);
            newSp.top = top;
            newSp.traceStack("returnTop after pop", context, extra);
            return @call(tailCall, process.branchCheck(callerContext.npc), .{ callerContext.tpc, newSp, process, callerContext, Extra.fromContextData(callerContext.contextDataPtr(sp)) });
        }
        test "returnTopNoContext" {
            if (true) return error.NotImplemented;
            var exe = Execution.initTest("returnTopNoContext", .{
                tf.pushLiteral,
                91,
                tf.pushLiteral,
                Object.tests[0],
                tf.returnTop,
                2,
                tf.pushLiteral,
                99,
            });
            try exe.runTest(
                &[_]Object{True()},
                &[_]Object{exe.object(42)},
            );
        }
    };
    pub const returnTopNonLocal = struct {
        pub fn threadedFn(_: PC, _: SP, _: *Process, _: *Context, _: Extra) Result {
            @panic("unreachable");
        }
    };
    inline
    fn getMethod(pc: PC, signature: Signature, receiver: Object) *const CompiledMethod {
        const class = receiver.get_class();
        const requiredSignature = signature.withClass(class);
        // std.debug.print("getMethod: {} {f} {f} {f}\n", .{ class, signature, receiver, requiredSignature });
        if (signature == requiredSignature) {
            return pc.next().method();
        }
        const method = class.lookupMethodForClass(requiredSignature);
        if (signature.getClass() == .none) {
            trace("getMethod: patch {f} {any}", .{ requiredSignature, method });
            pc.patchPtr().patchMethod(requiredSignature, method);
        } else {
            //trace("getMethod: alt {f} {any}", .{ signature, method });
        }
        return method;
    }
    pub const send = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            sp.traceStack("send", context, extra);
            const signature = pc.signature();
            const numArgs = signature.numArgs;
            const selfAddr = sp.unreserve(numArgs);
            const method = getMethod(pc, signature, selfAddr.top);
            const newPc = method.codePc();
            if (extra.installContextIfNone(sp, process, context)) |new| {
                const newSp = new.sp;
                const newContext = new.context;
                newContext.setReturn(pc.next2());
                return @call(tailCall, newPc.prim(), .{ newPc.next(), newSp, process, newContext, Extra.forMethod(method, newSp.unreserve(numArgs)) });
            }
            context.setReturn(pc.next2());
            // method.dump();
            return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, Extra.forMethod(method, selfAddr) });
        }
    };
    pub const send0 = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            sp.traceStack("send0", context, extra);
            const signature = pc.signature();
            const method = getMethod(pc, signature, sp.top);
            const newPc = method.codePc();
            if (extra.installContextIfNone(sp, process, context)) |new| {
                const newSp = new.sp;
                const newContext = new.context;
                newContext.setReturn(pc.next2());
                return @call(tailCall, newPc.prim(), .{ newPc.next(), newSp, process, newContext, Extra.forMethod(method, newSp) });
            }
            context.setReturn(pc.next2());
            return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, Extra.forMethod(method, sp) });
        }
    };
    pub const tailSend = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            const signature = pc.signature();
            const method = getMethod(pc, signature, sp.at(signature.numArgs));
            const newPc = method.codePc();
            _ = extra; // have to move parameters to self position
            if (true) @panic("unreachable");
            // return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, Extra.forMethod(method) });
            // const method = tailGetMethod(pc, sp);
            // const newPc = method.codePc();
            return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, Extra.forMethod(method) });
        }
    };
};

const DispatchElementType = enum { method, signature, function };
const dispatchElementType = DispatchElementType.method;
const DispatchElement = switch (dispatchElementType) {
    .method => DispatchMethod,
    else => unreachable,
};
const DispatchMethod = struct {
    method: *const CompiledMethod,
    const Self = @This();
    const IntSelf = u64;
    comptime {
        std.debug.assert(@sizeOf(Self) == @sizeOf(IntSelf));
    }
    fn initUpdateable(self: *Self) void {
        self.* = empty;
    }
    fn new(compiledMethod: *const CompiledMethod) Self {
        return .{ .method = compiledMethod };
    }
    const emptyMethod = dummyCompiledMethod(Signature.empty);
    const empty = new(&emptyMethod);
    inline //
    fn cas(self: *Self, replacement: *const CompiledMethod) ?Self {
        const current = self.asInt();
        const replace = new(replacement).asInt();
        if (@cmpxchgWeak(IntSelf, self.asIntPtr(), current, replace, .seq_cst, .seq_cst)) |notClean|
            return @bitCast(notClean);
        return null;
    }
    inline //
    fn storeMethod(self: *Self, replacement: *const CompiledMethod) void {
        self.method = replacement;
    }
    inline //
    fn match(self: *DispatchMethod, signature: Signature) ?*const CompiledMethod {
        const method = self.method;
        if (method.signature.equals(signature))
            return method;
        trace("match {*} {f} {f}", .{ self, method.signature, signature });
        return null;
    }
    inline //
    fn activeMethod(self: *const Self) ?*const CompiledMethod {
        if (self.isEmpty())
            return null;
        return self.method;
    }
    inline //
    fn isEmpty(self: *const Self) bool {
        return self.method == &emptyMethod;
    }
    inline //
    fn asInt(self: Self) IntSelf {
        return @bitCast(self);
    }
    inline //
    fn asIntPtr(self: *Self) *IntSelf {
        return @ptrCast(@alignCast(self));
    }
};
const DispatchMatch = struct {
    elements: [matchSize]DispatchElement,
    const matchSize = 3;
    const empty = DispatchMatch{ .elements = [_]DispatchElement{DispatchElement.empty} ** matchSize };
    inline //
    fn match(self: *DispatchMatch, signature: Signature) ?*const CompiledMethod {
        inline for (&self.elements) |*element| {
            if (element.match(signature)) |method| {
                return method;
            }
        }
        return null;
    }
    inline //
    fn matchOrEmpty(self: *DispatchMatch, signature: Signature) ?*DispatchMethod {
        inline for (&self.elements) |*element| {
            if (element.isEmpty())
                return element;
            if (element.match(signature)) |_|
                return element;
        }
        return null;
    }
};
