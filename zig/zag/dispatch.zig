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
    inline fn lookupMethodForClass(ci: ClassIndex, selector: Object) *const CompiledMethod {
        trace(" (lookupMethodForClass) {} {}", .{ ci, selector });
        if (dispatches[@intFromEnum(ci)].lookupMethod(selector, ci)) |method|
            return method;
        if (defaultForTest != null)
            return defaultForTest.loadMethodForClass(ci, selector);
        return loadMethodForClass(ci, selector);
    }
    fn loadMethodForClass(ci: ClassIndex, selector: Object) *const CompiledMethod {
        std.debug.print("loadMethodForClass({} {})\n", .{ ci, selector });
        unreachable;
    }
    fn stats(index: ClassIndex) Dispatch.Stats {
        return dispatches[@intFromEnum(index)].stats();
    }
    fn methodSlice(index: ClassIndex) []DispatchElement {
        return dispatches[@intFromEnum(index)].methodSlice();
    }
    fn addMethod(method: *const CompiledMethod) void {
        method.dump();
        const index = method.signature.getClassIndex();
        //        trace("\naddMethod: {} {} {}", .{ index, method.selector(), method.codePtr() });
        if (dispatches[index].addIfAllocated(method)) return;
        while (true) {
            const dispatch = dispatches[index];
            if (dispatch.lock()) |_| {
                defer dispatch.state = .dead;
                var numMethods: usize = 3;
                while (true) {
                    numMethods = @max(numMethods, dispatch.nMethods + 1) * 100 / loadFactor;
                    const newDispatch = alloc(numMethods);
                    if (dispatch.addMethodsTo(newDispatch, method)) {
                        dispatches[index] = newDispatch;
                        // for (newDispatch.methodsAllocatedSlice(), 0..) |*ptr,idx| {
                        //     std.debug.print("[{}]: {*}\n", .{idx, ptr.method});
                        // }
                        return;
                    }
                }
            } else |_| {}
        }
    }
    fn alloc(words: usize) *Dispatch {
        const nMethods = smallestPrimeAtLeast(words);
        const nInstVars = (nMethods * @sizeOf(DispatchElement) + @offsetOf(Dispatch, "matches")) / @sizeOf(Object) - 1;
        trace("\ninstVars: {}", .{nInstVars});
        const aR = globalArena.aHeapAllocator().alloc(.CompiledMethod, @intCast(nInstVars), null, Object, false);
        const newDispatch: *Dispatch = @alignCast(@ptrCast(aR.allocated));
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
        .header = HeapHeader.staticHeaderWithClassLengthHash(ClassIndex.Dispatch, @sizeOf(Self) / 8 - 1, 0),
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
    inline fn methods(self: *const Self) [*]DispatchElement {
        return @as([*]DispatchElement, @constCast(@ptrCast(@alignCast(&self.matches))));
    }
    inline fn methodSlice(self: *Self) []DispatchElement {
        return self.methods()[0..self.nMethods];
    }
    inline fn methodsAllocatedSlice(self: *Self) []DispatchElement {
        return self.methods()[0 .. self.nMethods + overAllocate];
    }
    fn addMethodsTo(self: *Self, newDispatch: *Self, method: *const CompiledMethod) bool {
        for (self.methodSlice()) |de| {
            if (de.activeMethod()) |ptr|
                if (!newDispatch.add(ptr)) return false;
        }
        return newDispatch.add(method);
    }
    inline fn lookupAddress(self: *const Self, selector: Object, ci: ClassIndex) ?*DispatchElement {
        const dm = self.dispatchMatch(selector);
        return dm.matchOrEmpty(Signature.from(selector, ci));
    }
    //inline
    fn lookupMethod(self: *const Self, selector: Object, ci: ClassIndex) ?*const CompiledMethod {
        const dm = self.dispatchMatch(selector);
        return dm.match(Signature.from(selector, ci));
    }
    inline fn dispatchMatch(self: *const Self, selector: Object) *DispatchMatch {
        const index = getIndex(selector, self.nMethods);
        return @ptrCast(self.methods() + index);
    }
    inline fn getIndex(selector: Object, size: u64) u64 {
        return selector.symbolDirectHash() * size >> 32;
    }
    fn lock(self: *Self) !void {
        while (true) {
            if (@cmpxchgWeak(DispatchState, &self.state, .clean, .beingUpdated, .seq_cst, .seq_cst)) |notClean| {
                if (notClean == .dead) return error.DeadDispatch;
            } else break;
            trace("\nlock: looping", .{});
        }
    }
    fn addIfAllocated(self: *Self, cmp: *const CompiledMethod) bool {
        if (self.nMethods == 0) return false;
        return self.add(cmp);
    }
    fn add(self: *Self, cmp: *const CompiledMethod) bool {
        const signature = cmp.signature;
        self.lock() catch {
            return false;
        };
        defer {
            self.state = .clean;
        }
        for (&self.dispatchMatch(signature.asSymbol()).elements) |*element| {
            if (element.match(signature)) |_| {
                element.storeMethod(cmp); // replace this
                trace(" - replaced existing", .{});
                return true;
            } else if (element.isEmpty()) {
                element.storeMethod(cmp);
                trace(" - installed", .{});
                return true;
            }
        }
        trace(" - no free space", .{});
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
pub const nullMethod = dummyCompiledMethod(@bitCast(@as(u64, 1)));
const defaultForTest = if (config.is_test) struct {
    var called: bool = false;
    const dummyMethod = dummyCompiledMethod(Signature.from(symbols.value, ClassIndex.Object));
    fn loadMethodForClass(ci: ClassIndex, selector: Object) *const CompiledMethod {
        called = true;
        _ = .{ ci, selector };
        return &dummyMethod;
    }
    fn reset() void {
        called = false;
    }
} else null;
test "add/lookup" {
    const selector = symbols.@"value:";
    const class = ClassIndex.Object;
    const emptyMethod = dummyCompiledMethod(Signature.from(selector, class));
    addMethod(&emptyMethod);
    try std.testing.expectEqual(lookupMethodForClass(class, selector), &emptyMethod);
    const altMethod = dummyCompiledMethod(Signature.from(selector, class));
    addMethod(&altMethod);
    try std.testing.expectEqual(lookupMethodForClass(class, selector), &altMethod);
    const stats = DispatchHandler.stats(class);
    try std.testing.expectEqual(1, stats.active);
    try std.testing.expectEqual(5, stats.nMethods);
    try std.testing.expectEqual(7, stats.total);
    defaultForTest.called = false;
    try std.testing.expectEqual(lookupMethodForClass(class, symbols.@"new:"), &defaultForTest.dummyMethod);
    try std.testing.expectEqual(true, defaultForTest.called);
    //@"value:" @"new:" @"ifNotNil:" @"~=" @">=" all hash to 4 with a dispatch table of size 5
    //return error.TestFailed;
}
// test "disambiguate" {
//     const ee = std.testing.expectEqual;
//     Process.resetForTest();
//     const empty = Object.empty;
//     const fns = struct {
//         fn push1(_: PC, sp: SP, _: *Process, _: *Context, extra: Extra) Result {
//             return sp.push(Object.from(1));
//         }
//         fn push2(_: PC, sp: SP, _: *Process, _: *Context, extra: Extra) Result {
//             return sp.push(Object.from(2));
//         }
//         fn push3(_: PC, sp: SP, _: *Process, _: *Context, extra: Extra) Result {
//             return sp.push(Object.from(3));
//         }
//     };
//     // value=01101 yourself=00001 @"<="=11101
//     var method1 = compileMethod(symbols.value, 0, 0, .SmallInteger, .{ &fns.push1, &Code.end });
//     method1.setLiterals(empty, empty);
//     var method2 = compileMethod(symbols.yourself, 0, 0, .SmallInteger, .{ &fns.push2, &Code.end });
//     method2.setLiterals(empty, empty);
//     var method3 = compileMethod(symbols.@"<=", 0, 0, .SmallInteger, .{ &fns.push3, &Code.end });
//     method3.setLiterals(empty, empty);
//     var space2 = [_]DispatchElement{undefined}**2;
//     var dispatcher = Dispatch.disambiguate2(&space2, @ptrCast(&method1), @ptrCast(&method2));
//     const push1Code = DispatchElement.init(&method1.code[0]);
//     const push2Code = DispatchElement.init(&method2.code[0]);
//     try ee(space2[0], push1Code);
//     try ee(space2[1], push2Code);
//     dispatcher = Dispatch.disambiguate2(&space2, @ptrCast(&method2), @ptrCast(&method1));
//     try ee(space2[0], push1Code);
//     try ee(space2[1], push2Code);
//     var process = Process.new();
//     process.init();
//     defer process.deinit();
//     var context:Context = undefined;
//     context = Context.init();
//     const sp = process.endOfStack();
//     if (config.dispatchCache) {
//         _ = .{context,sp};
//         try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.value).top.to(i64), 1);
//         try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.yourself).top.to(i64), 2);
//         try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.@"<=").top.to(i64), 3);
//         try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.value).top.to(i64), 1);
//     }
//     try ee(dispatcher.prim(), &Dispatch.bitTest2);
//         dispatcher = Dispatch.disambiguate2(&space2, @ptrCast(&method3), @ptrCast(&method1));
//     try ee(dispatcher.prim(), &Dispatch.bitTest4);
// }
fn doDispatch(tE: *Execution, dispatch: *Dispatch, extra: Extra) []Object {
    tE.initStack(&[_]Object{Object.from(0)});
    return tE.stack(dispatch.dispatch(tE.sp, &tE.process, &tE.ctxt, extra));
}
// test "add methods" {
//     const empty = Object.empty;
//     Process.resetForTest();
//     const ee = std.testing.expectEqual;
//     var temp0: usize = 0;
//     var temp: usize = 0;
//     const methodType = CompiledMethod(2);
//     const fns = struct {
//         fn testYourself(_: PC, sp: SP, _: *Process, _: *Code*Context, extra: Extra) Result {
//             if (!selector.equals(symbols.yourself)) @panic("hash doesn't match");
//             sp.top = Object.cast(sp.top.u() + 2);
//             return sp;
//         }
//         fn testAt(_: PC, sp: SP, _: *Process, _: *CodeContext, extra: Extra) Result {
//             if (!selector.equals(symbols.@"at:")) @panic("hash doesn't match");
//             sp.top = Object.cast(sp.top.u() + 4);
//             return sp;
//         }
//     };
//     var code0 = methodType.withCode(symbols.yourself, 0, 0, .{ Code.prim(&fns.testYourself), Code.uint(@intFromPtr(&temp0)) });
//     code0.setLiterals(empty, empty, null);
//     var code1 = methodType.withCode(symbols.yourself, 0, 0, .{ Code.prim(&fns.testYourself), Code.uint(@intFromPtr(&temp)) });
//     code1.setLiterals(empty, empty, null);
//     var code2 = methodType.withCode(symbols.@"at:", 0, 0, .{ Code.prim(&fns.testAt), Code.uint(@intFromPtr(&temp)) });
//     code2.setLiterals(empty, empty, null);
//     var tE = Execution.new();
//     tE.init();
//     var dispatch = Dispatch.new();
//     dispatch.init();
//     try dispatch.add(@ptrCast(&code0));
//     try dispatch.add(@ptrCast(&code1));
//     try ee(doDispatch(&tE, &dispatch, symbols.yourself)[0], Object.from(2));
//     try dispatch.add(@ptrCast(&code2));
//     try ee(doDispatch(&tE, &dispatch, symbols.yourself)[0], Object.from(2));
//     try ee(doDispatch(&tE, &dispatch, symbols.@"at:")[0], Object.from(4));
//     try std.testing.expectEqual(dispatch.add(@ptrCast(&code2)), error.Conflict);
// }
pub fn inlinePrimitiveFailed(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
    std.debug.print("Inline primitive failed, sp: {}\n", .{sp});
    _ = pc.prev().prim();
    _ = .{ pc, sp, process, context, unreachable };
}
pub const threadedFunctions = struct {
    const tf = zag.threadedFn.Enum;
    pub const returnSelf = struct {
        pub fn threadedFn(_: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            std.debug.print("returnSelf: {}\n", .{extra});
            if (extra.addressIfNoContext(0, sp)) |address| {
                const newSp: SP = @ptrCast(address);
                return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextData) });
            }
            trace("\nreturnSelf: {} ", .{sp.top});
            trace("{any} ", .{context.stack(sp, process)});
            const newSp, const callerContext = context.pop(process);
            return @call(tailCall, process.check(callerContext.getNPc()), .{ callerContext.getTPc(), newSp, process, callerContext, Extra.fromContextData(callerContext.contextData) });
        }
        test "returnSelfNoContext" {
            if (true) return error.NotImplemented;
            try Execution.runTest(
                "returnSelfNoContext",
                .{
                    tf.pushLiteral,
                    91,
                    tf.pushLiteral,
                    17,
                    tf.returnSelf,
                    2,
                    tf.pushLiteral,
                    99,
                },
                &[_]Object{Object.from(42, null)},
                &[_]Object{Object.from(42, null)},
            );
        }
    };
    pub const returnTop = struct {
        pub fn threadedFn(_: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            const top = sp.top;
            std.debug.print("returnTop: {} {}\n", .{ top, extra });
            if (extra.addressIfNoContext(0, sp)) |address| {
                const newSp: SP = @ptrCast(address);
                newSp.top = top;
                return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextData) });
            }
            const newSp, const callerContext = context.pop(process);
            newSp.top = top;
            return @call(tailCall, process.check(callerContext.npc), .{ callerContext.tpc, newSp, process, context, Extra.fromContextData(callerContext.contextData) });
        }
        test "returnTopNoContext" {
            if (true) return error.NotImplemented;
            try Execution.runTest(
                "returnTopNoContext",
                .{
                    tf.pushLiteral,
                    91,
                    tf.pushLiteral,
                    42,
                    tf.returnTop,
                    2,
                    tf.pushLiteral,
                    99,
                },
                &[_]Object{True()},
                &[_]Object{Object.from(42, null)},
            );
        }
    };
    pub const returnTopNonLocal = struct {
        pub fn threadedFn(_: PC, _: SP, _: *Process, _: *Context, _: Extra) Result {
            unreachable;
        }
    };
    const SaveCase = enum { TailCall, Return };
    fn getMethod(pc: PC, sp: SP, context: *Context, saveReturn: SaveCase) *const CompiledMethod {
        const selector = pc.object();
        if (saveReturn == .Return) context.setReturn(pc.next());
        const receiver = sp.at(selector.numArgs());
        return receiver.get_class().lookupMethodForClass(selector);
    }
    pub const send = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
            std.debug.print("send: {}\n", .{pc});
            const method = getMethod(pc, sp, context, .Return);
            const newPc = method.codePc();
            return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, Extra.forMethod(method, sp) });
        }
    };
    pub const tailSend = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
            const method = getMethod(pc, sp, context, .TailCall);
            const newPc = method.codePc();
            if (true) unreachable;
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
    inline fn cas(self: *Self, replacement: *const CompiledMethod) ?Self {
        const current = self.asInt();
        const replace = new(replacement).asInt();
        if (@cmpxchgWeak(IntSelf, self.asIntPtr(), current, replace, .seq_cst, .seq_cst)) |notClean|
            return @bitCast(notClean);
        return null;
    }
    inline fn storeMethod(self: *Self, replacement: *const CompiledMethod) void {
        self.method = replacement;
    }
    //inline
    fn match(self: *DispatchMethod, signature: Signature) ?*const CompiledMethod {
        const method = self.method;
        if (method.signature == signature)
            return method;
        return null;
    }
    inline fn activeMethod(self: *const Self) ?*const CompiledMethod {
        if (self.isEmpty())
            return null;
        return self.method;
    }
    inline fn isEmpty(self: *const Self) bool {
        return self.method == &emptyMethod;
    }
    inline fn asInt(self: Self) IntSelf {
        return @bitCast(self);
    }
    inline fn asIntPtr(self: *Self) *IntSelf {
        return @alignCast(@ptrCast(self));
    }
};
const DispatchMatch = struct {
    elements: [matchSize]DispatchElement,
    const matchSize = 3;
    const empty = DispatchMatch{ .elements = [_]DispatchElement{DispatchElement.empty} ** matchSize };
    //inline
    fn match(self: *DispatchMatch, signature: Signature) ?*const CompiledMethod {
        inline for (&self.elements) |*element| {
            if (element.match(signature)) |method| {
                return method;
            }
        }
        return null;
    }
    inline fn matchOrEmpty(self: *DispatchMatch, signature: Signature) ?*DispatchMethod {
        inline for (&self.elements) |*element| {
            if (element.isEmpty())
                return element;
            if (element.match(signature)) |_|
                return element;
        }
        return null;
    }
};
