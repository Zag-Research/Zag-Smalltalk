const std = @import("std");
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const zag = @import("zag.zig");
const config = zag.config;
const trace = config.trace;
const tailCall = config.tailCall;
const object = zag.object;
const Object = object.Object;
const execute = zag.execute;
const PC = execute.PC;
const SP = execute.SP;
const Extra = execute.Extra;
const Execution = execute.Execution;
const Process = zag.Process;
const Context = zag.Context;
//const primitives = zag.primitives;
//const globalArena = zag.globalArena;
const symbol = zag.symbol;

const Dispatch = struct {
    header: HeapHeader,
    nMethods: u64,
    state: DispatchState,
    methodStart: DispatchElement, // this is just the empty size... normally a larger array
    comptime {
        // @compileLog(@sizeOf(Self));
        // std.debug.assert(@as(usize, 1) << @ctz(@as(u62, @sizeOf(Self))) == @sizeOf(Self));
        std.debug.assert(@offsetOf(Self, "header") == 0);
        std.debug.assert(numberOfFixed == 0);
        //        std.debug.assert(@offsetOf(Self, "methods") & 0xf == 0);
    }
    fn init(array: []Object) *Self {
        var self: *Self = @ptrCast(array.ptr);
        self.state = .clean;
        self.nMethods = @divExact(array.len * @sizeOf(Object) - @sizeOf(Self), @sizeOf(DispatchElement)) - overAllocate;
        for (self.methodSlice()) |*de|
            de.* = DispatchElement.empty;
        return self;
    }
    const Self = @This();
    const Fixed = enum {
        // value,
        // valueColon,
        // cullColon,
        // equal,
        // hash,
    };
    const fixedSelectors = [_]Object{
        symbols.value,
        symbols.@"value:",
        symbols.@"cull:",
        symbols.@"=",
        symbols.hash,
    };
    const numberOfFixed: usize = @typeInfo(Fixed).@"enum".fields.len;
    const matchSize: usize = 4;
    const overAllocate = numberOfFixed + (matchSize - 1) - 1; // extra -1 is for space used by methodStart
    const loadFactor = 70; // hashing load factor
    const DispatchState = enum(u64) { clean, beingUpdated, dead };
    var empty = Self{
        // don't count header, but do count one element of methods
        .header = HeapHeader.staticHeaderWithClassLengthHash(ClassIndex.Dispatch, @sizeOf(Self) / 8 - 1, 0),
        .nMethods = 0,
        .state = .clean,
        .methodStart = DispatchElement.empty,
    };
    fn allocationSize(nMethods: usize) usize { // includes the header, so may need to subtract 1
        return @divExact(@sizeOf(Self) +
            @sizeOf(DispatchElement) * (smallestPrimeAtLeast(@max(5, nMethods)) + overAllocate), @sizeOf(Object));
    }
    var dispatches = [_]*Self{&empty} ** max_classes;
    inline fn methods(self: *Self) [*]DispatchElement {
        return @as([*]DispatchElement, @ptrCast(@alignCast(&self.methodStart))) + numberOfFixed;
    }
    fn isUpdateable(self: *const Self, methodPtr: *const DispatchElement) bool {
        return @intFromPtr(methodPtr) < @intFromPtr(self.methods() + self.nMethods + matchSize - 1) and methodPtr.isNil();
    }
    fn methodSlice(self: *Self) []DispatchElement {
        return self.methods()[0..self.nMethods];
    }
    fn dump(index: ClassIndex) void {
        trace("\ndump: {} {}", .{ index, dispatches[@intFromEnum(index)] });
    }
    pub fn addMethod(method: *CompiledMethod) void {
        const index = method.signature.getClassIndex();
        //        trace("\naddMethod: {} {} {}", .{ index, method.selector(), method.codePtr() });
        if (dispatches[index].add(method)) return;
        var numMethods: usize = 3;
        while (true) {
            const dispatch = dispatches[index];
            if (dispatch.lock()) |_| {
                defer dispatch.state = .dead;
                while (true) {
                    numMethods = @max(numMethods, dispatch.nMethods + 1) * 100 / loadFactor;
                    const newDispatch = alloc(numMethods);
                    if (dispatch.addMethodsTo(newDispatch, method)) {
                        dispatches[index] = newDispatch;
                        return;
                    }
                }
            } else |_| {}
        }
    }
    fn addMethodsTo(self: *Self, newDispatch: *Self, method: *CompiledMethod) bool {
        for (self.methodSlice()) |de| {
            if (de.methodPointer) |ptr|
                if (!newDispatch.add(ptr)) return false;
        }
        return newDispatch.add(method);
    }
    fn alloc(words: usize) *Self {
        const hash = smallestPrimeAtLeast(words);
        const nMethods = hash;
        const nInstVars = (nMethods * @sizeOf(DispatchElement) + @offsetOf(Self, "methodStart")) / @sizeOf(Object) - 1;
        trace("\ninstVars: {}", .{nInstVars});
        const aR = globalArena.aHeapAllocator().alloc(.CompiledMethod, @intCast(nInstVars), null, Object, false);
        const self: *Self = @alignCast(@ptrCast(aR.allocated));
        self.nMethods = hash;
        for (@constCast(self.methodSlice())) |*ptr|
            ptr.initUpdateable();
        self.state = .clean;
        return self;
    }
    //inline
    fn lookupAddress(self: *const Self, signature: Signature) ?*DispatchElement {
        const index = getIndex(signature, self.nMethods);
        const des: [*]DispatchElement = self.methods() + index;
        if (DispatchElement.matchOrNil(des, matchSize, signature)) |de|
            return de;
        return null;
    }
    //inline
    fn lookupMethod(self: *Self, signature: Signature) *const CompiledMethod {
        const index = getIndex(signature, self.nMethods);
        const des: [*]DispatchElement = self.methods() + index;
        if (DispatchElement.match(des, matchSize, signature)) |method|
            return method;
        unreachable;
    }
    inline fn getIndex(selector: Signature, size: u64) u64 {
        return selector.hash() * size >> 32;
    }
    pub inline fn lookupMethodForClass(signature: Signature) *const CompiledMethod {
        trace(" (lookupMethodForClass) {}", .{signature.getClass()});
        const cM = dispatches[@intFromEnum(signature.getClass())].lookupMethod(signature);
        return cM;
    }
    fn lock(self: *Self) !void {
        while (true) {
            if (@cmpxchgWeak(DispatchState, &self.state, .clean, .beingUpdated, .seq_cst, .seq_cst)) |notClean| {
                if (notClean == .dead) return error.DeadDispatch;
            } else break;
            trace("\nlock: looping", .{});
        }
    }
    fn add(self: *Self, cmp: *const CompiledMethod) bool {
        trace("\nadd: {}", .{cmp.signature});
        self.lock() catch {
            return false;
        };
        defer {
            self.state = .clean;
        }
        var address = self.lookupAddress(cmp.signature);
        while (address.methodPointer) |existing| : (address = address.next()) {
            if (existing.signature.equals(cmp.signature)) {
                address.store(cmp); // replace this
                trace(" - replaced existing", .{});
                return true;
            }
        } else {
            if (self.isUpdateable(address)) {
                address.store(cmp);
                trace(" - installed", .{});
                return true;
            }
        }
        trace(" - no free space", .{});
        return false;
    }
    fn fail(programCounter: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
        _ = .{ programCounter, sp, process, context, signature };
        if (programCounter.uint() == 0)
            @panic("called fail function");
        @panic("fail with non-zero next");
    }
    fn testDnu(programCounter: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
        _ = .{ programCounter, sp, process, context, signature, @panic("testDnu") };
        //        return sp.push(object.NotAnObject);
    }
    fn testGrow(programCounter: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
        _ = .{ programCounter, sp, process, context, signature, @panic("testGrow") };
        //        return sp.push(object.NotAnObject);
    }
    fn testIncrement(programCounter: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
        _ = .{ process, context, signature };
        @as(*usize, @ptrFromInt(programCounter.uint())).* += 1;
        return sp;
    }
};
test "dispatch" {
    var array align(@alignOf(Dispatch)) = [_]Object{undefined} ** Dispatch.allocationSize(0);

    try struct {
        const selector = Signature.from(Sym.value, .SmallInteger);
        fn t(dispatch: *Dispatch) !void {
            try std.testing.expectEqual(null, dispatch.lookupMethod(selector));
        }
        fn testIncrement(programCounter: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
            _ = .{ process, context, signature };
            @as(*usize, @ptrFromInt(programCounter.uint())).* += 1;
            return sp;
        }
    }.t(Dispatch.init(&array));
    return error.FailedInExecute;
}
// test "disambiguate" {
//     const ee = std.testing.expectEqual;
//     Process.resetForTest();
//     const empty = Object.empty;
//     const fns = struct {
//         fn push1(_: PC, sp: SP, _: *Process, _: *Context, extra: Extra) SP {
//             return sp.push(Object.from(1));
//         }
//         fn push2(_: PC, sp: SP, _: *Process, _: *Context, extra: Extra) SP {
//             return sp.push(Object.from(2));
//         }
//         fn push3(_: PC, sp: SP, _: *Process, _: *Context, extra: Extra) SP {
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
fn doDispatch(tE: *Execution, dispatch: *Dispatch, signature: Extra) []Object {
    tE.initStack(&[_]Object{Object.from(0)});
    return tE.stack(dispatch.dispatch(tE.sp, &tE.process, &tE.ctxt, signature));
}
// test "add methods" {
//     const empty = Object.empty;
//     Process.resetForTest();
//     const ee = std.testing.expectEqual;
//     var temp0: usize = 0;
//     var temp: usize = 0;
//     const methodType = compiledMethodType(2);
//     const fns = struct {
//         fn testYourself(_: PC, sp: SP, _: *Process, _: CodeContextPtr, signature: Extra) SP {
//             if (!selector.equals(symbols.yourself)) @panic("hash doesn't match");
//             sp.top = Object.cast(sp.top.u() + 2);
//             return sp;
//         }
//         fn testAt(_: PC, sp: SP, _: *Process, _: CodeContextPtr, signature: Extra) SP {
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
//     try ee(doDispatch(&tE, &dispatch, symbols.self)[0], object.NotAnObject);
//     try dispatch.add(@ptrCast(&code2));
//     try ee(doDispatch(&tE, &dispatch, symbols.yourself)[0], Object.from(2));
//     try ee(doDispatch(&tE, &dispatch, symbols.@"at:")[0], Object.from(4));
//     try std.testing.expectEqual(dispatch.add(@ptrCast(&code2)), error.Conflict);
// }
const threadedWords = struct {
    pub const callMethod = struct {
        pub const order = 0;
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
            context.setReturn(pc.next2());
            const method = pc.method();
            const newPc = PC.init(method.codePtr());
            if (process.needsCheck()) return @call(tailCall, process.check(Process.check), .{ newPc, sp, process, context, extra });
            return @call(tailCall, process.check(method.executeFn), .{ newPc.next(), sp, process, context, Extra{ .method = method } });
        }
    };
    pub const cullColon = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
            _ = .{ pc, sp, process, context, signature, unreachable };
        }
    };
    pub const returnSelf = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
            _ = .{ pc, sp, process, context, extra, unreachable };
        }
    };
    pub const returnSelfNoContext = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
            _ = .{ pc, sp, process, context, extra, unreachable };
        }
    };
    pub const returnTop = struct {
        pub fn threadedFn(_: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
            trace("\nreturnTop: {} ", .{sp.top});
            trace("{any} ", .{context.stack(sp, process)});
            const top = sp.top;
            const result = context.pop(process);
            const newSp = result.sp;
            newSp.top = top;
            const callerContext = result.ctxt;
            trace("-> {x}", .{@intFromPtr(newSp)});
            trace("-> {any}", .{callerContext.stack(newSp, process)});
            return @call(tailCall, process.check(callerContext.getNPc()), .{ callerContext.getTPc(), newSp, process, @constCast(callerContext), extra });
        }
    };
    pub const returnTopNoContext = struct {
        pub fn threadedFn(_: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
            _ = .{ sp, process, context, extra, unreachable };
        }
    };
    pub const returnTopNonLocal = struct {
        pub fn threadedFn(_: PC, _: SP, _: *Process, _: *Context, _: Extra) SP {
            unreachable;
        }
    };
    pub const returnTopNonLocalNoContext = struct {
        pub fn threadedFn(_: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
            trace("\nreturnNoContext: {any} N={} T={}", .{ context.stack(sp, process), context.getNPc(), context.getTPc() });
            return @call(tailCall, process.check(context.getNPc()), .{ context.getTPc(), sp, process, context, extra });
        }
    };
    pub const send = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
            _ = .{ pc, sp, process, context, signature, unreachable };
        }
    };
    pub const swap = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
            const saved = sp.top;
            sp.top = sp.next;
            sp.next = saved;
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp, process, context, undefined });
        }
    };
    pub const tailCallMethod = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
            _ = .{ pc, sp, process, context, signature, unreachable };
        }
    };
    pub const tailSend = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
            _ = .{ pc, sp, process, context, signature, unreachable };
        }
    };
    pub const tailSendNoContext = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
            _ = .{ pc, sp, process, context, extra, unreachable };
        }
    };
    pub const value = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
            _ = .{ pc, sp, process, context, signature, unreachable };
        }
    };
    pub const valueColon = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
            _ = .{ pc, sp, process, context, signature, unreachable };
        }
    };
};
const oldFns = struct {
    inline fn getSignature(pc: PC, sp: SP, comptime offset: anytype) Signature {
        const selector = pc.object();
        const receiver = sp.at(if (@TypeOf(offset) == @TypeOf(null)) selector.numArgs() else offset);
        const class = receiver.get_class();
        return Signature.from(selector, class);
    }
    fn setupSend(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
        const ms = getSignature(pc, sp, null);
        const returnPc = pc.next().returnOffset();
        context.setReturn(returnPc);
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), sp, process, context, ms });
    }
    fn setupTailSend(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
        return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp, process, context, getSignature(pc, sp, null) });
    }
    pub fn setupTailSend0(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
        return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp, process, context, getSignature(pc, sp, 0) });
    }
    pub fn dynamicDispatch(_: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
        const cM = Dispatch.lookupMethodForClass(signature);
        trace("\ndynamicDispatch: {any} {}", .{ cM, signature });
        const pc = cM.codePc();
        return @call(tailCall, process.check(cM.executeFn), .{ pc, sp, process, context, signature });
    }
    pub fn fallback(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
        const self = sp.at(signature.numArgs());
        context.setReturn(pc);
        const class = self.get_class();
        const cM = Dispatch.lookupMethodForClass(signature);
        trace("\nfallback: {} {} {} {}", .{ signature, class, pc, cM });
        return @call(tailCall, process.check(cM.executeFn), .{ cM.codePc(), sp, process, context, undefined });
    }
    pub fn callRecursive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
        context.setReturn(pc.next());
        const offset = pc.int();
        const newPc = pc.next().back(@intCast(-offset));
        trace("\ncallRecursive: {any}", .{context.stack(sp, process)});
        return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), sp, process, context, undefined });
    }
    // pub fn send1(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra, prevCache: SendCache) SP {
    //     const self = sp.next;
    //     context.setReturn(pc.next().skip(sendCacheSize));
    //     const class = self.get_class();
    //     const selector = pc.object().withClass(class);
    //     trace("\nsend1: {} {}", .{ selector, class });
    //     const cache = if (dispatchCache) @as(SendCache, @constCast(@ptrCast(pc + 1))) else prevCache;
    //     const newPc = if (dispatchCache) cache.current() else lookupAddress(selector, class);
    //     trace(" {} {any}", .{ newPc, process.getStack(sp) });
    //     return @call(tailCall, process.check(newPc.*.prim()), .{ newPc.*.next(), sp, process, context, signature });
    // }
    //    pub fn tailSend1(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
    //    }
    // pub fn perform(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
    //     const context = tfAsContext(_context);
    //     const selector = sp.top;
    //     const numArgs = selector.numArgs();
    //     if (numArgs != 0) @panic("wrong number of args");
    //     const newPc = lookupAddress(selector);//, sp.next.get_class());
    //     context.setReturn(pc);
    //     return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), sp + 1, process, context, undefined });
    // }
    // pub fn performWith(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
    //     const context = tfAsContext(_context);
    //     const selector = sp.next;
    //     sp.next = sp.top;
    //     if (selector.numArgs() != 1) @panic("wrong number of args");
    //     const newPc = lookupAddress(selector);//, sp.third.get_class());
    //     context.setTPc(pc + 1);
    //     return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), sp + 1, process, context, undefined });
    // }
};
const max_classes = config.max_classes;
const symbols = symbol.symbols;
const smallestPrimeAtLeast = @import("utilities.zig").smallestPrimeAtLeast;
// // note that self and other could become invalid after any method call if they are heap objects, so will need to be re-loaded from context.fields if needed thereafter

pub const forTest = Dispatch.forTest;
const noArgs = ([0]Object{})[0..];
pub const dump = Dispatch.dump;
pub fn init() void {
    //    _ = Dispatch.new();
}
pub fn loadIntrinsicsDispatch() void {}
pub const addMethod = Dispatch.addMethod;
const DispatchElementType = enum { method, signature, function };
const dispatchElementType = DispatchElementType.method;
const DispatchElement = switch (dispatchElementType) {
    .method => execute.CompiledMethod,
    else => unreachable,
};
const DispatchSignature = struct {
    signature: Signature,
    methodPointer: *const CompiledMethod,
    const Self = @This();
    const IntSelf = u128;
    comptime {
        std.debug.assert(@sizeOf(Self) == @sizeOf(IntSelf));
    }
    fn initUpdateable(self: *Self) void {
        self.signature = Extra.nil;
        self.methodPointer = undefined;
    }
    fn new(compiledMethod: *const CompiledMethod) Self {
        return .{ .signature = compiledMethod.signature, .methodPointer = compiledMethod };
    }
    const empty: Self = .{ .signature = Signature.nil, .methodPointer = undefined };
    inline fn cas(self: *Self, replacement: *const CompiledMethod) ?Self {
        const current = self.asInt();
        const replace = new(replacement).asInt();
        if (@cmpxchgWeak(IntSelf, self.asIntPtr(), current, replace, .seq_cst, .seq_cst)) |notClean|
            return @bitCast(notClean);
        return null;
    }
    inline fn storeXXX(self: *Self, replacement: *const CompiledMethod) void { // not idempotent
        self.signature = replacement.signature;
        self.methodPointer = replacement;
    }
    inline fn storeMethod(self: *Self, replacement: *const CompiledMethod) void {
        self.methodPointer = replacement;
    }
    inline fn match(array: [*]const Self, n: usize, signature: Signature) ?*const CompiledMethod {
        inline for (array[0..n]) |self|
            if (self.signature.equals(signature)) return self.methodPointer;
        return null;
    }
    inline fn matchOrNil(array: [*]Self, n: usize, signature: Signature) ?*DispatchElement {
        inline for (array[0..n]) |self| {
            if (self.signature.equals(signature)) return self;
            if (self.isNil()) return self;
        }
        return null;
    }
    inline fn isNil(self: Self) bool {
        return self.signature.isNil();
    }
    inline fn asInt(self: Self) IntSelf {
        return @bitCast(self);
    }
    inline fn asIntPtr(self: *Self) *IntSelf {
        return @alignCast(@ptrCast(self));
    }
    inline fn pc(self: *const Self) PC {
        return PC.init(self.methodPointer.?.codePtr());
    }
    inline fn next(self: *Self) *Self {
        return @ptrCast(@as([*]Self, @ptrCast(self)) + 1);
    }
    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = .{ fmt, options };
        try writer.print("DispatchElement(ThreadedFn@{x},CompiledMethod@{x})", .{ @intFromPtr(self.primitive), @intFromPtr(self.methodPointer) });
    }
};
inline fn bumpSize(size: u16) u16 {
    return size * 2;
}
inline fn initialSize(size: usize) u16 {
    return @import("utilities.zig").largerPowerOf2(@max(@as(u16, @intCast(size)), 4));
}
