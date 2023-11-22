const std = @import("std");
const config = @import("config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const object = @import("zobject.zig");
const Object = object.Object;
const Nil = object.Nil;
//const class = @import("class.zig");
const ClassIndex = object.ClassIndex;
const max_classes = 100; //class.ReservedNumberOfClasses;
const Process = @import("process.zig").Process;
const heap = @import("heap.zig");
const HeapPtr = heap.HeapPtr;
const HeapObject = heap.HeapObject;
const builtin = @import("builtin");
const symbol = @import("symbol.zig");
const symbols = symbol.symbols;
const execute = @import("execute.zig");
const SendCache = execute.SendCache;
const Context = execute.Context;
const TestExecution = execute.TestExecution;
const ThreadedFn = execute.ThreadedFn;
const CompiledMethod = execute.CompiledMethod;
const compileMethod = execute.compileMethod;
const compiledMethodType = execute.compiledMethodType;
const Code = execute.Code;
const PC = execute.PC;
const SP = execute.SP;
const CodeContextPtr = execute.CodeContextPtr;
const smallestPrimeAtLeast = @import("utilities.zig").smallestPrimeAtLeast;
// note that self and other could become invalid after any method call if they are heap objects, so will need to be re-loaded from context.fields if needed thereafter

pub const forTest = Dispatch.forTest;
const noArgs = ([0]Object{})[0..];
pub const lookup = Dispatch.lookupForClass;
pub fn init() void {
    _ = Dispatch.new();
}
pub const addMethod = Dispatch.addMethod;
const DispatchElement = if (config.indirectDispatch) PC else extern struct {
    prim: ThreadedFn,
    nextPointer: PC,
    const Self = @This();
    pub inline fn next(self: *const Self) PC {
        return self.nextPointer;
    }
    pub inline fn set(self: *Self, pc: PC) void {
        self.prim = pc.prim;
        self.nextPointer = pc.next();
    }
};
const Dispatch = extern struct {
    header: HeapObject,
    hash: u64,
    free: u16,
    length: u16,
    state: DispatchState,
    fixed: [numberOfFixed]DispatchElement,
    methods: [hashedMethods]DispatchElement, // this is just the default... normally a larger array
    const Self = @This();
    const Fixed = enum {
        equal,
        value,
        valueColon,
        // insert new names here
        maxIndex,
    };
    const numberOfFixed: usize = @intFromEnum(Fixed.maxIndex);
    const hashedMethods = (if (config.indirectDispatch) 12 else 6) - numberOfFixed;
    const classIndex = ClassIndex.Dispatch;
    const DispatchState = enum(u8) { clean, beingUpdated, dead };
    var internal = [_]ThreadedFn{&super} ** (bitTests.len + 6);
    var empty: Self = .{
        .header = HeapObject.staticHeaderWithClassLengthHash(classIndex, @offsetOf(Self, "methods") / 8 - 1 + 1, 0), // don't count header, but do count one element of methods
        .hash = 1,
        .free = 0,
        .length = 1,
        .state = .clean,
        .fixed = undefined,
        .methods = undefined, // should make this a footer
    };
    comptime {
        // @compileLog(@sizeOf(Self));
        std.debug.assert(@as(usize, 1) << @ctz(@as(u62, @sizeOf(Self) + 8)) == @sizeOf(Self) + 8);
    }
    const dnu = if (@import("builtin").is_test) &testDnu else &execute.controlPrimitives.forceDnu;
    const dnuThread = [_]Code{Code.prim(dnu)};
    const dnuInit: PC = @ptrCast(&dnuThread[0]);
    var dispatchData: [max_classes]Self = undefined;
    var dispatches = [_]*Self{@constCast(&empty)} ** max_classes;
    pub fn addMethod(index: ClassIndex, method: *CompiledMethod) !void {
        if (internalNeedsInitialization) initialize();
        trace("\naddMethod: {} {} 0x{x} {*}", .{ index, method.selector.asSymbol(), method.selector.u(), method.codePtr() });
        //method.checkFooter();
        const idx = @intFromEnum(index);
        var dispatchP = dispatches[idx];
        if (dispatchP == &empty) {
            dispatchP = &dispatchData[idx];
            dispatches[idx] = dispatchP;
            dispatchP.init();
        }
        return try dispatchP.add(method);
    }
    var internalNeedsInitialization = true;
    fn initialize() void {
        empty.methods[0] = dnuInit;
        empty.header.addFooter();
        internal[0] = super;
        internal[1] = testDnu;
        internal[2] = fail;
        internal[3] = execute.controlPrimitives.forceDnu;
        internal[4] = prime3;
        internal[5] = prime5;
        for (bitTests[0..], internal[6..]) |s, *i| {
            i.* = s;
        }
        std.sort.insertion(ThreadedFn, &internal, {}, lessThan);
        internalNeedsInitialization = false;
    }
    fn new() Self {
        if (internalNeedsInitialization) initialize();
        return undefined;
    }
    fn lessThan(_: void, lhs: ThreadedFn, rhs: ThreadedFn) bool {
        return @intFromPtr(lhs) < @intFromPtr(rhs);
    }
    inline fn init(self: *Self) void {
        self.initOfSize(@sizeOf(Self) / 8);
    }
    inline fn initOfSize(self: *Self, words: usize) void {
        self.header = HeapObject.staticHeaderWithClassLengthHash(classIndex, words - 1, 0);
        const nMethods: u16 = words - @offsetOf(Self, "methods") / 8;
        const hash = smallestPrimeAtLeast(nMethods * 6 / 10);
        self.hash = hash;
        for (self.methods[0..nMethods]) |*ptr|
            ptr.* = dnuInit;
        self.free = hash;
        self.length = words;
        self.state = .clean;
    }
    fn isExternalCompiledMethod(self: *Self, cmp: ThreadedFn) bool {
        const ptr = @intFromPtr(cmp);
        if (ptr >= @intFromPtr(self) and ptr <= @intFromPtr(self) + self.length * @sizeOf(Object)) return false;
        var low: usize = 0;
        var high: usize = internal.len;
        while (low < high) {
            const mid = (low + high) / 2;
            const v = internal[mid];
            if (v == cmp) return false;
            if (@intFromPtr(v) < ptr) {
                low = mid + 1;
            } else {
                high = mid;
            }
        }
        return true;
    }
    fn isAvailable(self: *Self, ptr: PC) bool {
        _ = self;
        if (ptr == dnuInit) return true;
        return false;
    }
    inline fn lookupAddress(self: *const Self, selector: u64) *PC {
        const hash = selector * self.hash >> 32;
        //trace("\nlookupAddress: {} {}",.{selector,hash});
        return @constCast(&self.methods[hash]);
    }
    inline fn preHash(selector: Object) u64 {
        return selector.hash32() *% @import("utilities.zig").inversePhi(u32);
        //return (selector.u() *% @import("utilities.zig").inversePhi(u64))>>32;
    }
    pub inline fn lookupForClass(selector: Object, index: ClassIndex) PC {
        const code = dispatches[@intFromEnum(index)].lookup(selector);
        trace(" (lookupForClass) {}", .{index});
        return code;
    }
    inline fn lookup(self: *Self, selector: Object) PC {
        const hashed = preHash(selector);
        const address = self.lookupAddress(hashed);
        trace("\nlookup: {} {} {*} {*}", .{ selector.asSymbol(), hashed, address, address.* });
        return address.*;
    }
    pub fn dispatch(self: *Self, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const code = self.lookup(selector);
        trace(" (dispatch)", .{});
        // all the ugly casting is to make signature match
        return @call(tailCall, @as(*const fn (*Self, SP, *Process, CodeContextPtr, Object, SendCache) callconv(stdCall) SP, @ptrCast(code.prim)), .{ @as(*Dispatch, @ptrCast(@constCast(code.next()))), sp, process, context, selector, cache });
    }
    fn disambiguate(location: []Code, one: *const CompiledMethod, another: *const CompiledMethod) PC {
        const oneHash = one.selector.hash32();
        const anotherHash = another.selector.hash32();
        const shift = @ctz(oneHash ^ anotherHash);
        if (shift == 32) unreachable;
        const bit = @as(u64, 1) << shift;
        if (oneHash & bit == 0) {
            location[1] = Code.codeRef(&one.code);
            location[2] = Code.codeRef(&another.code);
        } else {
            location[1] = Code.codeRef(&another.code);
            location[2] = Code.codeRef(&one.code);
        }
        location[0] = Code.prim(bitTests[shift]);
        return @ptrCast(location.ptr);
    }
    fn add(self: *Self, cmp: *CompiledMethod) !void {
        while (true) {
            if (@cmpxchgWeak(DispatchState, &self.state, .clean, .beingUpdated, .SeqCst, .SeqCst)) |notClean| {
                if (notClean == .dead) return error.DeadDispatch;
            } else break;
        }
        defer {
            self.state = .clean;
        }
        const hashed = preHash(cmp.selector);
        const address = self.lookupAddress(hashed);
        trace("\nadd: {} {} {*} {*}", .{ cmp.selector, hashed, address, address.* });
        if (@cmpxchgWeak(PC, address, dnuInit, cmp.codePtr(), .SeqCst, .SeqCst) == null) {
            //trace("\nexchange: {*} {*} {*} {}", .{ address.*, dnuInit, cmp.codePtr(), cmp.codePtr()[0] });
            return; // we replaced DNU with method
        }
        const existing = @as(*const Code, @ptrCast(address.*)).compiledMethodPtr(0);
        if (existing.selector.equals(cmp.selector)) {
            address.* = cmp.codePtr();
            return;
        } else trace("\nexisting: {*}", .{existing});
        if (self.isExternalCompiledMethod(@constCast(existing).codePtr().prim)) { // an actual cmp - not internal
            const end = self.length - @offsetOf(Self, "methods") / @sizeOf(Object) - 2;
            if (self.free < end) {
                self.free += 3;
                const disambiguator = disambiguate(@ptrCast(@as([*]Code, @ptrCast(&self.methods))[self.free - 3 .. self.free]), existing, cmp);
                address.* = disambiguator;
                return;
            }
        }
        return error.Conflict;
    }
    const bitTests = [_]ThreadedFn{
        &bitTest0,  &bitTest1,  &bitTest2,  &bitTest3,  &bitTest4,  &bitTest5,
        &bitTest6,  &bitTest7,  &bitTest8,  &bitTest9,  &bitTest10, &bitTest11,
        &bitTest12, &bitTest13, &bitTest14, &bitTest15, &bitTest16, &bitTest17,
        &bitTest18, &bitTest19, &bitTest20, &bitTest21, &bitTest22, &bitTest23,
        &bitTest24, &bitTest25, &bitTest26, &bitTest27, &bitTest28, &bitTest29,
        &bitTest30, &bitTest31,
    };
    const pcBits = @ctz(@as(u32, @sizeOf(PC)));
    inline fn offset(hash: u32, pc: PC, comptime bit: comptime_int) PC {
        const offs = (if (bit <= pcBits) hash << (pcBits - bit) else hash >> (bit - pcBits)) & @sizeOf(PC);
        return @ptrFromInt(@intFromPtr(pc) + offs);
    }
    test "offset for bitTests" {
        const ee = std.testing.expectEqual;
        const pc = [_]Code{undefined} ** 5;
        try ee(offset(0xffffff55, &pc[0], 0), &pc[1]);
        try ee(offset(0xffffff55, &pc[0], 1), &pc[0]);
        try ee(offset(0xffffff55, &pc[0], 2), &pc[1]);
        try ee(offset(0xffffff55, &pc[0], 3), &pc[0]);
        try ee(offset(0xffffff55, &pc[0], 4), &pc[1]);
        try ee(offset(0xffffff55, &pc[0], 5), &pc[0]);
        try ee(offset(0xffffff55, &pc[0], 6), &pc[1]);
        try ee(offset(0xffffff55, &pc[0], 7), &pc[0]);
        try ee(offset(0xffffff55, &pc[0], 8), &pc[1]);
    }
    fn bitTest0(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 0);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest1(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 1);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest2(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 2);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest3(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 3);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest4(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 4);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest5(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 5);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest6(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 6);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest7(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 7);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest8(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 8);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest9(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 9);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest10(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 10);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest11(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 11);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest12(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 12);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest13(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 13);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest14(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 14);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest15(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 15);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest16(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 16);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest17(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 17);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest18(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 18);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest19(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 19);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest20(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 20);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest21(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 21);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest22(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 22);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest23(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 23);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest24(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 24);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest25(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 25);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest26(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 26);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest27(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 27);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest28(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 28);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest29(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 29);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest30(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 30);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn bitTest31(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 31);
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    const primes = [_]?ThreadedFn{ null, null, null, &prime3, null, &prime5 };
    fn prime3(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = programCounter.at((preHash(selector) * 3) >> 32).codeRef;
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn prime5(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const pc = programCounter.at((preHash(selector) * 5) >> 32).codeRef;
        return @call(tailCall, pc.prim, .{ pc.next(), sp, process, context, selector, cache });
    }
    fn super(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        _ = .{ programCounter, sp, process, context, selector, cache, @panic("called super function") };
    }
    fn fail(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        _ = .{ programCounter, sp, process, context, selector, cache };
        if (programCounter.uint == 0)
            @panic("called fail function");
        @panic("fail with non-zero next");
    }
    fn testDnu(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        _ = .{ programCounter, sp, process, context, selector, cache };
        return sp.push(object.NotAnObject);
    }
    fn testIncrement(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        _ = .{ process, context, selector, cache };
        @as(*usize, @ptrFromInt(programCounter.uint)).* += 1;
        return sp;
    }
};
//fn initTest(self: *Self, target: *usize) void {
//    self.initPrivate(.{Code.prim(&testIncrement),Code.uint(@intFromPtr(target))});
//}
//pub fn forTest() void {
//    var foo = Self.new();
//    foo.initDNU();
//u}
test "disambiguate" {
    const ee = std.testing.expectEqual;
    const empty = Object.empty;
    const fns = struct {
        fn push1(_: PC, sp: SP, _: *Process, _: CodeContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
            return sp.push(Object.from(1));
        }
        fn push2(_: PC, sp: SP, _: *Process, _: CodeContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
            return sp.push(Object.from(2));
        }
        fn push3(_: PC, sp: SP, _: *Process, _: CodeContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
            return sp.push(Object.from(3));
        }
    };
    // value=01101 yourself=00001 @"<="=11101
    var method1 = compileMethod(symbols.value, 0, 0, .{ &fns.push1, &Code.end });
    method1.setLiterals(empty, empty, null);
    var method2 = compileMethod(symbols.yourself, 0, 0, .{ &fns.push2, &Code.end });
    method2.setLiterals(empty, empty, null);
    var method3 = compileMethod(symbols.@"<=", 0, 0, .{ &fns.push3, &Code.end });
    method3.setLiterals(empty, empty, null);
    var space = [_]Code{ Code.object(Nil), Code.object(Nil), Code.object(Nil) };
    var dispatcher = Dispatch.disambiguate(&space, @ptrCast(&method1), @ptrCast(&method2));
    const push1Code: PC = @ptrCast(&method1.code);
    const push2Code: PC = @ptrCast(&method2.code);
    try ee(space[2].codeRef, push1Code);
    try ee(space[1].codeRef, push2Code);
    dispatcher = Dispatch.disambiguate(&space, @ptrCast(&method2), @ptrCast(&method1));
    try ee(space[2].codeRef, push1Code);
    try ee(space[1].codeRef, push2Code);
    var process = Process.new();
    process.init();
    defer process.deinit();
    var context = Context.init();
    const sp = process.endOfStack();
    var cache = execute.SendCacheStruct.init();
    try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.value, cache.dontCache()).top.to(i64), 1);
    try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.yourself, cache.dontCache()).top.to(i64), 2);
    try ee(dispatcher.prim, &Dispatch.bitTest2);
    dispatcher = Dispatch.disambiguate(&space, @ptrCast(&method3), @ptrCast(&method1));
    try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.@"<=", cache.dontCache()).top.to(i64), 3);
    try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.value, cache.dontCache()).top.to(i64), 1);
    try ee(dispatcher.prim, &Dispatch.bitTest4);
}
test "isExternalCompiledMethod" {
    const e = std.testing.expect;
    var d: Dispatch = undefined;
    try e(d.isExternalCompiledMethod(&Dispatch.bitTest0));
    d = Dispatch.new();
    try e(!d.isExternalCompiledMethod(&Dispatch.bitTest0));
    try e(!d.isExternalCompiledMethod(&Dispatch.bitTest31));
    try e(!d.isExternalCompiledMethod(&Dispatch.super));
    try e(d.isExternalCompiledMethod(&Dispatch.testIncrement));
}
test "empty dispatch" {
    const ee = std.testing.expectEqual;
    _ = Dispatch.new();
    const empty = Dispatch.empty;
    try ee(empty.lookupAddress(symbols.value.hash32()).*, Dispatch.dnuInit);
}
fn doDispatch(tE: *TestExecution, dispatch: *Dispatch, selector: Object) []Object {
    var cache = execute.SendCacheStruct.init();
    tE.initStack(&[_]Object{Object.from(0)});
    return tE.stack(dispatch.dispatch(tE.sp, &tE.process, &tE.ctxt, selector, cache.dontCache()));
}
// test "add methods" {
//     const empty = Object.empty;
//     const ee = std.testing.expectEqual;
//     var temp0: usize = 0;
//     var temp: usize = 0;
//     const methodType = compiledMethodType(2);
//     const fns = struct {
//         fn testYourself(_: PC, sp: SP, _: *Process, _: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
//             if (!selector.equals(symbols.yourself)) @panic("hash doesn't match");
//             sp.top = Object.cast(sp.top.u() + 2);
//             return sp;
//         }
//         fn testAt(_: PC, sp: SP, _: *Process, _: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
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
//     var tE = TestExecution.new();
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
inline fn bumpSize(size: u16) u16 {
    return size * 2;
}
inline fn initialSize(size: usize) u16 {
    return @import("utilities.zig").largerPowerOf2(@max(@as(u16, @intCast(size)), 4));
}
