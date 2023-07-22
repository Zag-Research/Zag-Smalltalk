const std = @import("std");
const config = @import("config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
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
const CodeContextPtr = execute.CodeContextPtr;
const u32_phi_inverse = @import("utilities.zig").inversePhi(u32);
const smallestPrimeAtLeast = @import("utilities.zig").smallestPrimeAtLeast;
// note that self and other could become invalid after any method call if they are heap objects, so will need to be re-loaded from context.fields if needed thereafter

pub const forTest = Dispatch.forTest;
const noArgs = ([0]Object{})[0..];
pub const lookup = Dispatch.lookup;
pub fn init() void {
    _ = Dispatch.new();
}
pub const addMethod = Dispatch.addMethod;
const Dispatch = extern struct {
    header: HeapObject,
    hash: u64,
    free: u16,
    length: u16,
    state: DispatchState,
    methods: [12][*]const Code, // this is just the default... normally a larger array
    const Self = @This();
    const classIndex = ClassIndex.Dispatch;
    const DispatchState = enum(u8) { clean, beingUpdated, dead };
    var internal = [_]ThreadedFn{&super} ** (bitTests.len + 5);
    var empty: Self = .{
        .header = HeapObject.staticHeaderWithClassLengthHash(classIndex, @offsetOf(Self, "methods") / 8 - 1 + 1, 0), // don't count header, but do count one element of methods
        .hash = 1,
        .free = 0,
        .length = 1,
        .state = .clean,
        .methods = undefined, // should make this a footer
    };
    comptime {
        // @compileLog(@sizeOf(Self));
        std.debug.assert(@as(usize,1)<<@ctz(@as(u62,@sizeOf(Self)+8))==@sizeOf(Self)+8);
    }
    const dnuThread = [_]Code{Code.prim(&dnu)};
    const dnuInit: [*]const Code = @ptrCast(&dnuThread[0]);
    var dispatchData: [max_classes]Self = undefined;
    var dispatches = [_]*Self{@constCast(&empty)} ** max_classes;
    pub inline fn lookup(selector: Object, index: ClassIndex) [*]const Code {
        const hashed = preHash(selector.hash32());
        const address = dispatches[@intFromEnum(index)].lookupAddress(hashed);
        trace("\nlookup: {} {} {} {*} {*}", .{ index, selector, hashed, address, address.* });
        return dispatches[@intFromEnum(index)].lookupAddress(preHash(selector.hash32())).*;
    }
    pub fn addMethod(index: ClassIndex, method: *CompiledMethod) !void {
        if (internalNeedsInitialization) initialize();
        trace("\naddMethod: {} {}",.{index,method.selector});
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
        internal[1] = dnu;
        internal[2] = fail;
        internal[3] = prime3;
        internal[4] = prime5;
        for (bitTests[0..], internal[5..]) |s, *i| {
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
        trace("\nwords: {} nM: {} hash: {}", .{ words, nMethods, hash });
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
    fn isAvailable(self: *Self, ptr: [*]const Code) bool {
        _ = self;
        if (ptr == dnuInit) return true;
        return false;
    }
    inline fn lookupAddress(self: *const Self, selector: u64) *[*]const Code {
        const hash = selector * self.hash >> 32;
        //trace("\nlookupAddress: {} {}",.{selector,hash});
        return @constCast(&self.methods[hash]);
    }
    inline fn preHash(selector: u32) u64 {
        return @as(u64, selector *% u32_phi_inverse);
    }
    pub fn dispatch(self: *Self, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const hash = selector.hash32();
        const hashed = preHash(hash);
        const code = self.lookupAddress(hashed).*;
        trace("\ndispatch: {} {} {} {*} {*}", .{ selector, hash, hashed, self.lookupAddress(hashed), code });
        // all the ugly casting is to make signature match
        return @call(tailCall, @as(*const fn (*Self, [*]Object, *Process, CodeContextPtr, Object) [*]Object, @ptrCast(code[0].prim)), .{ @as(*Dispatch, @ptrCast(@constCast(code + 1))), sp, process, context, selector, cache });
    }
    fn disambiguate(location: []Code, one: *const CompiledMethod, another: *const CompiledMethod) [*]Code {
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
        return location.ptr;
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
        const hashed = preHash(cmp.selector.hash32());
        const address = self.lookupAddress(hashed);
        //trace("\nadd: {} {} {*} {*}", .{ cmp.selector, hashed, address, address.* });
        if (@cmpxchgWeak([*]const Code, address, dnuInit, cmp.codePtr(), .SeqCst, .SeqCst) == null) {
            //trace("\nexchange: {*} {*} {*} {}", .{ address.*, dnuInit, cmp.codePtr(), cmp.codePtr()[0] });
            return; // we replaced DNU with method
        }
        const existing = @as(*const Code, @ptrCast(address.*)).compiledMethodPtr(0);
        if (existing.selector.equals(cmp.selector)) {
            address.* = cmp.codePtr();
            return;
        } else trace("\nexisting: {*}", .{existing});
        if (self.isExternalCompiledMethod(@constCast(existing).codePtr()[0].prim)) { // an actual cmp - not internal
            const end = self.length - @offsetOf(Self, "methods") / @sizeOf(Object) - 2;
            if (self.free < end) {
                self.free += 3;
                const disambiguator = disambiguate(@as([*]Code, @ptrCast(&self.methods))[self.free - 3 .. self.free], existing, cmp);
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
    fn bitTest0(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 0 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest1(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 1 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest2(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 2 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest3(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 3 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest4(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 4 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest5(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 5 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest6(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 6 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest7(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 7 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest8(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 8 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest9(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 9 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest10(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 10 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest11(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 11 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest12(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 12 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest13(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 13 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest14(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 14 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest15(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 15 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest16(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 16 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest17(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 17 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest18(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 18 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest19(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 19 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest20(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 20 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest21(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 21 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest22(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 22 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest23(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 23 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest24(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 24 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest25(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 25 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest26(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 26 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest27(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 27 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest28(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 28 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest29(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 29 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest30(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 30 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn bitTest31(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = (if (selector.hash32() & 1 << 31 == 0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    const primes = [_]?ThreadedFn{ null, null, null, &prime3, null, &prime5 };
    fn prime3(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = programCounter[(preHash(selector.hash32()) * 3) >> 32].codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn prime5(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        const pc = programCounter[(preHash(selector.hash32()) * 5) >> 32].codeRef;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    fn super(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        _ = .{ programCounter, sp, process, context, selector, cache, @panic("called super function")};
    }
    fn fail(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        _ = .{ programCounter, sp, process, context, selector, cache };
        if (programCounter[0].uint == 0)
            @panic("called fail function");
        @panic("fail with non-zero next");
    }
    fn dnu(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        _ = .{ programCounter, sp, process, context, selector, cache };
        if (@import("builtin").is_test) {
            const newSp = sp - 1;
            newSp[0] = object.NotAnObject;
            return newSp;
        }
        @panic("called dnu function");
    }
    fn testIncrement(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
        _ = .{ process, context, selector, cache };
        @as(*usize, @ptrFromInt(programCounter[0].uint)).* += 1;
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
    const fns = struct {
        fn push1(_: [*]const Code, sp: [*]Object, _: *Process, _: CodeContextPtr, _: Object, _: SendCache) [*]Object {
            const newSp = sp - 1;
            newSp[0] = Object.from(1);
            return newSp;
        }
        fn push2(_: [*]const Code, sp: [*]Object, _: *Process, _: CodeContextPtr, _: Object, _: SendCache) [*]Object {
            const newSp = sp - 1;
            newSp[0] = Object.from(2);
            return newSp;
        }
        fn push3(_: [*]const Code, sp: [*]Object, _: *Process, _: CodeContextPtr, _: Object, _: SendCache) [*]Object {
            const newSp = sp - 1;
            newSp[0] = Object.from(3);
            return newSp;
        }
    };
    // value=01101 yourself=00001 @"<="=11101
    const method1 = compileMethod(symbols.value, 0, 0, .{ &fns.push1, &Code.end });
    const method2 = compileMethod(symbols.yourself, 0, 0, .{ &fns.push2, &Code.end });
    const method3 = compileMethod(symbols.@"<=", 0, 0, .{ &fns.push3, &Code.end });
    var space = [_]Code{ Code.object(Nil), Code.object(Nil), Code.object(Nil) };
    var dispatcher = Dispatch.disambiguate(&space, method1.asCompiledMethodPtr(), method2.asCompiledMethodPtr());
    const push1Code: [*]Code = @ptrCast(&method1.asCompiledMethodPtr().code);
    const push2Code: [*]Code = @ptrCast(&method2.asCompiledMethodPtr().code);
    try ee(space[2].codeRef, push1Code);
    try ee(space[1].codeRef, push2Code);
    dispatcher = Dispatch.disambiguate(&space, method2.asCompiledMethodPtr(), method1.asCompiledMethodPtr());
    try ee(space[2].codeRef, push1Code);
    try ee(space[1].codeRef, push2Code);
    var process = Process.new();
    process.init();
    defer process.deinit();
    var context = Context.init();
    const sp = process.endOfStack();
    try ee(dispatcher[0].prim(dispatcher + 1, sp, &process, &context, symbols.value)[0].to(i64), 1);
    try ee(dispatcher[0].prim(dispatcher + 1, sp, &process, &context, symbols.yourself)[0].to(i64), 2);
    try ee(dispatcher[0].prim, &Dispatch.bitTest2);
    dispatcher = Dispatch.disambiguate(&space, method3.asCompiledMethodPtr(), method1.asCompiledMethodPtr());
    try ee(dispatcher[0].prim(dispatcher + 1, sp, &process, &context, symbols.@"<=")[0].to(i64), 3);
    try ee(dispatcher[0].prim(dispatcher + 1, sp, &process, &context, symbols.value)[0].to(i64), 1);
    try ee(dispatcher[0].prim, &Dispatch.bitTest4);
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
    tE.initStack(&[_]Object{Object.from(0)});
    return tE.stack(dispatch.dispatch(tE.sp, &tE.process, &tE.ctxt, selector));
}
test "add methods" {
    const ee = std.testing.expectEqual;
    var temp0: usize = 0;
    var temp: usize = 0;
    const methodType = compiledMethodType(2);
    const fns = struct {
        fn testYourself(_: [*]const Code, sp: [*]Object, _: *Process, _: CodeContextPtr, selector: Object, _: SendCache) [*]Object {
            if (!selector.equals(symbols.yourself)) @panic("hash doesn't match");
            sp[0] = Object.cast(sp[0].u() + 2);
            return sp;
        }
        fn testAt(_: [*]const Code, sp: [*]Object, _: *Process, _: CodeContextPtr, selector: Object, _: SendCache) [*]Object {
            if (!selector.equals(symbols.@"at:")) @panic("hash doesn't match");
            sp[0] = Object.cast(sp[0].u() + 4);
            return sp;
        }
    };
    var code0 = methodType.withCode(symbols.yourself, 0, 0, .{ Code.prim(&fns.testYourself), Code.uint(@intFromPtr(&temp0)) });
    var code1 = methodType.withCode(symbols.yourself, 0, 0, .{ Code.prim(&fns.testYourself), Code.uint(@intFromPtr(&temp)) });
    var code2 = methodType.withCode(symbols.@"at:", 0, 0, .{ Code.prim(&fns.testAt), Code.uint(@intFromPtr(&temp)) });
    var tE = TestExecution.new();
    tE.init();
    var dispatch = Dispatch.new();
    dispatch.init();
    try dispatch.add(code0.asCompiledMethodPtr());
    try dispatch.add(code1.asCompiledMethodPtr());
    try ee(doDispatch(&tE, &dispatch, symbols.yourself)[0], Object.from(2));
    try ee(doDispatch(&tE, &dispatch, symbols.self)[0], object.NotAnObject);
    try dispatch.add(code2.asCompiledMethodPtr());
    try ee(doDispatch(&tE, &dispatch, symbols.yourself)[0], Object.from(2));
    try ee(doDispatch(&tE, &dispatch, symbols.@"at:")[0], Object.from(4));
    try std.testing.expectEqual(dispatch.add(code2.asCompiledMethodPtr()), error.Conflict);
}
inline fn bumpSize(size: u16) u16 {
    return size * 2;
}
inline fn initialSize(size: usize) u16 {
    return @import("utilities.zig").largerPowerOf2(@max(@as(u16, @intCast(size)), 4));
}
