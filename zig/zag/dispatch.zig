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
const HeapHeader = heap.HeapHeader;
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
pub const lookupAddress = Dispatch.lookupAddressForClass;
pub const dump = Dispatch.dump;
//pub const initClass = Dispatch.initClass;
pub fn init() void {
    _ = Dispatch.new();
}
pub const addMethod = Dispatch.addMethod;
const DispatchElement = if (config.indirectDispatch) PC else extern struct {
    primitive: ThreadedFn,
    nextPointer: PC,
    const baseType = Self;
    const Self = @This();
    pub inline fn init(code: *const Code) Self {
        return .{.primitive = code.prim,.nextPointer = PC.init(@ptrCast(@as([*]Code,@constCast(@ptrCast(code)))+1))};
    }
    pub inline fn initDispatchElement(f: ThreadedFn, code: *const Code) Self {
        return .{.primitive = f,.nextPointer = PC.init(code)};
    }
    inline fn equivalentInt() type {
        return u128;
    }
    pub inline fn set(self: *Self, code: *const Code) void {
        self.primitive = code.prim;
        self.nextPointer = PC.init(@ptrCast(@as([*]Code,@constCast(@ptrCast(code)))+1));
    }
    inline fn asInt(self: Self) equivalentInt() {
        return @bitCast(self);
    }
    inline fn asIntPtr(self: *Self) *equivalentInt() {
        return @alignCast(@ptrCast(self));
    }
    pub inline fn prim(self: *const Self) ThreadedFn {
        return self.primitive;
    }
    pub inline fn next(self: *const Self) PC {
        return self.nextPointer;
    }
    pub inline fn compiledMethodPtr(self: Self, comptime index: comptime_int) *const CompiledMethod {
        return @fieldParentPtr(CompiledMethod, "code", @as(*const [2]Code, @ptrCast(@as([*]const Code,  @ptrFromInt(@as(usize,@bitCast(self.nextPointer)))) - index)));
    }
    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = .{fmt,options};
        try writer.print("DispatchElement(ThreadedFn@{x},Code@{x})", .{@intFromPtr(self.primitive),@as(usize,@bitCast(self.nextPointer))});
    }
};
const Dispatch = extern struct {
    header: HeapHeader,
    hash: u64,
    free: u16,
    length: u16,
    state: DispatchState,
    fixed: [numberOfFixed]DispatchElement align(@sizeOf(DispatchElement)),
    methods: [hashedMethods]DispatchElement, // this is just the default... normally a larger array
    const Self = @This();
    const Fixed = enum {
        equal,
        hash,
        value,
        valueColon,
        cullColon,
        // insert new names here
        maxIndex,
    };
    const numberOfFixed: usize = @intFromEnum(Fixed.maxIndex);
    const hashedMethods = (if (config.indirectDispatch) 59 else 29) - numberOfFixed; // FIX was 12 else 6
    const classIndex = ClassIndex.Dispatch;
    const DispatchState = enum(u8) { clean, beingUpdated, dead };
    var internal = [_]ThreadedFn{&super} ** (bitTests.len + 6);
    var empty = Self{
        .header = HeapHeader.staticHeaderWithClassLengthHash(classIndex, @offsetOf(Self, "methods") / 8 - 1 + 1, 0), // don't count header, but do count one element of methods
        .hash = 1,
        .free = 0,
        .length = 1,
        .state = .clean,
        .fixed = undefined,
        .methods = undefined,
    };
    comptime {
        // @compileLog(@sizeOf(Self));
        std.debug.assert(@as(usize, 1) << @ctz(@as(u62, @sizeOf(Self) + 16)) == @sizeOf(Self) + 16);
    }
    const dnu = if (@import("builtin").is_test) &testDnu else &execute.controlPrimitives.forceDnu;
    const dnuThread = [_]Code{Code.prim(dnu),Code.uint(0)};
    const dnuInit = DispatchElement.init(&dnuThread[0]);
    var dispatchData: [max_classes]Self = undefined;
    var dispatches = [_]*Self{&empty}**max_classes;
    fn dump(index: ClassIndex) void {
        trace("\ndump: {} {}",.{index,dispatches[@intFromEnum(index)]});
    }
    fn initClass(index: ClassIndex) void {
        dispatches[@intFromEnum(index)].init();
    }
    pub fn addMethod(index: ClassIndex, method: *CompiledMethod) !void {
        if (internalNeedsInitialization) initialize();
        trace("\naddMethod: {} {} {}", .{ index, method.selector(), method.codePtr() });
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
        trace("\nDispatch.initialize:",.{});
        empty.methods[0] = dnuInit;
//        empty.header.addFooter();
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
    inline fn init(self: *Self) align(@sizeOf(DispatchElement)) void {
        self.initOfSize(@sizeOf(Self) / @sizeOf(usize));
    }
    inline fn initOfSize(self: *Self, words: usize) align(@sizeOf(DispatchElement)) void {
        self.header = HeapHeader.staticHeaderWithClassLengthHash(classIndex, words - 1, 0);
        const nMethods: u16 = (words * @sizeOf(usize) - @offsetOf(Self, "methods")) / @sizeOf(DispatchElement);
        const hash = smallestPrimeAtLeast(nMethods * 6 / 10);
        self.hash = hash;
        for (self.fixed[0..]) |*ptr|
            ptr.* = dnuInit;
        for (self.methods[0..nMethods]) |*ptr|
            ptr.* = dnuInit;
        self.free = hash;
        self.length = nMethods;
        self.state = .clean;
    }
    fn isExternalCompiledMethod(self: *Self, cmp: ThreadedFn) bool {
        const ptr = @intFromPtr(cmp);
        if (ptr >= @intFromPtr(self) and ptr <= @intFromPtr(self) + self.header.length * @sizeOf(Object)) return false;
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
    inline fn lookupAddress(self: *align(@sizeOf(DispatchElement)) const Self, selector: Object) align(@sizeOf(DispatchElement)) *DispatchElement {
        const hash = doHash(selector,self.hash);
        trace("\nlookupAddress: {} {} {*} {*}",.{selector,hash,&self.methods[0],&self.methods[hash]});
        return @constCast(&self.methods[hash]);
    }
    inline fn doHash(selector: Object, size: u64) u64 {
        return @as(u64,@intCast(selector.hash32())) * size >> 32;
    }
    pub inline fn lookupAddressForClass(selector: Object, index: ClassIndex) *DispatchElement {
        trace(" (lookupAddressForClass) {}", .{index});
        const code = dispatches[@intFromEnum(index)].lookupAddress(selector);
        return code;
    }
    fn disambiguate2(location: []DispatchElement, one: *const CompiledMethod, another: *const CompiledMethod) DispatchElement {
        const oneHash = one.selector().hash32();
        const anotherHash = another.selector().hash32();
        const shift = @ctz(oneHash ^ anotherHash);
        if (shift == 32) unreachable;
        const bit = @as(u64, 1) << shift;
        trace("\ndisambiguate2: bit={}",.{bit});
        const offs = if (config.indirectDispatch) 1 else 0;
        if (oneHash & bit == 0) {
            location[offs] = DispatchElement.init(&one.code[0]);
            location[offs+1] = DispatchElement.init(&another.code[0]);
        } else {
            location[offs] = DispatchElement.init(&another.code[0]);
            location[offs+1] = DispatchElement.init(&one.code[0]);
        }
        return DispatchElement.initDispatchElement(bitTests[shift],@ptrCast(location.ptr));
    }
    fn add(self: *Self, cmp: *CompiledMethod) !void {
        trace("\nadd: {}", .{ cmp.selector()});
        while (true) {
            trace("\nadd: {*}",.{self});
            if (@cmpxchgWeak(DispatchState, &self.state, .clean, .beingUpdated, .SeqCst, .SeqCst)) |notClean| {
                if (notClean == .dead) return error.DeadDispatch;
            } else break;
        }
        trace("\nadd: after loop", .{});
        defer {
            self.state = .clean;
        }
        const address = self.lookupAddress(cmp.selector());
        trace("\nadd: {} {} {}", .{ cmp.selector(), address, address.* });
        const newElement = DispatchElement.init(cmp.codePtr()).asInt();
        if (@cmpxchgWeak(DispatchElement.equivalentInt(), address.asIntPtr(), dnuInit.asInt(), newElement, .SeqCst, .SeqCst) == null) {
            //trace("\nexchange: {*} {*} {*} {}", .{ address.*, dnuInit, cmp.codePtr(), cmp.codePtr()[0] });
            return; // we replaced DNU with method
        }
        const existing = address.compiledMethodPtr(0);
        if (existing.selector().equals(cmp.selector())) {
            address.set(cmp.codePtr());
            return;
        } else trace("\nexisting: {}", .{existing.selector()});
        if (self.isExternalCompiledMethod(@constCast(existing).codePtr().prim)) { // an actual cmp - not internal
            trace("\nfree:{} hash:{} len:{} length:{}",.{self.free,self.hash,self.header.length,self.length});
            if (self.free < self.length) {
                self.free += 3;
                const disambiguator = disambiguate2(@ptrCast(@as([*]DispatchElement, @ptrCast(&self.methods))[self.free - 3 .. self.free]), existing, cmp);
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
    const deBits = @ctz(@as(u32, @sizeOf(DispatchElement)));
    inline fn offset(hash: u32, pc: PC, comptime bit: comptime_int) *const DispatchElement {
        const offs = (if (bit <= deBits) hash << (deBits - bit) else hash >> (bit - deBits)) & @sizeOf(DispatchElement);
        const result = @as(* const DispatchElement,@ptrFromInt(@as(usize,@bitCast(pc.offsetBytes(offs)))));
        trace("\noffset: offs={} result={}",.{offs,result});
        return result;
    }
    fn bitTest0(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 0);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest1(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 1);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest2(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 2);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest3(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 3);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest4(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 4);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest5(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 5);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest6(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 6);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest7(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 7);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest8(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 8);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest9(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 9);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest10(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 10);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest11(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 11);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest12(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 12);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest13(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 13);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest14(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 14);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest15(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 15);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest16(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 16);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest17(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 17);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest18(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 18);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest19(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 19);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest20(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 20);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest21(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 21);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest22(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 22);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest23(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 23);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest24(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 24);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest25(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 25);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest26(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 26);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest27(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 27);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest28(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 28);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest29(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 29);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest30(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 30);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn bitTest31(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = offset(selector.hash32(), programCounter, 31);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    const primes = [_]?ThreadedFn{ null, null, null, &prime3, null, &prime5 };
    fn prime3(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = programCounter.offsetFor(doHash(selector,3));
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn prime5(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP {
        const pc = programCounter.offsetFor(doHash(selector,5));
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    fn super(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        _ = .{ programCounter, sp, process, context, selector, cache, @panic("called super function") };
    }
    fn fail(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        _ = .{ programCounter, sp, process, context, selector, cache };
        if (programCounter.uint() == 0)
            @panic("called fail function");
        @panic("fail with non-zero next");
    }
    fn testDnu(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        _ = .{ programCounter, sp, process, context, selector, cache };
        return sp.push(object.NotAnObject);
    }
    fn testIncrement(programCounter: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        _ = .{ process, context, selector, cache };
        @as(*usize, @ptrFromInt(programCounter.uint())).* += 1;
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
fn tOffset(hash: u32, ptr:*const DispatchElement, comptime bit: comptime_int) *const DispatchElement {
    return Dispatch.offset(hash,PC.init(@ptrCast(ptr)),bit);
}
fn eo(lDe: *const DispatchElement, rDe: *const DispatchElement) !void {
    try std.testing.expectEqual(@intFromPtr(lDe),@intFromPtr(rDe));
}
test "offset for bitTests" {
    const de = [_]DispatchElement{undefined} ** 5;
    try eo(tOffset(0xffffff55, &de[0], 0), &de[1]);
    try eo(tOffset(0xffffff55, &de[0], 1), &de[0]);
    try eo(tOffset(0xffffff55, &de[0], 2), &de[1]);
    try eo(tOffset(0xffffff55, &de[0], 3), &de[0]);
    try eo(tOffset(0xffffff55, &de[0], 4), &de[1]);
    try eo(tOffset(0xffffff55, &de[0], 5), &de[0]);
    try eo(tOffset(0xffffff55, &de[0], 6), &de[1]);
    try eo(tOffset(0xffffff55, &de[0], 7), &de[0]);
    try eo(tOffset(0xffffff55, &de[0], 8), &de[1]);
}
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
    if (config.indirectDispatch) {
        var space3 = [_]DispatchElement{undefined}**3;
        var dispatcher = Dispatch.disambiguate2(&space3, @ptrCast(&method1), @ptrCast(&method2));
        const push1Code = DispatchElement.init(&method1.code[0]);
        const push2Code = DispatchElement.init(&method2.code[0]);
        try ee(space3[1], push1Code);
        try ee(space3[2], push2Code);
        dispatcher = Dispatch.disambiguate2(&space3, @ptrCast(&method2), @ptrCast(&method1));
        try ee(space3[1], push1Code);
        try ee(space3[2], push2Code);
        var process = Process.new();
        process.init();
        defer process.deinit();
        var context = Context.init();
        const sp = process.endOfStack();
        var cache = execute.SendCacheStruct.init();
        if (config.dispatchCache) {
            try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.value, cache.dontCache()).top.to(i64), 1);
            try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.yourself, cache.dontCache()).top.to(i64), 2);
            try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.@"<=", cache.dontCache()).top.to(i64), 3);
            try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.value, cache.dontCache()).top.to(i64), 1);
        }
        try ee(dispatcher.prim(), &Dispatch.bitTest8);
        dispatcher = Dispatch.disambiguate2(&space3, @ptrCast(&method3), @ptrCast(&method1));
        try ee(dispatcher.prim(), &Dispatch.bitTest0);
    } else {
        var space2 = [_]DispatchElement{undefined}**2;
        var dispatcher = Dispatch.disambiguate2(&space2, @ptrCast(&method1), @ptrCast(&method2));
        const push1Code = DispatchElement.init(&method1.code[0]);
        const push2Code = DispatchElement.init(&method2.code[0]);
        try ee(space2[0], push1Code);
        try ee(space2[1], push2Code);
        dispatcher = Dispatch.disambiguate2(&space2, @ptrCast(&method2), @ptrCast(&method1));
        try ee(space2[0], push1Code);
        try ee(space2[1], push2Code);
        var process = Process.new();
        process.init();
        defer process.deinit();
        var context = Context.init();
        const sp = process.endOfStack();
        var cache = execute.SendCacheStruct.init();
        if (config.dispatchCache) {
            try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.value, cache.dontCache()).top.to(i64), 1);
            try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.yourself, cache.dontCache()).top.to(i64), 2);
            try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.@"<=", cache.dontCache()).top.to(i64), 3);
            try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.value, cache.dontCache()).top.to(i64), 1);
        }
        try ee(dispatcher.prim(), &Dispatch.bitTest2);
        dispatcher = Dispatch.disambiguate2(&space2, @ptrCast(&method3), @ptrCast(&method1));
        try ee(dispatcher.prim(), &Dispatch.bitTest4);
    }
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
    try ee(empty.lookupAddress(symbols.value).*, Dispatch.dnuInit);
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
