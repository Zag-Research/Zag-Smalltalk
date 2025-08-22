const std = @import("std");
const math = std.math;
const assert = std.debug.assert;
const mem = std.mem;
const builtin = @import("builtin");
const zag = @import("zag.zig");
const config = zag.config;
const tailCall = config.tailCall;
const trace = config.trace;
const SeqCst = std.builtin.AtomicOrder.seq_cst;
const object = zag.object;
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const ClassIndex = object.ClassIndex;
const checkEqual = zag.utilities.checkEqual;
//const dispatch = @import("dispatch.zig");
const heap = zag.heap;
const HeapHeader = heap.HeapHeader;
const HeapObject = heap.HeapObject;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectArray = heap.HeapObjectArray;
const footer = heap.footer;
const Age = heap.Age;
const Format = heap.Format;
const allocationInfo = heap.AllocationInfo.calc;
const AllocReturn = heap.AllocReturn;
const Context = zag.Context;
const Extra = Context.Extra;
const execute = zag.execute;
const Code = execute.Code;
const PC = execute.PC;
const Result = execute.Result;

/// this is really a Process object with the low bits encoding additional information
const Self = @This();
m: [process_total_size]u8 align(1), // alignment explicitly stated to emphasize the difference from Process
const process_total_size = config.process_total_size;
pub const alignment = @max(stack_mask_overflow, flagMask + 1);
pub const stack_mask_overflow = zag.utilities.largerPowerOf2(Process.stack_size * @sizeOf(Object));
pub const stack_mask = stack_mask_overflow - 1;
pub const process_stack_size = Process.stack_size;
pub const process_nursery_size = Process.nursery_size;
const Process = extern struct {
    stack: [stack_size]Object align(alignment),
    h: Fields,
    nursery0: [nursery_size]HeapObject,
    nursery1: [nursery_size]HeapObject,
    const Fields = extern struct {
        next: ?*Self,
        id: u64,
        context: *Context,
        trapContextNumber: u64,
        debugFn: ?*const fn (programCounter: PC, stackPointer: SP, process: *Self, context: *Context, signature: Extra) Result,
        sp: SP,
        process: Object,
        currHeap: HeapObjectArray,
        currHp: HeapObjectArray,
        currEnd: HeapObjectArray,
        otherHeap: HeapObjectArray,
    };
    const headerSize = @sizeOf(Fields);
    const processAvail = (process_total_size - headerSize) / @sizeOf(Object);
    const approx_nursery_size = (processAvail - processAvail / 9) / 2;
    const stack_size = @min(processAvail - approx_nursery_size * 2, (1 << 16) / @sizeOf(Object) - 1);
    const nursery_size = (processAvail - stack_size) / 2;
    comptime {
        assert(stack_size <= nursery_size);
    }
    const lastNurseryAge = Age.lastNurseryAge;
    const maxNurseryObjectSize = @min(HeapHeader.maxLength, nursery_size / 4);
    const maxStackObjectSize = @min(HeapHeader.maxLength, stack_size / 4);
};
pub fn format(
    orig: *const @This(),
    writer: anytype,
) !void {
    const self = orig.ptr();
    try writer.print("process: {} .stack = {any}", .{ orig.header().id, orig.getStack(self.h.sp) });
    try writer.print(" .heap = {any}", .{orig.getHeap()});
}
comptime {
    assert(process_total_size == @sizeOf(Process));
}
var allProcesses: ?*Self = null;
pub inline fn ptr(self: *align(1) const Self) *Process {
    return @ptrFromInt(@intFromPtr(self) & nonFlags);
}
pub inline fn header(self: *align(1) const Self) *Process.Fields {
    return &self.ptr().h;
}
pub fn new() Self {
    return undefined;
}
pub fn init(origin: *align(alignment) Self, process: Object) void {
    const self = origin.ptr();
    self.h.sp = origin.endOfStack();
    self.h.currHeap = HeapObject.fromObjectPtr(@ptrCast(&self.nursery0));
    self.h.currEnd = self.h.currHeap + Process.nursery_size;
    self.h.currHp = self.h.currHeap;
    self.h.otherHeap = HeapObject.fromObjectPtr(@ptrCast(&self.nursery1));
    self.h.process = process;
    while (true) {
        self.h.next = allProcesses;
        self.h.id = if (allProcesses) |p| p.header().id + 1 else 1;
        if (@cmpxchgWeak(?*Self, &allProcesses, self.h.next, origin, SeqCst, SeqCst) == null) break;
    }
    self.h.trapContextNumber = 0;
}
pub fn deinit(self: *align(1) Self) void {
    self.ptr().* = undefined;
}
const countType = u5;
const countMask: usize = math.maxInt(countType);
const countOverflowFlag = countMask + 1;
const nonCount = ~(countOverflowFlag + countMask);
const othersFlag = countOverflowFlag << 1;
const checkFlags = othersFlag | countOverflowFlag;
const flagMask = checkFlags | countMask;
const nonFlags = ~flagMask;
pub inline fn check(self: *align(1) const Self, next: *const fn (PC, SP, *Self, *Context, Extra) Result) *const fn (PC, SP, *Self, *Context, Extra) Result {
    return if (self.needsCheck()) &fullCheck else next;
}
inline fn needsCheck(self: *align(1) const Self) bool {
    return (@intFromPtr(self) & checkFlags) != 0;
}
fn fullCheck(pc: PC, sp: SP, process: *align(1) Self, context: *Context, extra: Extra) Result {
    trace("fullCheck: {f}\n", .{extra});
    return @call(tailCall, pc.prev().prim(), .{ pc, sp, process, context, extra });
}
pub inline fn checkBump(self: *Self) *Self {
    if (self.needsCheck()) return self;
    return @ptrFromInt(@intFromPtr(self) + 1);
}
pub inline fn maxCount(self: *align(1) const Self) *align(1) Self {
    return @ptrFromInt(@intFromPtr(self) | countMask);
}
pub inline fn clearCount(self: *align(1) const Self) *align(1) Self {
    return @ptrFromInt(@intFromPtr(self) & nonCount);
}
pub inline fn endOfStack(self: *align(1) const Self) SP {
    return @ptrCast(@as([*]Object, @ptrCast(&self.ptr().stack[0])) + Process.stack_size);
}
pub inline fn setSp(self: *align(1) Self, sp: SP) void {
    self.header().sp = sp;
}
pub inline fn getContext(self: *align(1) const Self) *Context {
    return self.header().context;
}
pub inline fn setContext(self: *align(1) Self, context: *Context) void {
    self.header().context = context;
}
pub inline fn getSp(self: *align(1) const Self) SP {
    return self.header().sp;
}
pub inline fn freeStack(self: *align(1) const Self, sp: SP) usize {
    return (@intFromPtr(sp) - @intFromPtr(self.ptr())) / 8;
}
pub inline//
fn getStack(self: *align(1) const Self, sp: SP) []Object {
    //    return sp.slice((@intFromPtr(self.endOfStack()) - @intFromPtr(sp)) / @sizeOf(Object));
    return sp.sliceTo(self.endOfStack());
}
pub inline fn dumpStack(self: *align(1) const Self, sp: SP, why: []const u8) void {
    trace("dumpStack ({s})\n", .{ why });
    for (self.getStack(sp)) |*obj|
        trace("[{x:0>10}]: {x:0>16}\n", .{ @intFromPtr(obj), @as(u64, @bitCast(obj.*))});
}
pub inline fn canAllocStackSpace(self: *align(1) Self, sp: SP, words: usize) bool {
    const newSp = sp.reserve(words);
    return @intFromPtr(newSp) > @intFromPtr(self);
}
pub inline fn getHeap(self: *align(1) const Self) []HeapObject {
    return self.header().currHeap[0..((@intFromPtr(self.header().currHp) - @intFromPtr(self.header().currHeap)) / @sizeOf(Object))];
}
pub inline fn freeNursery(self: *align(1) const Self) usize {
    return (@intFromPtr(self.header().currEnd) - @intFromPtr(self.header().currHp)) / 8;
}
pub fn spillStackAndPush(self: *align(1) Self, value: Object, sp: SP, context: *Context, extra: Extra) struct { SP, *Context, Extra } {
    const newSp, const newContext, const newExtra = self.spillStackAndReserve(1, sp, context, extra);
    newSp.top = value;
    return .{ newSp, newContext, newExtra };
}
pub fn spillStackAndReserve(self: *align(1) Self, reserve: usize, sp: SP, context: *Context, extra: Extra) struct { SP, *Context, Extra } {
    const newSp, const newContext, const newExtra = self.spillStack(sp, context, extra);
    return .{ newSp.safeReserve(reserve), newContext, newExtra };
}
pub fn spillStack(self: *align(1) Self, sp: SP, context: *Context, extra: Extra) struct { SP, *Context, Extra } {
    if (!context.isOnStack()) return .{ sp, context, extra };
    // if the Context is on the stack, the Context, Extra and SP will move
    _ = .{ self, @panic("unimplemented") };
}
pub fn allocArray(self: *align(1) Self, slice: []const Object, sp: SP, context: *Context) HeapObjectArray {
    const len: u11 = @truncate(slice.len);
    const hop = self.allocSpace(len, sp, context);
    hop.header.objectInNursery(.Array, len);
    const target: HeapObjectArray = @ptrCast(hop);
    @memcpy(target + 1, @as([]const HeapObject, @ptrCast(slice)));
    return target;
}
fn allocSpace(self: *align(1) Self, size: u11, sp: SP, context: *Context) HeapObjectPtr {
    const head = self.header();
    const result = head.currHp;
    const newHp = result + size + 1;
    if (@intFromPtr(newHp) <= @intFromPtr(head.currEnd)) {
        head.currHp = newHp;
        return @ptrCast(result);
    }
    _ = .{ sp, context, unreachable };
}
pub fn alloc(self: *align(1) Self, classIndex: ClassIndex, iVars: u11, indexed: ?usize, comptime element: type, makeWeak: bool) heap.AllocResult {
    const aI = allocationInfo(iVars, indexed, element, makeWeak);
    if (aI.objectSize(Process.maxNurseryObjectSize)) |size| {
        //std.debug.print("self: {x} self.header() {x} {} {x}\n", .{ @intFromPtr(self), @intFromPtr(self.header()), size, @intFromPtr(self.header().currHp) });
        const result = HeapObject.fillToBoundary(self.header().currHp);
        const newHp = result + size + 1;
        if (@intFromPtr(newHp) <= @intFromPtr(self.header().currEnd)) {
            self.header().currHp = newHp;
            const obj: heap.HeapObjectPtr = @ptrCast(result);
            aI.initObjectStructure(obj, classIndex, .nursery);
            return .{
                .age = .nursery,
                .allocated = obj,
                .info = aI,
            };
        }
    }
    @panic("NeedNurseryCollection");
}
pub fn allocStackX(self: *align(1) Self, oldSp: SP, classIndex: ClassIndex, iVars: u11, indexed: ?usize, comptime element: type) !SP {
    const aI = allocationInfo(iVars, indexed, element, false);
    if (aI.objectSize(Process.maxStackObjectSize)) |size| {
        if (self.canAllocStackSpace(oldSp, size + 2)) {
            const sp = oldSp.reserve(size + 2);
            const obj: heap.HeapObjectPtr = @ptrCast(sp.array() + 1);
            sp.top = Object.from(obj);
            aI.initObjectStructure(obj, classIndex, .onStack);
            aI.initContents(obj);
            return sp;
        }
    }
    return error.NoSpace;
}

pub fn collectNursery(self: *align(1) Self, sp: SP, context: *Context, need: usize) void {
    assert(need <= Process.nursery_size);
    const ageSizes = [_]usize{0} ** Process.lastNurseryAge;
    self.collectNurseryPass(sp, context, ageSizes, Process.lastNurseryAge + 1);
    if (self.freeNursery() >= need) return;
    var total: usize = 0;
    var age = Process.lastNurseryAge;
    while (age >= 0) : (age -= 1) {
        total += ageSizes[age];
        if (total >= need) {
            self.collectNurseryPass(sp, context, ageSizes, age);
            return;
        }
    }
    unreachable;
}
fn collectNurseryPass(self: *align(1) Self, originalSp: SP, contextMutable: *Context, sizes: [Process.lastNurseryAge]usize, promoteAge: usize) void {
    _ = .{ sizes, promoteAge };
    var scan = self.header().otherHeap;
    var hp = scan;
    var context = contextMutable;
    const endStack = self.endOfStack();
    var sp = originalSp;
    // find references from the stacked contexts
    while (context.endOfStack()) |endSP| {
        while (sp.lessThan(endSP)) {
            if (sp.top.asMemoryObject()) |pointer|
                hp = pointer.copyTo(hp, &sp.top);
            sp = sp.drop();
        }
        sp = context.callerStack();
        context = context.previous();
    }
    // find references from the residual stack
    while (sp.lessThan(endStack)) {
        if (sp.top.asMemoryObject()) |pointer|
            hp = pointer.copyTo(hp, &sp.top);
        sp = sp.drop();
    }
    // find self referencesy
    var count: usize = 10;
    while (@intFromPtr(hp) > @intFromPtr(scan)) {
        if (scan[0].iterator()) |iter| {
            var it = iter;
            while (it.next()) |objPtr| {
                if (objPtr.asMemoryObject()) |pointer| {
                    if (pointer.isForwarded()) {
                        unreachable;
                    } else if (pointer.isNursery())
                        hp = pointer.copyTo(hp, objPtr);
                }
            }
        }
        scan = scan[0].skipForward();
        count = count - 1;
    }
    // swap heaps
    const head = self.header();
    const tempHeap = head.otherHeap;
    head.otherHeap = head.currHeap;
    head.currHeap = tempHeap;
    head.currHp = hp;
    head.currEnd = tempHeap + Process.nursery_size;
}
pub fn resetForTest() void {
    allProcesses = null;
}
test "nursery allocation" {
    const ee = std.testing.expectEqual;
    var process align(alignment) = new();
    var pr = &process;
    pr.init(Nil());
    const emptySize = Process.nursery_size;
    try ee(Process.stack_size, 27);
    try ee(pr.freeNursery(), emptySize);
    var sp = pr.endOfStack();
    var initialContext = Context.init();
    var ar = pr.alloc(ClassIndex.Class, 4, null, void, false);
    _ = ar.initAll();
    const o1 = ar.allocated;
    try ee(pr.freeNursery(), emptySize - 5);
    ar = pr.alloc(ClassIndex.Class, 5, null, void, false);
    _ = ar.initAll();
    ar = pr.alloc(ClassIndex.Class, 6, null, void, false);
    const o2 = ar.initAll();
    try ee(emptySize - 19, pr.freeNursery());
    try o1.instVarPut(0, o2.asObject());
    sp = sp.push(o1.asObject()).?;
    const news, const newContext, _ = pr.spillStack(sp, &initialContext, Extra.none);
    try ee(sp, news);
    try ee(&initialContext, newContext);
    pr.collectNursery(sp, &initialContext, 0);
    try ee(emptySize - switch (config.objectEncoding) {
        .zag, .nan => 12,
        else => 7,
    }, pr.freeNursery());
    // age test
    // o1 still contains corrected address of o2
    // add second reference to o2 and circulare ref to o1
    // sp.top should be updated
}
test "check flag" {
    const testing = std.testing;
    var process: struct { f: [alignment]u8 align(alignment) = undefined, p: Self } = .{ .p = new() };
    @as(*align(alignment) Self, @alignCast(&process.p)).init(Nil());
    var pr align(1) = &process.p;
    try testing.expect(!pr.needsCheck());
    const origEOS = pr.endOfStack();
    try testing.expect(!pr.needsCheck());
    for (0..countMask) |_| {
        pr = pr.checkBump();
    }
    try testing.expect(!pr.needsCheck());
    try testing.expectEqual(pr.endOfStack(), origEOS);
    pr = pr.checkBump();
    try testing.expect(pr.needsCheck());
    pr = pr.clearCount().maxCount();
    try testing.expect(!pr.needsCheck());
    pr = pr.checkBump();
    try testing.expect(pr.needsCheck());
}
pub const SP = *Stack;
pub const initStack = Stack.from;
const Stack = struct {
    top: Object,
    next: Object,
    third: Object,
    comptime {
        std.debug.assert(@offsetOf(Stack, "top") == 0);
        std.debug.assert(@offsetOf(Stack, "next") == @sizeOf(Object));
        std.debug.assert(@offsetOf(Stack, "third") == @sizeOf(Object) * 2);
    }
    pub inline fn lessThan(self: SP, other: anytype) bool {
        return @intFromPtr(self) < @intFromPtr(other);
    }
    fn from(self: anytype) SP {
        return @ptrCast(self);
    }
    pub inline fn push(self: SP, v: Object) ?SP {
        if (self.reserve(1)) |newSp| {
            newSp.top = @bitCast(v);
            return newSp;
        }
        return null;
    }
    pub inline fn pushRawInt(self: SP, v: u64) ?SP {
        if (self.reserve(1)) |newSp| {
            newSp.top = @bitCast(v);
            return newSp;
        }
        return null;
    }
    pub inline fn dropPut(self: SP, v: Object) SP {
        self.next = v;
        return self.unreserve(1);
    }
    pub inline fn drop(self: SP) SP {
        return self.unreserve(1);
    }
    pub inline fn reserve(self: SP, n: anytype) ?SP {
        const selfInt = @intFromPtr(self);
        const newInt = selfInt - @sizeOf(Object) * n;
        if (n == 1 and newInt & stack_mask > 0) {
            return @ptrFromInt(newInt);
        } else if ((selfInt & stack_mask_overflow) == (newInt & stack_mask_overflow)) {
            return @ptrFromInt(newInt);
        } else return null;
    }
    pub inline fn safeReserve(self: SP, n: usize) SP {
        return @ptrFromInt(@intFromPtr(self) - @sizeOf(Object) * n);
    }
    pub inline fn unreserve(self: SP, n: usize) SP {
        return @ptrFromInt(@intFromPtr(self) + @sizeOf(Object) * n);
    }
    pub inline fn delta(self: SP, other: SP) usize {
        return (@intFromPtr(other) - @intFromPtr(self)) / @sizeOf(Object);
    }
    pub inline fn array(self: SP) [*]Object {
        return @ptrCast(self);
    }
    pub inline fn slice(self: SP, n: usize) []Object {
        return self.array()[0..n];
    }
    pub inline//
    fn sliceTo(self: SP, a_ptr: anytype) []Object {
        const i_ptr = @intFromPtr(a_ptr);
        return self.slice(((i_ptr - @intFromPtr(self))) / @sizeOf(Object));
    }
    pub inline fn at(self: SP, n: usize) Object {
        return self.array()[n];
    }
    pub inline fn atPut(self: SP, n: usize, o: Object) void {
        self.array()[n] = o;
    }
};
test "Stack" {
    std.debug.print("Test: Stack\n", .{});
    var process: Self align(alignment) = new();
    process.init(Nil());
    const ee = std.testing.expectEqual;
    var stack: [11]Object = undefined;
    const sp0 = @as(SP, @ptrCast(&stack[10]));
    sp0.top = True();
    try ee(True(), stack[10]);
    const sp1 = sp0.push(False()).?;
    try ee(True(), stack[10]);
    try ee(False(), stack[9]);
    _ = sp1.drop().push(Object.from(42, &process));
    try ee(Object.from(42, &process).to(i64), 42);
    try ee(stack[9].to(i64), 42);
}
pub const threadedFunctions = struct {
    pub const pushThisProcess = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Self, context: *Context, extra: Extra) Result {
            const newSp = sp.push(process.ptr().h.process);
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp.?, process, context, extra });
        }
    };
};
