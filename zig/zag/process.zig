const std = @import("std");
const math = std.math;
const assert = std.debug.assert;
const mem = std.mem;
const builtin = @import("builtin");
const config = @import("config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const SeqCst = std.builtin.AtomicOrder.seq_cst;
const object = @import("zobject.zig");
const Object = object.Object;
const Nil = object.Nil;
const ClassIndex = object.ClassIndex;
const checkEqual = @import("utilities.zig").checkEqual;
//const dispatch = @import("dispatch.zig");
const heap = @import("heap.zig");
const HeapHeader = heap.HeapHeader;
const HeapObject = heap.HeapObject;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectArray = heap.HeapObjectArray;
const footer = heap.footer;
const Age = heap.Age;
const Format = heap.Format;
const allocationInfo = heap.AllocationInfo.calc;
const AllocReturn = heap.AllocReturn;
const Context = @import("context.zig");
const ContextPtr = *Context;
const execute = @import("execute.zig");
const SendCache = execute.SendCache;
const Code = execute.Code;
const PC = execute.PC;
const SP = execute.SP;
const Extra = execute.Extra;
const Result = execute.Result;
const CodeContextPtr = execute.CodeContextPtr;

/// this is really a Process object with the low bits encoding additional information
m: [process_total_size]u8 align(1), // alignment explicitly stated to emphasize the difference from Process
const process_total_size = if (config.is_test) 2048 else 64 * 1024; // must be more than HeapObject.maxLength*8 so externally allocated
pub const alignment = flagMask + 1;
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
        debugFn: ?execute.ThreadedFn.Fn,
        sp: SP,
        process: Object,
        currHeap: HeapObjectArray,
        currHp: HeapObjectArray,
        currEnd: HeapObjectArray,
        otherHeap: HeapObjectArray,
    };
    const headerSize = @sizeOf(Fields);
    const processAvail = (process_total_size - headerSize) / @sizeOf(Object);
    const nursery_size = (processAvail - processAvail / 9) / 2;
    const stack_size = processAvail - nursery_size * 2;
    comptime {
        assert(stack_size <= nursery_size);
    }
    const lastNurseryAge = Age.lastNurseryAge;
    const maxNurseryObjectSize = @min(HeapHeader.maxLength, nursery_size / 4);
    const maxStackObjectSize = @min(HeapHeader.maxLength, stack_size / 4);
};
pub fn format(
    orig: *const @This(),
    comptime fmt: []const u8,
    options: std.fmt.FormatOptions,
    writer: anytype,
) !void {
    _ = fmt;
    _ = options;
    const self = orig.ptr();
    try writer.print("process: {} .stack = {any}", .{ orig.header().id, orig.getStack(self.h.sp) });
    try writer.print(" .heap = {any}", .{orig.getHeap()});
}
comptime {
    assert(process_total_size == @sizeOf(Process));
}
const Self = @This();
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
        trace("\nprocess.init {}", .{self.h.id});
        if (@cmpxchgWeak(?*Self, &allProcesses, self.h.next, origin, SeqCst, SeqCst) == null) break;
    }
    self.h.trapContextNumber = 0;
}
pub fn deinit(self: *align(1) Self) void {
    self.ptr().* = undefined;
}
const countType = u5;
const countMask: usize = math.maxInt(u5);
const countOverflowFlag = countMask + 1;
const nonCount = ~(countOverflowFlag + countMask);
const othersFlag = countOverflowFlag << 1;
const checkFlags = othersFlag | countOverflowFlag;
const flagMask = checkFlags | countMask;
const nonFlags = ~flagMask;
pub inline fn check(self: *align(1) const Self, next: execute.ThreadedFn.Fn) execute.ThreadedFn.Fn {
    return if (self.needsCheck()) &fullCheck else next;
}
inline fn needsCheck(self: *align(1) const Self) bool {
    return (@intFromPtr(self) & checkFlags) != 0;
}
fn fullCheck(pc: PC, sp: SP, process: *align(1) Self, context: *Context, signature: Extra) Result {
    return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, signature });
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
pub //inline
fn getStack(self: *align(1) const Self, sp: SP) []Object {
    //    return sp.slice((@intFromPtr(self.endOfStack()) - @intFromPtr(sp)) / @sizeOf(Object));
    return sp.sliceTo(self.endOfStack());
}
pub inline fn allocStackSpace(self: *align(1) Self, sp: SP, words: usize) !SP {
    const newSp = sp.reserve(words);
    if (@intFromPtr(newSp) > @intFromPtr(self)) return newSp;
    return error.NoSpace;
}
pub inline fn getHeap(self: *align(1) const Self) []HeapObject {
    return self.header().currHeap[0..((@intFromPtr(self.header().currHp) - @intFromPtr(self.header().currHeap)) / @sizeOf(Object))];
}
pub inline fn freeNursery(self: *align(1) const Self) usize {
    return (@intFromPtr(self.header().currEnd) - @intFromPtr(self.header().currHp)) / 8;
}
pub fn spillStack(self: *align(1) Self, sp: SP, contextMutable: *ContextPtr) SP {
    if (!contextMutable.*.isOnStack()) return sp;
    // if the Context is on the stack, both the Context and the SP will move
    _ = .{ self, @panic("unimplemented") };
}
pub fn alloc(self: *align(1) Self, classIndex: ClassIndex, iVars: u12, indexed: ?usize, comptime element: type, makeWeak: bool) heap.AllocReturn {
    const aI = allocationInfo(iVars, indexed, element, makeWeak);
    if (aI.objectSize(Process.maxNurseryObjectSize)) |size| {
        const result = self.header().currHp;
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
    return error.NeedNurseryCollection;
}
pub fn allocStack(self: *align(1) Self, oldSp: SP, classIndex: ClassIndex, iVars: u12, indexed: ?usize, comptime element: type) !SP {
    const aI = allocationInfo(iVars, indexed, element, false);
    if (aI.objectSize(Process.maxStackObjectSize)) |size| {
        const sp = try self.allocStackSpace(oldSp, size + 2);
        const obj: heap.HeapObjectPtr = @ptrCast(sp.array() + 1);
        sp.top = Object.from(obj);
        aI.initObjectStructure(obj, classIndex, .onStack);
        aI.initContents(obj);
        return sp;
    }
    return error.NoSpace;
}

pub fn collectNursery(self: *align(1) Self, sp: SP, contextMutable: *ContextPtr, need: usize) void {
    assert(need <= Process.nursery_size);
    const ageSizes = [_]usize{0} ** Process.lastNurseryAge;
    self.collectNurseryPass(sp, contextMutable, ageSizes, Process.lastNurseryAge + 1);
    if (self.freeNursery() >= need) return;
    // var total: usize = 0;
    // var age = lastNurseryAge;
    // while (age>=0) : ( age -= 1) {
    //     total += ageSizes[age];
    //     if (total >= need) {
    //         self.collectNurseryPass(sp, contextMutable, ageSizes, age);
    //         return;
    //     }
    // }
    unreachable;
}
fn collectNurseryPass(self: *align(1) Self, originalSp: SP, contextMutable: *ContextPtr, sizes: [Process.lastNurseryAge]usize, promoteAge: usize) void {
    _ = .{ sizes, promoteAge };
    var scan = self.header().otherHeap;
    var context = contextMutable.*;
    const endStack = self.endOfStack();
    var sp = originalSp;
    var hp = scan;
    // find references from the stack
    while (sp.lessThan(endStack)) {
        const endSP = context.endOfStack(self);
        while (sp.lessThan(endSP)) {
            trace("sp: before{} {*}\n", .{ sp.top, hp });
            if (sp.top.asMemoryObject()) |pointer| {
                hp = pointer.copyTo(hp, &sp.top);
            }
            trace("sp: after {} {*}\n", .{ sp.top, hp });
            sp = sp.drop();
        }
        if (!context.isOnStack()) break;
        // scan specials
        context = context.previous();
        unreachable;
    }
    // find self references
    while (@intFromPtr(hp) < @intFromPtr(scan)) {
        trace("hp: {*} scan: {*}\n", .{ hp, scan });
        const heapObject = scan - 1;
        trace("obj: {} {any}\n", .{ heapObject[0], heapObject[0].instVars() });
        //@compileLog(heapObject[0],heapObject[0].iterator());
        if (heapObject[0].iterator()) |iter| {
            //@compileLog(iter);
            trace("iter: {}\n", .{iter});
            var it = iter;
            while (it.next()) |objPtr| {
                if (objPtr.asMemoryObject()) |pointer| {
                    if (pointer.isForwarded()) {
                        unreachable;
                    } else if (pointer.header.age.isNursery())
                        hp = pointer.copyTo(hp, objPtr);
                }
            }
        }
        scan = heapObject[0].skipForward();
    }
    // swap heaps
    const h = self.header();
    const tempHeap = h.otherHeap;
    h.otherHeap = h.currHeap;
    h.currHeap = tempHeap;
    h.currHp = hp;
    h.currEnd = tempHeap - Process.nursery_size;
    @panic("assumes heap grows down");
}
pub fn resetForTest() void {
    allProcesses = null;
}
test "nursery allocation" {
    const ee = std.testing.expectEqual;
    var process align(alignment) = new();
    var pr = &process;
    pr.init(Nil);
    const emptySize = Process.nursery_size;
    trace("\nemptySize = {}\n", .{emptySize});
    try ee(Process.stack_size, 27);
    try ee(pr.freeNursery(), emptySize);
    var sp = pr.endOfStack();
    var initialContext = Context.init();
    var mutableContext = &initialContext;
    var ar = try pr.alloc(ClassIndex.Class, 4, null, void, false);
    _ = ar.initAll();
    const o1 = ar.allocated;
    try ee(pr.freeNursery(), emptySize - 5);
    ar = try pr.alloc(ClassIndex.Class, 5, null, void, false);
    _ = ar.initAll();
    ar = try pr.alloc(ClassIndex.Class, 6, null, void, false);
    const o2 = ar.initAll();
    try ee(pr.freeNursery(), emptySize - 18);
    try o1.instVarPut(0, o2.asObject());
    sp = sp.push(o1.asObject());
    try ee(@intFromPtr(pr.spillStack(sp, &mutableContext)), @intFromPtr(sp));
    try ee(@intFromPtr(&initialContext), @intFromPtr(mutableContext));
    pr.collectNursery(sp, &mutableContext, 0);
    try ee(pr.freeNursery(), emptySize - 12);
    // age test
    // o1 still contains corrected address of o2
    // add second reference to o2 and circulare ref to o1
    // sp.top should be updated
}
test "check flag" {
    const testing = std.testing;
    var process: struct { f: [alignment]u8 align(alignment) = undefined, p: Self } = .{ .p = new() };
    @as(*align(alignment) Self, @alignCast(&process.p)).init(Nil);
    var pr align(1) = &process.p;
    try testing.expect(!pr.needsCheck());
    const origEOS = pr.endOfStack();
    try testing.expect(!pr.needsCheck());
    std.debug.print("before: {x}\n", .{@intFromPtr(pr)});
    for (0..countMask) |_| {
        pr = pr.checkBump();
    }
    std.debug.print("after:  {x}\n", .{@intFromPtr(pr)});
    try testing.expect(!pr.needsCheck());
    try testing.expectEqual(pr.endOfStack(), origEOS);
    pr = pr.checkBump();
    try testing.expect(pr.needsCheck());
    pr = pr.clearCount().maxCount();
    try testing.expect(!pr.needsCheck());
    pr = pr.checkBump();
    try testing.expect(pr.needsCheck());
}
pub const threadedFunctions = struct {
    pub const pushThisProcess = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Self, context: *Context, extra: Extra) Result {
            const newSp = sp.push(process.ptr().h.process);
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, context, extra });
        }
    };
};
