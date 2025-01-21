const std = @import("std");
const math = std.math;
const assert = std.debug.assert;
const mem = std.mem;
const builtin = @import("builtin");
const config = @import("config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
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
const TFProcess = execute.TFProcess;
const TFContext = execute.TFContext;
const tfAsProcess = execute.tfAsProcess;
const tfAsContext = execute.tfAsContext;
const PC = execute.PC;
const SP = execute.SP;
const MethodSignature = execute.MethodSignature;
const CodeContextPtr = @import("execute.zig").CodeContextPtr;

//test "force dispatch load" {
//    dispatch.forTest();
//}
const process_total_size = 64 * 1024; // must be more than HeapObject.maxLength*8 so externally allocated
m: [@sizeOf(Process)] u8,
const Process = extern struct {
    stack: [stack_size]Object,
    h: Fields,
    nursery0: [nursery_size]Object,
    nursery1: [nursery_size]Object,
    const Fields = extern struct {
        next: ?*Self,
        id: u64,
        trapContextNumber: u64,
        debugFn: execute.ThreadedFn,
        sp: SP,
        currHeap: HeapObjectArray,
        currHp: HeapObjectArray,
        currEnd: HeapObjectArray,
        otherHeap: HeapObjectArray,
    };
    const headerSize = @sizeOf(Fields);
    const processAvail = (process_total_size - headerSize) / @sizeOf(Object);
    const stack_size = processAvail / 9;
    const nursery_size = (processAvail - stack_size) / 2;
    comptime {
        assert(stack_size <= nursery_size);
    }
    const lastNurseryAge = Age.lastNurseryAge;
    const maxNurseryObjectSize = @min(HeapHeader.maxLength, nursery_size / 4);
    const maxStackObjectSize = @min(HeapHeader.maxLength, stack_size / 4);
};
const Self = @This();
var allProcesses: ?*Self = null;
pub inline fn ptr(self: *const align(1) Self) *align(alignment) Process {
    return @ptrFromInt(@intFromPtr(self) & nonFlags);
}
pub inline fn header(self: *const align(1) Self) *Process.Fields {
    return &self.ptr().h;
}
pub fn new() align(alignment) Self {
    return undefined;
}
pub fn init(origin: *align(1) Self) void {
    const self = origin.ptr();
    const h = @as(HeapObjectArray, @alignCast(@ptrCast(&self.stack[0])));
    const stack_end = h + Process.stack_size;
    std.debug.assert(@as(*Process,@alignCast(@ptrCast(self))) == origin.ptr());
    self.h.sp = @ptrCast(stack_end);
    self.h.currHeap = stack_end;
    self.h.otherHeap = self.h.currHeap + Process.nursery_size;
    self.h.currEnd = stack_end + Process.nursery_size;
    self.h.currHp = self.h.currHeap;
    while (true) {
        self.h.next = allProcesses;
        self.h.id = if (allProcesses) |p| p.header().id + 1 else 1;
        trace("\nprocess.init {}", .{self.h.id});
        if (@cmpxchgWeak(?*Self, &allProcesses, self.h.next, origin, SeqCst, SeqCst) == null) break;
    }
    self.h.trapContextNumber = 0;
}
const countType = u5;
const countMask: usize = math.maxInt(u5);
const countOverflowFlag = countMask + 1;
const othersFlag = countOverflowFlag << 1;
const checkFlags = othersFlag | countOverflowFlag;
const flagMask = checkFlags | countMask;
const alignment = flagMask + 1;
const nonFlags = ~flagMask;
pub inline fn needsCheck(self: *const align(1) Self) bool {
    return (@intFromPtr(self) & checkFlags) != 0;
}
pub fn check(pc: PC, sp: SP, process: TFProcess, context: TFContext, signature: MethodSignature) callconv(stdCall) SP {
    return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, signature });
}
pub inline fn checkBump(self: *align(1) Self) *align(1) Self {
    if (self.needsCheck()) return self;
    @setRuntimeSafety(false);
    return @as(*align(1) Self, @ptrFromInt(@intFromPtr(self) + 1));
}
pub inline fn maxCheck(self: *const align(1) Self) *align(1) Self {
    @setRuntimeSafety(false);
    return @as(*Self, @ptrFromInt(@intFromPtr(self) | countMask));
}
pub fn deinit(self: *align(1) Self) void {
    self.ptr().* = undefined;
}
pub inline fn endOfStack(self: *const align(1) Self) SP {
    return @ptrCast(@as([*]Object, @ptrCast(&self.ptr().stack[0])) + Process.stack_size);
}
pub inline fn setSp(self: *align(1) Self, sp: SP) void {
    self.header().sp = sp;
}
pub inline fn freeStack(self: *const align(1) Self, sp: SP) usize {
    return (@intFromPtr(sp) - @intFromPtr(self.ptr())) / 8;
}
pub inline fn getStack(self: *const align(1) Self, sp: SP) []Object {
    return sp.slice((@intFromPtr(self.endOfStack()) - @intFromPtr(sp)) / @sizeOf(Object));
}
pub inline fn allocStackSpace(self: *align(1) Self, sp: SP, words: usize) !SP {
    const newSp = sp.reserve(words);
    if (@intFromPtr(newSp) > @intFromPtr(self)) return newSp;
    return error.NoSpace;
}
pub inline fn getHeap(self: *const align(1) Self) []HeapObject {
    return self.header().currHeap[0..((@intFromPtr(self.header().currHp) - @intFromPtr(self.header().currHeap)) / @sizeOf(Object))];
}
pub inline fn freeNursery(self: *const align(1) Self) usize {
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
//};
pub fn resetForTest() void {
    allProcesses = null;
}
test "nursery allocation" {
    const ee = std.testing.expectEqual;
    var process align(alignment) = new();
    var pr = &process;
    pr.init();
    const emptySize = Process.nursery_size;
    trace("\nemptySize = {}\n", .{emptySize});
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
    var process align(1) = new();
    var pr = &process;
    pr.init();
    try testing.expect(!pr.needsCheck());
    const origEOS = pr.endOfStack();
    pr = pr.maxCheck();
    try testing.expect(!pr.needsCheck());
    var count = countMask - 1;
    while (count > 1) : (count -= 1) {
        pr = pr.checkBump();
    }
    try testing.expect(!pr.needsCheck());
    try testing.expectEqual(pr.endOfStack(), origEOS);
    pr = pr.checkBump();
    try testing.expect(pr.needsCheck());
}
test "allocStack" {
    //    const testing = std.testing;
    var process align(alignment) = new();
    var pr = &process;
    pr.init();
}
