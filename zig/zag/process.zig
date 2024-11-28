const std = @import("std");
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
const Context = @import("context.zig").Context;
const ContextPtr = *Context;
const execute = @import("execute.zig");
const SendCache = execute.SendCache;
const Code = execute.Code;
const PC = execute.PC;
const SP = execute.SP;
const CodeContextPtr = @import("execute.zig").CodeContextPtr;

//test "force dispatch load" {
//    dispatch.forTest();
//}
const process_total_size = 64 * 1024; // must be more than HeapObject.maxLength*8 so externally allocated
pub const Process = extern struct {
    stack: [stack_size]Object,
    next: ?*Self,
    id: u64,
    trapContextNumber: u64,
    debugFn: ?ThreadedFn,
    sp: SP,
    currHeap: HeapObjectArray,
    currHp: HeapObjectArray,
    currEnd: HeapObjectArray,
    otherHeap: HeapObjectArray,
    nursery0: [nursery_size]Object,
    nursery1: [nursery_size]Object,
    const Self = @This();
    const headerSize = @sizeOf(?*Self) + @sizeOf(u64) + @sizeOf(?ThreadedFn) + @sizeOf(SP) + @sizeOf(HeapObjectArray) + @sizeOf(HeapObjectArray) + @sizeOf(HeapObjectArray) + @sizeOf(HeapObjectArray) + @sizeOf(u64);
    const ThreadedFn = execute.ThreadedFn;
    const processAvail = (process_total_size - headerSize) / @sizeOf(Object);
    const stack_size = processAvail / 9;
    const nursery_size = (processAvail - stack_size) / 2;
    comptime {
        assert(stack_size <= nursery_size);
    }
    const lastNurseryAge = Age.lastNurseryAge;
    const maxNurseryObjectSize = @min(HeapHeader.maxLength, nursery_size / 4);
    const maxStackObjectSize = @min(HeapHeader.maxLength, stack_size / 4);
    var allProcesses: ?*Self = null;
    pub fn new() Self {
        return undefined;
    }
    pub fn resetForTest() void {
        allProcesses = null;
    }
    pub fn init(self: *Self) void {
        const h = @as(HeapObjectArray, @ptrCast(&self.stack[0]));
        const stack_end = h + stack_size;
        std.debug.assert(self == self.ptr());
        self.sp = @ptrCast(stack_end);
        self.currHeap = stack_end;
        self.otherHeap = self.currHeap + nursery_size;
        self.currEnd = stack_end + nursery_size;
        self.currHp = self.currHeap;
        while (true) {
            self.next = allProcesses;
            self.id = if (allProcesses) |p| p.id + 1 else 1;
            trace("\nprocess.init {}",.{self.id});
            if (@cmpxchgWeak(?*Self, &allProcesses, self.next, self, SeqCst, SeqCst) == null) break;
        }
        self.trapContextNumber = 0;
    }
    const countType = u5;
    const countMask: usize = math.maxInt(u16);
    const countOverflowFlag = countMask + 1;
    const othersFlag = countOverflowFlag << 1;
    const checkFlags = othersFlag | countOverflowFlag;
    pub inline fn needsCheck(self: *const Self) bool {
        return (@intFromPtr(self) & checkFlags) != 0;
    }
    pub inline fn decCheck(self: *Self) *Self {
        if (self.needsCheck()) return self;
        @setRuntimeSafety(false);
        return @as(*Self, @ptrFromInt(@intFromPtr(self) + 1));
    }
    pub inline fn maxCheck(self: *const Self) *Self {
        @setRuntimeSafety(false);
        return @as(*Self, @ptrFromInt(@intFromPtr(self) | checkMax));
    }
    pub inline fn noCheck(self: *const Self) *Self {
        return @as(*Self, @ptrFromInt(@intFromPtr(self) & ~@as(usize, checkMax)));
    }
    pub inline fn debugger(self: *Self) ?ThreadedFn {
        return self.ptr().debugFn;
    }
    pub inline fn ptr(self: *const Self) *Self {
        return @as(*Self, @ptrFromInt(@intFromPtr(self.noCheck())));
    }
    pub fn deinit(self: *Self) void {
        self.ptr().* = undefined;
    }
    pub inline fn endOfStack(self: *const Self) SP {
        return @ptrCast(@as([*]Object, @ptrCast(&self.ptr().stack[0])) + stack_size);
    }
    pub inline fn setSp(self: *Self, sp: SP) void {
        self.ptr().sp = sp;
    }
    pub inline fn freeStack(self: *const Self, sp: SP) usize {
        return (@intFromPtr(sp) - @intFromPtr(self.ptr())) / 8;
    }
    pub inline fn getStack(self: *const Self, sp: SP) []Object {
        return sp.slice((@intFromPtr(self.endOfStack()) - @intFromPtr(sp)) / @sizeOf(Object));
    }
    pub inline fn allocStackSpace(self: *Self, sp: SP, words: usize) !SP {
        const newSp = sp.reserve(words);
        if (@intFromPtr(newSp) > @intFromPtr(self)) return newSp;
        return error.NoSpace;
    }
    pub inline fn getHeap(self: *const Self) []HeapObject {
        return self.ptr().currHeap[0..((@intFromPtr(self.ptr().currHp) - @intFromPtr(self.ptr().currHeap)) / @sizeOf(Object))];
    }
    pub inline fn freeNursery(self: *const Self) usize {
        return (@intFromPtr(self.ptr().currEnd) - @intFromPtr(self.ptr().currHp)) / 8;
    }
    pub fn spillStack(self: *Self, sp: SP, contextMutable: *ContextPtr) SP {
        if (!contextMutable.*.isOnStack()) return sp;
        // if the Context is on the stack, both the Context and the SP will move
        _ = .{ self, @panic("unimplemented") };
    }
    pub fn alloc(self: *Self, classIndex: ClassIndex, iVars: u12, indexed: ?usize, comptime element: type, makeWeak: bool) heap.AllocReturn {
        const aI = allocationInfo(iVars, indexed, element, makeWeak);
        if (aI.objectSize(maxNurseryObjectSize)) |size| {
            const result = self.currHp;
            const newHp = result + size + 1;
            if (@intFromPtr(newHp) <= @intFromPtr(self.currEnd)) {
                self.currHp = newHp;
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
    pub fn allocStack(self: *Self, oldSp: SP, classIndex: ClassIndex, iVars: u12, indexed: ?usize, comptime element: type) !SP {
        const aI = allocationInfo(iVars, indexed, element, false);
        if (aI.objectSize(maxStackObjectSize)) |size| {
            const sp = try self.allocStackSpace(oldSp, size + 2);
            const obj: heap.HeapObjectPtr = @ptrCast(sp.array() + 1);
            sp.top = Object.from(obj);
            aI.initObjectStructure(obj, classIndex, .onStack);
            aI.initContents(obj);
            return sp;
        }
        return error.NoSpace;
    }
    
    pub fn collectNursery(self: *Self, sp: SP, contextMutable: *ContextPtr, need: usize) void {
        assert(need <= nursery_size);
        const ageSizes = [_]usize{0} ** lastNurseryAge;
        self.collectNurseryPass(sp, contextMutable, ageSizes, lastNurseryAge + 1);
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
    fn collectNurseryPass(self: *Self, originalSp: SP, contextMutable: *ContextPtr, sizes: [lastNurseryAge]usize, promoteAge: usize) void {
        _ = .{ sizes, promoteAge };
        var scan = self.otherHeap;
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
                    if (objPtr.asMemoryObject ()) |pointer| {
                        if (pointer.isForwarded()){
                            unreachable;
                        } else if (pointer.header.age.isNursery())
                            hp = pointer.copyTo(hp, objPtr);
                    }
                }
            }
            scan = heapObject[0].skipForward();
        }
        // swap heaps
        const tempHeap = self.otherHeap;
        self.otherHeap = self.currHeap;
        self.currHeap = tempHeap;
        self.currHp = hp;
        self.currEnd = tempHeap - nursery_size;
        @panic("assumes heap grows down");
    }
    pub fn format(
        orig: *const Process,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        const self = orig.ptr();
        try writer.print("process: {} .stack = {any}",.{self.id,self.getStack(self.sp)});
        try writer.print(" .heap = {any}",.{self.getHeap()});
    }
};
test "nursery allocation" {
    const ee = std.testing.expectEqual;
    var process = Process.new();
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
    var process = Process.new();
    var pr = &process;
    pr.init();
    try testing.expect(!pr.needsCheck());
    const origEOS = pr.endOfStack();
    pr = pr.maxCheck();
    try testing.expect(!pr.needsCheck());
    var count = Process.checkMax - 1;
    while (count > 1) : (count -= 1) {
        pr = pr.decCheck();
    }
    try testing.expect(!pr.needsCheck());
    try testing.expectEqual(pr.endOfStack(), origEOS);
    pr = pr.decCheck();
    try testing.expect(pr.needsCheck());
}
test "allocStack" {
    //    const testing = std.testing;
    var process = Process.new();
    var pr = &process;
    pr.init();
}
