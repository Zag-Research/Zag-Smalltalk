const std = @import("std");
const assert = std.debug.assert;
const mem = std.mem;
const builtin = @import("builtin");
const config = @import("config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const SeqCst = std.builtin.AtomicOrder.SeqCst;
const object = @import("zobject.zig");
const Object = object.Object;
const Nil = object.Nil;
const ClassIndex = object.ClassIndex;
const checkEqual = @import("utilities.zig").checkEqual;
//const dispatch = @import("dispatch.zig");
const heap = @import("heap.zig");
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectArray = heap.HeapObjectArray;
const HeapObject = heap.HeapObject;
const footer = heap.footer;
const Age = heap.Age;
const Format = heap.Format;
const allocationInfo = Format.allocationInfo;
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
    nursery0: [nursery_size]Object,
    nursery1: [nursery_size]Object,
    next: ?*Self,
    id: u64,
    debugFn: ?ThreadedFn,
    sp: SP,
    currHeap: HeapObjectArray,
    currHp: HeapObjectArray,
    currEnd: HeapObjectArray,
    otherHeap: HeapObjectArray,
    trapContextNumber: u64,
    const Self = @This();
    const headerSize = @sizeOf(?*Self) + @sizeOf(u64) + @sizeOf(?ThreadedFn) + @sizeOf(SP) + @sizeOf(HeapObjectArray) + @sizeOf(HeapObjectArray) + @sizeOf(HeapObjectArray) + @sizeOf(HeapObjectArray) + @sizeOf(u64);
    const ThreadedFn = *const fn (programCounter: PC, stackPointer: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) SP;
    const processAvail = (process_total_size - headerSize) / @sizeOf(Object);
    const stack_size = processAvail / 9;
    const nursery_size = (processAvail - stack_size) / 2;
    comptime { assert(stack_size <= nursery_size);}
    const lastNurseryAge = Age.lastNurseryAge;
    var allProcesses: ?*Self = null;
    pub fn new() Self {
        return undefined;
    }
    pub fn init(self: *Self) void {
        const h = @as(HeapObjectArray, @ptrCast(&self.stack[0]));
        const stack_end = h + stack_size;
        const at = allProcesses;
        self.sp = @ptrCast(stack_end);
        self.currHeap = stack_end + nursery_size;
        self.otherHeap = self.currHeap + nursery_size;
        self.currEnd = stack_end;
        self.currHp = self.currHeap;
        while (true) {
            self.next = at;
            self.id = if (at) |p| p.id + 1 else 1;
            if (@cmpxchgWeak(?*Self, &allProcesses, self.next, self, SeqCst, SeqCst) == null) break;
        }
        self.trapContextNumber = 0;
    }
    const checkType = u5;
    const checkMax: checkType = @as(checkType, @truncate(std.mem.page_size - 1));
    pub inline fn needsCheck(self: *const Self) bool {
        return @as(checkType, @truncate(@intFromPtr(self))) == 1;
    }
    pub inline fn decCheck(self: *Self) *Self {
        if (self.needsCheck()) return self;
        @setRuntimeSafety(false);
        return @as(*Self, @ptrFromInt(@intFromPtr(self) - 1));
    }
    pub inline fn maxCheck(self: *const Self) *Self {
        @setRuntimeSafety(false);
        return @as(*Self, @ptrFromInt(@intFromPtr(self) | checkMax));
    }
    pub inline fn noCheck(self: *const Self) *Self {
        return @as(*Self, @ptrFromInt(@intFromPtr(self) & ~@as(usize, checkMax)));
    }
    pub inline fn debugger(self: *Self) ?ThreadedFn {
        return self.debugFn;
    }
    inline fn ptr(self: *const Self) *Self {
        return @as(*Self, @ptrFromInt(@intFromPtr(self.noCheck())));
    }
    pub fn deinit(self: *Self) void {
        self.ptr().* = undefined;
    }
    pub inline fn endOfStack(self: *const Self) SP {
        return @ptrCast(@as([*]Object, @ptrCast(&self.ptr().stack[0])) + stack_size);
    }
    pub inline fn freeStack(self: *const Self, sp: SP) usize {
        return (@intFromPtr(sp)-@intFromPtr(self.ptr()))/8;
    }
    pub inline fn getStack(self: *const Self, sp: SP) []Object {
        trace("\ngetStack: sp={x} eos={x}",.{ @intFromPtr(sp), @intFromPtr(self.endOfStack()) });
        return sp.slice((@intFromPtr(self.endOfStack()) - @intFromPtr(sp)) / @sizeOf(Object));
    }
    pub inline fn allocStack(self: *Self, sp: SP, words: u64) !SP {
        const newSp = sp.reserve(words);
        if (@intFromPtr(newSp) > @intFromPtr(self)) return newSp;
        return error.NoSpace;
    }
    pub inline fn freeNursery(self: *const Self) usize {
        return (@intFromPtr(self.currHp)-@intFromPtr(self.currEnd))/8;
    }
    //allocationInfo(iVars: u12, indexed: ?usize, eSize: ?usize, makeWeak: bool)
    //fillFooters(self: Self, theHeapObject: HeapObjectPtr, classIndex: u16, age: Age, nElements: usize, elementSize: usize)
    pub fn spillStack(self: *Self, sp: SP, contextMutable: *ContextPtr) SP {
        if (!contextMutable.*.isOnStack()) return sp;
        // if the Context is on the stack, both the Context and the SP will move
        _ = .{self,@panic("unimplemented")};
    }
    pub fn alloc(self: *Self, classIndex: ClassIndex, iVars: u12, indexed: ?usize, elementSize: usize, makeWeak: bool) heap.AllocReturn {
        const aI = allocationInfo(iVars, indexed, elementSize, makeWeak);
        if (aI.wholeSize(@min(HeapObject.maxLength,nursery_size / 4))) |size| {
            const result = self.currHp - 1;
            const newHp = result - size;
            if (@intFromPtr(newHp) >= @intFromPtr(self.currEnd)) {
                self.currHp = newHp;
                const obj = @as(heap.HeapObjectPtr, @ptrCast(result));
                _ = aI.fillFooters(obj, classIndex, .nursery, indexed orelse 0, elementSize);
                return .{
                    .age = .nursery,
                    .allocated = obj,
                    .info = aI,
                };
            }
        } else |err| {
            return err;
        }
        return error.NeedNurseryCollection;
    }
    pub fn collectNursery(self: *Self, sp: SP, contextMutable: *ContextPtr, need: usize) void {
        assert(need<=nursery_size);
        var ageSizes = [_]usize{0}**lastNurseryAge;
        self.collectNurseryPass(sp, contextMutable, ageSizes, lastNurseryAge+1);
        if (self.freeNursery() >= need) return;
        var total: usize = 0;
        var age = lastNurseryAge;
        while (age>=0) : ( age -= 1) {
            total += ageSizes[age];
            if (total >= need) {
                self.collectNurseryPass(sp, contextMutable, ageSizes, age);
                return;
            }
        }
        unreachable;
    }
    fn collectNurseryPass(self: *Self, originalSp: SP, contextMutable: *ContextPtr, sizes: [lastNurseryAge]usize, promoteAge: usize) void {
        _ = .{sizes, promoteAge};
        var scan = self.otherHeap;
        var context = contextMutable.*;
        const endStack = self.endOfStack();
        var sp = originalSp;
        var hp = scan;
        // find references from the stack
        while (sp.lessThan(endStack)) {
            const endSP = context.endOfStack(self);

            if (!context.isOnStack()) break;
            // scan specials
            sp = endSP;
            context = context.previous();
        }
        // find self references
        while (@intFromPtr(hp)<@intFromPtr(scan)) {
            const heapObject = @as(HeapObjectPtr,@ptrCast(scan-1));
            const iter = heapObject.makeIterator();
            while (iter.next()) |field| {
                _ = field;
            }
            scan = heapObject.skipBack();
        }
        // swap heaps
        const tempHeap = self.otherHeap;
        self.otherHeap = self.currHeap;
        self.currHeap = tempHeap;
        self.currHp = tempHeap;
        self.currEnd = tempHeap - nursery_size;
    }
};
test "nursery allocation" {
    const ee = std.testing.expectEqual;
    var process = Process.new();
    var pr = &process;
    pr.init();
    const emptySize = Process.nursery_size;
    try ee(pr.freeNursery(),emptySize);
    var sp = pr.endOfStack();
    var initialContext = Context.init();
    var mutableContext = &initialContext;
    var ar = try pr.alloc(ClassIndex.Class,4,null,0,false);
    ar.nilAll();
    const o1 = ar.allocated;
    try ee(pr.freeNursery(),emptySize-5);
    ar = try pr.alloc(ClassIndex.Class,5,null,0,false);
    ar.nilAll();
    ar = try pr.alloc(ClassIndex.Class,6,null,0,false);
    ar.nilAll();
    const o2 = ar.allocated;
    try ee(pr.freeNursery(),emptySize-18);
    o1.instVarPut(0,o2.asObject());
    sp = sp.push(o1.asObject());
    try ee(@intFromPtr(pr.spillStack(sp,&mutableContext)),@intFromPtr(sp));
    try ee(@intFromPtr(&initialContext),@intFromPtr(mutableContext));
    pr.collectNursery(sp,&mutableContext,0);
    try ee(pr.freeNursery(),emptySize-12);
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
