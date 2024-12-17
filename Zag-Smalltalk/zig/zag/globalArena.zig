const std = @import("std");
const print = std.debug.print;
const math = std.math;
const builtin = @import("builtin");
const SeqCst = std.builtin.AtomicOrder.seq_cst;
const mem = std.mem;
const utilities = @import("utilities.zig");
const checkEqual = utilities.checkEqual;
const bitsToRepresent = utilities.bitsToRepresent;
const smallerPowerOf2 = utilities.smallerPowerOf2;
const largerPowerOf2 = utilities.largerPowerOf2;
const largerPowerOf2Not1 = utilities.largerPowerOf2Not1;
const object = @import("zobject.zig");
const Object = object.Object;
const objectWidth = @sizeOf(Object);
const ClassIndex = object.ClassIndex;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const heap = @import("heap.zig");
const HeapObject = heap.HeapObject;
const HeapHeader = heap.HeapHeader;
const Format = heap.Format;
const AllocationInfo = heap.AllocationInfo;
const Age = heap.Age;
const HeapObjectArray = heap.HeapObjectArray;
const HeapObjectPtr = heap.HeapObjectPtr;
const AllocErrors = heap.AllocErrors;
const os = @import("os.zig");

pub const WeakObject = extern struct {
    object: Object,
};
pub const StructObject = extern struct {
    object: Object,
};
const nFreeLists: usize = bitsToRepresent(HeapHeader.maxLength) + 1;
const heap_allocation_size = @max(4 << nFreeLists, @max(128 * 1024, (math.maxInt(u16) + 1) * @sizeOf(Object)));
pub const HeapAllocationPtr = *align(heap_allocation_size) HeapAllocation;
comptime {
    std.testing.expectEqual(@sizeOf(HeapAllocation), heap_allocation_size) catch unreachable;
}
const HeapAllocation = extern struct {
    header: HeapHeader, // Object header with hash='ZAG', class=ZagHeap - e.g. 0x00A8775A41470021
    loadAddress: HeapAllocationPtr, // address that corresponds with the filename
    nextHeap: ?HeapAllocationPtr, // link to next heap header
    freeLists: [nFreeLists]FreeList,
    flags: HeapFlags,
    mutex: MutexType = mutex_init,
    mem: [size]HeapObject,
    const SelfPtr = HeapAllocationPtr;
    const HeapFlags = extern union { int: u64, f: extern struct {
        allocators: u16,
        objectsNeedingScan: u16,
        marking: bool,
    } };
    const field_size = @sizeOf(HeapHeader) + @sizeOf(HeapAllocationPtr) + @sizeOf(?HeapAllocationPtr) + @sizeOf(FreeList) * nFreeLists + @sizeOf(HeapFlags) + @sizeOf(MutexType);
    const size = (heap_allocation_size - field_size) / @sizeOf(HeapObject);
    const minFreeList = 1;
    const mutex_init = MutexType{};
    const MutexType = DummyMutex;
    const DummyMutex = extern struct {
        fn lock(_: *DummyMutex) void {}
        fn unlock(_: *DummyMutex) void {}
    };
    var memoryAllocator = @import("os.zig").MemoryAllocator(HeapAllocation).new();
    fn getAligned() *align(heap_allocation_size) HeapAllocation {
        return @alignCast(memoryAllocator.allocBlock() catch @panic("page allocator failed"));
    }
    fn init() SelfPtr {
        var self = getAligned();
        self.flags.int = 0;
        self.nextHeap = null;
        self.freeLists = FreeList.init(nFreeLists);
        self.putInFreeLists(@as(HeapObjectArray, @ptrCast(&self.mem[0])), 0, size);
        return self;
    }
    fn freeAll(self: SelfPtr) void {
        var ptr: ?SelfPtr = self;
        while (ptr) |ha| {
            ptr = ha.nextHeap;
            ha.deinit();
        }
    }
    fn deinit(self: SelfPtr) void {
        memoryAllocator.unmap(@as([*]align(os.page_size) u8, @ptrCast(self))[0..heap_allocation_size]);
    }
    fn cycleNext(self: SelfPtr) SelfPtr {
        var temp = self.nextHeap;
        if (temp == null) temp = heapAllocations;
        // ToDo: update number of users of self and temp
        return temp orelse unreachable;
    }
    fn putInFreeLists(self: SelfPtr, ptr: HeapObjectArray, from: usize, to: usize) void {
        if (to == from) return;
        //if (self.header.length >= @sizeOf(FreeListElement) / @sizeOf(HeapObject))
        const freeIndex = @min(nFreeLists - 1, bitsToRepresent((to - from) >> 1));
        const freeSize = @as(usize, 1) << freeIndex;
        const freeMask = freeSize - 1;
        var start = (from + freeMask) & ~freeMask;
        if (from < start) self.putInFreeLists(ptr, from, start);
        while (start + freeSize <= to) {
            self.freeLists[freeIndex].addToFree(ptr + start);
            start += freeSize;
        }
        if (start < to) self.putInFreeLists(ptr, start, to);
    }
    fn freeSpace(self: SelfPtr) usize {
        var count: usize = 0;
        for (self.freeLists[0..], 0..nFreeLists) |fl, i| {
            count += fl.count() * (@as(usize, 1) << @as(u6, @truncate(i)));
        }
        return count;
    }
    fn allocLarge(self: SelfPtr, arraySize: ?usize, aI: AllocationInfo) !HeapObjectPtr {
        _ = self;
        _ = arraySize;
        _ = aI;
        return error.HeapFull;
    }
    fn allocOfSize(self: SelfPtr, classIndex: ClassIndex, instVars: u12, arraySize: ?usize, comptime T: anytype) AllocErrors!HeapObjectPtr {
        const ar = self.alloc(classIndex, instVars, arraySize, T, T == WeakObject);
        return ar.initAll();
    }
    pub fn alloc(self: SelfPtr, classIndex: ClassIndex, iVars: u12, indexed: ?usize, comptime element: type, makeWeak: bool) heap.AllocResult {
        const aI = AllocationInfo.calc(iVars, indexed, element, makeWeak);
        while (true) {
            if (self.allocBlock(aI, classIndex)) |ptr|
                return .{
                    .age = .global,
                    .allocated = ptr,
                    .info = aI,
                };
            unreachable;
        }
    }
    fn allocBlock(self: SelfPtr, aI: AllocationInfo, classIndex: ClassIndex) ?HeapObjectPtr {
        self.mutex.lock();
        defer self.mutex.unlock();
        const words: usize = aI.objectHeapSize();
        for (bitsToRepresent(words + 1)..nFreeLists + 1) |index| {
            if (self.freeLists[index].getSlice()) |slice| {
                const heapPtr: *HeapObject = @ptrCast(slice.ptr);
                aI.initObjectStructure(heapPtr, classIndex, Age.global);
                self.putInFreeLists(slice.ptr, words + 1, slice.len);
                if (aI.externalSize() > 0) {
                    @panic("external allocation");
                }
                return heapPtr;
            }
        }
        return null;
    }
    fn sweep(self: *HeapAllocation) void {
        self.mutex.lock();
        defer self.mutex.unlock();
        const ptr = @as(HeapObjectArray, @ptrCast(&self.mem));
        const end = size;
        while (end > 0) {
            const head = ptr[end - 1];
            _ = head;
            unreachable;
        }
    }
};
comptime {
    std.debug.assert(@sizeOf(HeapAllocation) == heap_allocation_size);
}
test "check HeapAllocations" {
    const ee = std.testing.expectEqual;
    var ha = HeapAllocation.init();
    defer ha.deinit();
    try ee(ha.freeSpace(), HeapAllocation.size);
    //    try ee(ha.allocOfSize(.none, HeapHeader.maxLength + 2, null, Object), error.HeapFull);
    const alloc0 = try ha.allocOfSize(.none, 0, 60, u8);
    try ee(alloc0.header.length, 8);
    const alloc1 = try ha.allocOfSize(.none, 0, 50, Object);
    try ee(alloc1.header.length, 50);
    const alloc2 = try ha.allocOfSize(.none, 0, 127, Object);
    try ee(alloc2.header.length, 127);
    const alloc3 = try ha.allocOfSize(.none, 124, null, WeakObject);
    try ee(alloc3.header.length, 127);
    const alloc4 = try ha.allocOfSize(.none, 128, null, Object);
    try ee(alloc4.header.length, 128);
    const alloc5 = try ha.allocOfSize(.none, HeapHeader.maxLength, null, Object);
    try ee(alloc5.header.length, HeapHeader.maxLength);
    //print("\nallocs={any}",.{
    _ = .{
        alloc0,
        alloc1,
        alloc2,
        alloc3,
        alloc4,
        alloc5,
    };
    // });
    try ee(ha.freeSpace(), HeapAllocation.size - 9 - 51 - 128 - 128 - 129 - HeapHeader.maxLength - 1 - ((HeapHeader.maxLength - 1) & 1));
}
const FreeList = extern struct {
    const Self = @This();
    header: HeapHeader,
    list: FreeListPtr,
    const FreeListPtr = ?*FreeListElement;
    const FreeListElement = struct {
        header: HeapHeader,
        next: FreeListPtr,
        fn count(self: *FreeListElement) usize {
            var ptr: FreeListPtr = self;
            var size: usize = 0;
            while (ptr) |fle| {
                size += 1;
                ptr = fle.next;
            }
            return size;
        }
    };
    inline fn addToFree(self: *Self, ptr: HeapObjectArray) void {
        const fle: *FreeListElement = @ptrCast(ptr);
        fle.header = HeapHeader.freeHeader(self.header.length);
        var prev = self.list;
        while (true) {
            fle.next = prev;
            if (@cmpxchgWeak(FreeListPtr, &self.list, prev, fle, SeqCst, SeqCst)) |old| {
                prev = old;
            } else return;
        }
    }
    fn getSlice(self: *Self) ?[]HeapObject {
        const myList = &self.list;
        while (true) {
            if (myList.*) |fle| {
                const next = fle.next;
                if (@cmpxchgWeak(FreeListPtr, myList, fle, next, SeqCst, SeqCst)) |_|
                    continue;
                return @as(HeapObjectArray, @ptrCast(fle))[0 .. @as(usize, self.header.length) + 1];
            } else return null;
        }
    }
    fn init(comptime n: comptime_int) [n]FreeList {
        var initial_value: [n]FreeList = undefined;
        for (initial_value[0..], 0..) |*fl, index| {
            fl.header = HeapHeader.freeHeader(@truncate((@as(u16, 1) << @as(u4, @intCast(index))) - 1));
            fl.list = null;
        }
        return initial_value;
    }
    fn count(self: *const FreeList) usize {
        if (self.list) |fpe|
            return fpe.count();
        return 0;
    }
};
test "freeList structure" {
    const ee = std.testing.expectEqual;
    const fls = FreeList.init(13);
    try ee(fls[0].header.length, 0);
    try ee(fls[9].header.length, 511);
    try ee(fls.len, 13);
    try ee(nFreeLists, 13);
}

var heapAllocations: ?HeapAllocationPtr = null;

pub inline fn aHeapAllocator() HeapAllocationPtr {
    if (heapAllocations) |ptr|
        return ptr;
    return newHeapAllocation();
}
fn newHeapAllocation() HeapAllocationPtr {
    const ha = HeapAllocation.init();
    var prev = heapAllocations;
    while (true) {
        ha.nextHeap = prev;
        if (@cmpxchgWeak(@TypeOf(heapAllocations), &heapAllocations, prev, ha, SeqCst, SeqCst)) |old| {
            prev = old;
            continue;
        }
        return ha;
    }
}
fn allocatedSpace() usize {
    var sum: usize = 0;
    var ptr: ?HeapAllocationPtr = heapAllocations;
    while (ptr) |ha| {
        sum += ha.mem.len;
        ptr = ha.nextHeap;
    }
    return sum;
}
var unused: u64 = 0;
pub fn allocator() Allocator {
    return .{
        .ptr = &unused,
        .vtable = &vtable,
    };
}
const Allocator = std.mem.Allocator;
const vtable = Allocator.VTable{
    .alloc = allocForAllocator,
    .resize = Allocator.noResize,
    .free = freeForAllocator,
};
fn allocForAllocator(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
    _ = .{ ptr_align, ret_addr, ctx, len };
    const obj = rawAlloc(1, (len + @sizeOf(Object) - 1) / @sizeOf(Object), &heapAllocations, StructObject) catch return null;
    // ToDo: add obj to the allocatorKnown list for its page
    const array = obj.arrayAsSlice(u8) catch unreachable;
    for (array) |*ptr| ptr.* = undefined;
    return array.ptr;
}
fn freeForAllocator(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
    // const self = @ptrCast(*Self, @alignCast(ctx));
    _ = .{ buf, buf_align, ret_addr, ctx };
    // unreachable;
}
fn rawAlloc(instVars: u12, arraySize: usize, hint: *?HeapAllocationPtr, comptime T: anytype) AllocErrors!HeapObjectPtr {
    if (heapAllocations == null) _ = newHeapAllocation();
    const startingAllocation = hint.*;
    var workingAllocation = startingAllocation orelse unreachable;

    // ToDo        if (index==0) return GlobalArena.allocIndirect(self,sp,hp,context,heapSize,arraySize);
    while (true) {
        if (workingAllocation.allocOfSize(.none, instVars, arraySize, T)) |allocation| {
            if (workingAllocation != startingAllocation) {
                hint.* = workingAllocation; // ToDo: use xchg
            }
            return allocation;
        } else |err| {
            _ = err catch {};
            workingAllocation = workingAllocation.cycleNext();
            if (workingAllocation == startingAllocation) unreachable;
        }
    }
}
// test "check zig-compatible allocator" {
//     const ee = std.testing.expectEqual;
//     var alloc = allocator();
//     const alloc0 = try alloc.create([100]u64);
//     //    defer alloc.destroy(alloc0);
//     try ee(alloc0.len, 100);
// }
// fn allocIndirect(self: *Self, sp:[*]Object, fieldsSize: usize, arraySize: usize) AllocReturn {
//     const array = @ptrCast(HeapObjectPtr,std.heap.page_allocator.alloc(Object, arraySize) catch @panic("page allocator failed"));
//     var result = try GlobalArena.alloc(self,sp,hp,context,heapSize,0);
//     const offs = @ptrCast([*]u64,result.allocated)+heapSize-2;
//     offs[1] = @intFromPtr(array);
//     return result;
// }
fn collect() AllocErrors!void {
    @panic("incomplete");
}
pub fn promote(obj: Object) !Object {
    if (!obj.isHeapAllocated()) return obj;
    if (obj.header().age == Age.static) return obj;
    unreachable;
    //       @memcpy(@ptrCast([*]u8,result),@ptrCast([*]const u8,ptr),totalSize*8);
    //       return result.asObject();
}
inline fn boundaryCalc(space: []HeapObject) usize {
    const po2: usize = smallerPowerOf2(space.len);
    const mask = @as(usize, @bitCast(-@as(isize, @intCast(po2 * @sizeOf(HeapObject)))));
    const alignedLen = ((@intFromPtr(space.ptr + space.len) & mask) - @intFromPtr(space.ptr)) / @sizeOf(HeapObject);
    return alignedLen;
}
// fn freeToList(self: *Self, space: []HeapObject) void {
//     const alignedLen = boundaryCalc(space);
//     if (alignedLen<space.len) self.freeToList(space[alignedLen..]);
//     var free = space[0..alignedLen];
//     while (free.len>0) {
//         const len = smallerPowerOf2(free.len);
//         const end = free.len - len;
//         FreeList.addToFree(self,@intCast(u12,len),@ptrCast(HeapObjectPtr,free.ptr+end));
//         free = free[0..end];
//     }
// }
// fn freeSpace(self: *Self) usize {
//     var sum: usize = 0;
//     for (self.freeLists) |fl| {
//         sum += fl.freeSpace();
//     }
//     return sum;
// }
// fn freeOfSize(self: *Self, size: u16) usize {
//     return self.freeLists[bitsToRepresent(size-1)].freeSpace();
// }
// test "check GlobalArena alloc object" {
//     const ee = std.testing.expectEqual;
//     const err = std.testing.expectError;
//     var ga = GlobalArena.init();
//     defer ga.deinit();
//     var o1 = ga.allocObject(17,5);
//     try ee(ga.allocatedSpace(),heapAllocationSize);
//     try ee(o1.inHeapSize(),6);
//     try ee(ga.freeSpace(),heapAllocationSize-6);
//     try err(error.NotIndexable,o1.size());
// }
// test "check alloc array" {
//     const ee = std.testing.expectEqual;
// //    const err = std.testing.expectError;
//     const allocSize = (HeapObject.maxLength>>1)-1;
//     var ga = GlobalArena.init();
//     defer ga.deinit();
//     var o1 = ga.allocArray(17,allocSize,u64);
//     try ee(ga.allocatedSpace(),heapAllocationSize);
//     try ee(o1.inHeapSize(),allocSize+1);
//     const a1 = o1.arrayAsSlice(u64);
//     try ee(a1.len,allocSize);
//     try ee(ga.freeSpace(),heapAllocationSize-(allocSize+1));
//     var o2 = ga.allocArray(42,allocSize,u64);
//     try ee(ga.allocatedSpace(),heapAllocationSize);
//     try ee(o2.inHeapSize(),allocSize+1);
//     const a2 = o2.arrayAsSlice(u64);
//     try ee(a2.len,allocSize);
//     try ee(ga.freeSpace(),heapAllocationSize-(allocSize+1)*2);
//     // var context = Context.init();
//     // var result = ga.asSelf().allocArray(([0]Object{})[0..],([0]HeapObject{})[0..],&context,19,42,0,u8) catch @panic("allocArray failed");
// //    try err(error.NotIndexable,result.allocated.asObject().size());
// }
// test "findAllocationList" {
//     const ee = std.testing.expectEqual;
//     var ga = GlobalArena.init();
//     defer ga.deinit();
//     try ee(ga.findAllocationList(1),1);
//     try ee(ga.findAllocationList(2),1);
//     try ee(ga.findAllocationList(3),2);
//     try ee(ga.findAllocationList(4),2);
//     try ee(ga.findAllocationList(17),5);
//     try ee(ga.findAllocationList(HeapObject.maxLength),GlobalArena.nFreeLists);
//     try ee(ga.findAllocationList(HeapObject.maxLength+1),0);
// }
// test "allocStruct" {
//     const Test_S = extern struct {
//         header: HeapObject,
//         superclass: Object,
//         methodDict: Object,
//         format: Object,
//     };
//     const ee = std.testing.expectEqual;
//     var ga = GlobalArena.init();
//     defer ga.deinit();
//     var test1 = ga.allocStruct(17,Test_S,0,Object);
//     try ee(test1.superclass,Nil);
//     const h1: HeapObjectPtr = &test1.header;
//     try ee(h1.inHeapSize(),4);
//     try std.testing.expectError(error.NotIndexable,h1.arrayAsSlice(u8));
//     var test2 = ga.allocStruct(17,Test_S,8,u8);
//     try ee(test2.methodDict,Nil);
//     const h2: HeapObjectPtr = &test2.header;
//     try ee(h2.inHeapSize(),7);
//     const a2: []u8 = try h2.arrayAsSlice(u8);
//     try ee(a2.len,8);
// }
// test "check alloc indirect" {
//     const ee = std.testing.expectEqual;
//     const err = std.testing.expectError;
//     var ga = GlobalArena.init();
//     defer ga.deinit();
//     var context = Context.init();
//     var o1 = ga.allocObject(17,5);
//     try ee(ga.allocatedSpace(),heapAllocationSize);
//     try ee(o1.inHeapSize(),6);
//     try ee(ga.freeSpace(),heapAllocationSize-6);
//     try err(error.NotIndexable,o1.size());
//     const maxValid = HeapObject.maxLength-45;
//     const result2 = ga.asSelf().allocArray(([0]Object{})[0..],([0]HeapObject{})[0..],&context,19,42,maxValid*8-1,u8) catch @panic("allocArray failed");
//     const h2 = result2.allocated;
//     try ee(h2.isIndirect(),false);
//     try ee(h2.inHeapSize(),HeapObject.maxLength);
//     try ee(h2.arraySize(),maxValid*8-1);
//     const result3 = ga.asSelf().allocArray(([0]Object{})[0..],([0]HeapObject{})[0..],&context,19,42,maxValid*8+1,u8) catch @panic("allocArray failed");
//     const h3 = result3.allocated;
//     try ee(h3.isIndirect(),true);
//     try ee(h3.inHeapSize(),45);
//     try ee(h3.arraySize(),maxValid*8+1);
//     const maxVArray = HeapObject.maxLength-1;
//     const result4 = ga.asSelf().allocArray(([0]Object{})[0..],([0]HeapObject{})[0..],&context,19,0,maxVArray*8-1,u8) catch @panic("allocArray failed");
//     const h4 = result4.allocated;
//     try ee(h4.isIndirect(),false);
//     try ee(h4.inHeapSize(),HeapObject.maxLength);
//     try ee(h4.arraySize(),maxVArray*8-1);
//     const result5 = ga.asSelf().allocArray(([0]Object{})[0..],([0]HeapObject{})[0..],&context,19,0,maxVArray*8+1,u8) catch @panic("allocArray failed");
//     const h5 = result5.allocated;
//     try ee(h5.isIndirect(),true);
//     try ee(h5.inHeapSize(),3);
//     try ee(h5.arraySize(),maxVArray*8+1);
// }

/// TODO replace callsites with `@log2` after this proposal is implemented:
/// https://github.com/ziglang/zig/issues/13642
inline fn log2a(x: anytype) switch (@typeInfo(@TypeOf(x))) {
    .Int => math.Log2Int(@TypeOf(x)),
    .ComptimeInt => comptime_int,
    else => @compileError("int please"),
} {
    switch (@typeInfo(@TypeOf(x))) {
        .Int => return math.log2_int(@TypeOf(x), x),
        .ComptimeInt => return math.log2(x),
        else => @compileError("bad"),
    }
}
