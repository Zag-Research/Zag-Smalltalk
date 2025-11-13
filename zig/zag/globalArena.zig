const std = @import("std");
const print = std.log.err;
const math = std.math;
const builtin = @import("builtin");
const SeqCst = std.builtin.AtomicOrder.seq_cst;
const mem = std.mem;
const zag = @import("zag.zig");
const config = zag.config;
const utilities = zag.utilities;
const checkEqual = utilities.checkEqual;
const bitsToRepresent = utilities.bitsToRepresent;
const smallerPowerOf2 = utilities.smallerPowerOf2;
const largerPowerOf2 = utilities.largerPowerOf2;
const object = zag.object;
const Object = object.Object;
const objectWidth = @sizeOf(Object);
const ClassIndex = object.ClassIndex;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const heap = zag.heap;
const HeapObject = heap.HeapObject;
const HeapHeader = heap.HeapHeader;
const Format = heap.Format;
const AllocationInfo = heap.AllocationInfo;
const Age = heap.Age;
const HeapObjectArray = heap.HeapObjectArray;
const HeapObjectPtr = heap.HeapObjectPtr;
const AllocErrors = heap.AllocErrors;
const os = @import("os.zig");

pub const WeakObject = struct {
    object: Object,
};
pub const StructObject = struct {
    object: Object,
};
const nFreeLists: usize = bitsToRepresent(HeapHeader.maxLength + @as(usize, 1)) + 1;
const heap_allocation_size = 128 * 1024;
const heapStartAddress = 0x10000000000;
pub const HeapAllocationPtr = *align(heap_allocation_size) HeapAllocation;
comptime {
    std.testing.expectEqual(13, nFreeLists) catch @panic("unreachable");
    std.testing.expectEqual(0x4000, HeapAllocation.size) catch @panic("unreachable");
    std.testing.expectEqual(heap_allocation_size, HeapAllocation.size * 8) catch @panic("unreachable");
    std.debug.assert(HeapAllocation.headerSize >= @sizeOf(HeapAllocation.HeapAllocationHeader) / @sizeOf(HeapObject));
}
pub const HeapAllocation = extern union {
    mem: [size]HeapObject,
    header: HeapAllocationHeader,
    const SelfPtr = *align(heap_allocation_size) HeapAllocation;
    const HeapAllocationHeader = extern struct {
        loadAddress: *align(heap_allocation_size) HeapAllocation, // address that corresponds with the filename
        nextHeap: ?*align(heap_allocation_size) HeapAllocation, // link to next heap header
        freeLists: [nFreeLists]FreeList,
        allocators: u16,
        objectsNeedingScan: u16,
        marking: bool,
        mutex: MutexType = mutex_init,
    };
    const size: usize = heap_allocation_size / @sizeOf(HeapObject);
    const headerSize = 32;
    const minFreeList = 1;
    const mutex_init = MutexType{};
    const MutexType = DummyMutex;
    const DummyMutex = packed struct {
        lockField: i64 = -1,
        fn lock(_: *DummyMutex) void {}
        fn unlock(_: *DummyMutex) void {}
    };
    var memoryAllocator = @import("os.zig").MemoryAllocator(HeapAllocation).new(heapStartAddress);
    fn getIndex(ptr: [*]HeapObject) usize {
        return (@intFromPtr(ptr) & (heap_allocation_size - 1)) / @sizeOf(HeapObject);
    }
    fn init() SelfPtr {
        var self = memoryAllocator.allocBlock() catch @panic("page allocator failed");
        self.initHeader();
        self.putInFreeLists(headerSize, size);
        return self;
    }
    fn initHeader(self: SelfPtr) void {
        self.header.allocators = 0;
        self.header.objectsNeedingScan = 0;
        self.header.marking = false;
        self.header.nextHeap = null;
        self.header.freeLists = FreeList.init(nFreeLists);
    }
    pub fn loadHeap(file: std.fs.File, address: usize) !void {
        var self = memoryAllocator.allocBlockAtAddress(address) catch @panic("page allocator failed");
        const fSize = try file.read(@as([*]u8, @ptrCast(self))[0..@sizeOf(HeapAllocation)]);
        self.initHeader();
        self.putInFreeLists(fSize, size);
        return;
    }
    pub fn loadLargeHeapObject(file: std.fs.File, address: usize, fSize: usize) !SelfPtr {
        _ = .{ file, address, fSize, unreachable };
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
        var temp = self.header.nextHeap;
        if (temp == null) temp = heapAllocations;
        // ToDo: update number of users of self and temp
        return temp orelse @panic("unreachable");
    }
    fn putInFreeLists(self: SelfPtr, from: usize, to: usize) void {
        var start = from;
        const ptr: [*]HeapHeader = @ptrCast(&self.mem);
        var freeIndex: usize = 0;
        var freeBit: usize = 1;
        while (start + freeBit < to) {
            if (start & freeBit != 0) {
                self.header.freeLists[freeIndex].addToFree(ptr + start);
                start += freeBit;
            }
            if (freeIndex == nFreeLists - 1) break;
            freeBit <<= 1;
            freeIndex += 1;
        }
        while (start < to) {
            if (start + freeBit > to) {
                if (freeIndex == 0) break;
                freeBit >>= 1;
                freeIndex -= 1;
            } else {
                self.header.freeLists[freeIndex].addToFree(ptr + start);
                start += freeBit;
            }
        }
    }
    fn freeCount(self: SelfPtr, freeList: usize) usize {
        return self.header.freeLists[freeList].freeCount();
    }
    fn freeSpace(self: SelfPtr) usize {
        var free: usize = 0;
        for (self.header.freeLists[0..]) |fl| {
            free += fl.freeCount() * (@as(usize, fl.header.length) + 1);
        }
        return free;
    }
    fn allocLarge(self: SelfPtr, arraySize: ?usize, aI: AllocationInfo) !HeapObjectPtr {
        _ = self;
        _ = arraySize;
        _ = aI;
        return error.HeapFull;
    }
    fn allocOfSize(self: SelfPtr, classIndex: ClassIndex, instVars: u11, arraySize: ?usize, comptime T: anytype) AllocErrors!HeapObjectPtr {
        const ar = self.alloc(classIndex, instVars, arraySize, T, T == WeakObject);
        const result = ar.initAll();
        return result;
    }
    pub fn alloc(self: SelfPtr, classIndex: ClassIndex, iVars: u11, indexed: ?usize, comptime element: type, makeWeak: bool) heap.AllocResult {
        const aI = AllocationInfo.calc(iVars, indexed, element, makeWeak);
        while (true) {
            if (self.allocBlock(aI, classIndex)) |ptr|
                return .{
                    .age = .global,
                    .allocated = ptr,
                    .info = aI,
                };
            @panic("unreachable"); // add a new HeapAllocation
        }
    }
    fn allocBlock(self: SelfPtr, aI: AllocationInfo, classIndex: ClassIndex) ?HeapObjectPtr {
        self.header.mutex.lock();
        defer self.header.mutex.unlock();
        const words: usize = aI.objectHeapSize() + 1;
        for (bitsToRepresent(words)..nFreeLists + 1) |index| {
            if (self.header.freeLists[index].getSlice()) |slice| {
                const heapPtr: *HeapObject = @ptrCast(slice.ptr);
                aI.initObjectStructure(heapPtr, classIndex, Age.global);
                const offset = getIndex(slice.ptr);
                self.putInFreeLists(offset + words, offset + slice.len);
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
            @panic("unreachable");
        }
    }
};
comptime {
    //    std.debug.assert(@sizeOf(HeapAllocation) == heap_allocation_size);
}
test "check HeapAllocations" {
    const ee = std.testing.expectEqual;
    const fullHeapSize = HeapAllocation.size - HeapAllocation.headerSize;
    var ha = HeapAllocation.init();
    defer ha.deinit();
    for ([_]u8{ 5, 6, 7, 8, 9, 10, 11 }) |index|
        try ee(1, ha.freeCount(index));
    try ee(3, ha.freeCount(12));
    if (true) return error.SkipZigTest;
    try ee(fullHeapSize, ha.freeSpace());
    //    try ee(ha.allocOfSize(.none, HeapHeader.maxLength + 2, null, Object), error.HeapFull);
    const alloc0 = try ha.allocOfSize(.none, 0, 60, u8);
    try ee(alloc0.header.length, 8);
    try ee(fullHeapSize - 9, ha.freeSpace());
    const alloc1 = try ha.allocOfSize(.none, 0, 50, Object);
    try ee(alloc1.header.length, 50);
    const alloc2 = try ha.allocOfSize(.none, 0, 127, Object);
    try ee(alloc2.header.length, 127);
    const alloc3 = try ha.allocOfSize(.none, 124, null, WeakObject);
    try ee(alloc3.header.length, 127);
    const alloc4 = try ha.allocOfSize(.none, 128, null, Object);
    try ee(alloc4.header.length, 128);
    try ee(fullHeapSize - 9 - 51 - 128 - 128 - 129, ha.freeSpace());
    const big = HeapHeader.maxLength;
    const alloc5 = try ha.allocOfSize(.none, big, null, Object);
    try ee(alloc5.header.length, big);
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
    try ee(fullHeapSize - 9 - 51 - 128 - 128 - 129 - big - 1, ha.freeSpace());
}
const FreeList = extern struct {
    header: HeapHeader,
    list: FreeListPtr,
    const Self = @This();
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
    inline fn addToFree(self: *Self, ptr: anytype) void {
        const fle: *FreeListElement = @ptrCast(ptr);
        fle.header = self.header;
        if (self.header.length == 0) {
            HeapHeader.storeFreeHeader(@ptrFromInt(@intFromPtr(ptr)));
            @setRuntimeSafety(false);
            self.list = @ptrFromInt(@intFromPtr(self.list) + 1);
            return;
        }
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
    fn freeCount(self: *const FreeList) usize {
        if (self.header.length == 0) return @intFromPtr(self.list);
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
        ha.header.nextHeap = prev;
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
    .remap = undefined,
};
fn allocForAllocator(ctx: *anyopaque, len: usize, ptr_align: mem.Alignment, ret_addr: usize) ?[*]u8 {
    _ = .{ ptr_align, ret_addr, ctx, len };
    const obj = rawAlloc(1, (len + @sizeOf(Object) - 1) / @sizeOf(Object), &heapAllocations, StructObject) catch return null;
    // ToDo: add obj to the allocatorKnown list for its page
    const array = obj.arrayAsSlice(u8) catch @panic("unreachable");
    for (array) |*ptr| ptr.* = undefined;
    return array.ptr;
}
fn freeForAllocator(ctx: *anyopaque, buf: []u8, buf_align: mem.Alignment, ret_addr: usize) void {
    // const self = @ptrCast(*Self, @alignCast(ctx));
    _ = .{ buf, buf_align, ret_addr, ctx };
    // @panic("unreachable");
}
fn rawAlloc(instVars: u11, arraySize: usize, hint: *?HeapAllocationPtr, comptime T: anytype) AllocErrors!HeapObjectPtr {
    if (heapAllocations == null) _ = newHeapAllocation();
    const startingAllocation = hint.*;
    var workingAllocation = startingAllocation orelse @panic("unreachable");

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
            if (workingAllocation == startingAllocation) @panic("unreachable");
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
// fn allocIndirect(self: *Self, sp:[*]Object, fieldsSize: usize, arraySize: usize) AllocResult {
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
    if (!obj.isMemoryAllocated()) return obj;
    if (obj.header().age == Age.static) return obj;
    @panic("unreachable");
    //       @memcpy(@ptrCast([*]u8,result),@ptrCast([*]const u8,ptr),totalSize*8);
    //       return result.asObject();
}
// inline fn boundaryCalc(space: []HeapObject) usize {
//     const po2: usize = smallerPowerOf2(space.len);
//     const mask = @as(usize, @bitCast(-@as(isize, @intCast(po2 * @sizeOf(HeapObject)))));
//     const alignedLen = ((@intFromPtr(space.ptr + space.len) & mask) - @intFromPtr(space.ptr)) / @sizeOf(HeapObject);
//     return alignedLen;
// }
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
// fn freeOfSize(self: *Self, size: u16) usize {
//     return self.freeLists[bitsToRepresent(size-1)].freeCount();
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
