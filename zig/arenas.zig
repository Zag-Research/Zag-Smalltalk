const std = @import("std");
const builtin = @import("builtin");
const SeqCst = std.builtin.AtomicOrder.SeqCst;
const mem = std.mem;
const checkEqual = @import("utilities.zig").checkEqual;
const bitsToRepresent = @import("utilities.zig").bitsToRepresent;
const smallerPowerOf2 = @import("utilities.zig").smallerPowerOf2;
const largerPowerOf2 = @import("utilities.zig").largerPowerOf2;
const largerPowerOf2Not1 = @import("utilities.zig").largerPowerOf2Not1;
const object = @import("object.zig");
const Object = object.Object;
const objectWidth = @sizeOf(Object);
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const class = @import("class.zig");
const ClassIndex = class.ClassIndex;
const Header = @import("heap.zig").Header;
const header = @import("heap.zig").header;
const Format = @import("heap.zig").Format;
const Age = @import("heap.zig").Age;
const HeaderArray = @import("heap.zig").HeaderArray;
const HeapPtr = @import("heap.zig").HeapPtr;
const AllocErrors = @import("heap.zig").AllocErrors;
const Context = @import("context.zig").Context;
const ContextPtr = @import("context.zig").ContextPtr;
const os = @import("os.zig");
const blockAllocation = os.BlockAllocation(HeapAllocation).new();
pub inline fn arenaFree(stackPointer: [*]const Object, heapPointer: HeaderArray) isize {
    return @divFloor(@as(isize, @bitCast((@intFromPtr(stackPointer) -% @intFromPtr(heapPointer)))), @sizeOf(Object));
}
test "arenaFree" {
    _ = try blockAllocation;
    const testing = std.testing;
    const stack: [10]Object align(8) = undefined;
    const s1: [*]const Object = @as([*]const Object, @ptrCast(&stack[1]));
    const s5 = s1 + 4;
    const hp: HeaderArray = Header.fromObjectPtr(s1 + 2);
    try testing.expectEqual(arenaFree(s5, hp), 2);
    try testing.expectEqual(arenaFree(s1, hp), -2);
}
const AllocResult = struct {
    sp: [*]Object,
    hp: HeaderArray,
    context: ContextPtr,
    age: Age,
    allocated: HeapPtr,
};
const AllocReturn = AllocErrors!AllocResult;

pub var globalArena = GlobalArena.init();
pub const heapAllocationSize = GlobalArena.HeapAllocation.size;
var junkContext = Context.init();
pub const GlobalArena = struct {
    const Self = @This();
    heapAllocations: ?*HeapAllocation,
    freeLists: [nFreeLists]FreeList,
    minFreeList: u7,
    minAllocation: u16,
    const nFreeLists = bitsToRepresent(Header.maxLength);
    const allocationUnit = Header.maxLength; // size in u64 units including the header
    pub fn init() Self {
        const minFreeList = 1;
        return Self{
            .heapAllocations = null,
            .freeLists = FreeList.init(nFreeLists),
            .minFreeList = minFreeList,
            .minAllocation = 1 << minFreeList,
        };
    }
    pub fn deinit(self: *Self) void {
        if (self.heapAllocations) |ha| ha.freeAll();
    }
    const Allocator = std.mem.Allocator;
    const vtable = Allocator{
        .alloc = allocForAllocator,
        .resize = Allocator.noResize,
        .free = freeForAllocator,
    };
    pub fn allocator(self: *Self) Allocator {
        return .{
            .ptr = self,
            .vtable = vtable,
        };
    }
    fn allocForAllocator(ctx: *anyopaque, len: usize, ptr_align: u8, ret_addr: usize) ?[*]u8 {
        const self = @as(*Self, @ptrCast(@alignCast(@alignOf(Self), ctx)));
        _ = .{ ptr_align, ret_addr };
        const obj = self.rawAlloc(0, len + @sizeOf(*Allocator)) catch return null;
        // ToDo: add obj to the allocatorKnown list for its page
        const array = obj.arrayAsSlice(u8);
        for (array) |*ptr| ptr.* = undefined;
        return array.ptr;
    }
    fn freeForAllocator(ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
        const self = @as(*Self, @ptrCast(@alignCast(@alignOf(Self), ctx)));
        _ = .{ self, buf, buf_align, ret_addr, @panic("freeForAllocator unimplemented") };
    }
    fn allocIndirect(self: *Self, sp: [*]Object, hp: HeaderArray, context: ContextPtr, heapSize: usize, arraySize: usize) AllocReturn {
        const array = @as(HeapPtr, @ptrCast(std.heap.page_allocator.alloc(Object, arraySize) catch @panic("page allocator failed")));
        var result = try GlobalArena.alloc(self, sp, hp, context, heapSize, 0);
        const offs = @as([*]u64, @ptrCast(result.allocated)) + heapSize - 2;
        offs[1] = @intFromPtr(array);
        return result;
    }
    fn alloc(self: *Self, sp: [*]Object, hp: HeaderArray, context: ContextPtr, heapSize: usize, arraySize: usize) AllocReturn {
        const allocation = try self.rawAlloc(heapSize, arraySize);
        return .{
            .sp = sp,
            .hp = hp,
            .context = context,
            .age = Age.global,
            .allocated = allocation,
        };
    }
    fn rawAlloc(self: *Self, heapSize: usize, arraySize: usize) AllocErrors!HeapPtr {
        const totalSize = heapSize + arraySize;
        var index = self.findAllocationList(totalSize);
        // ToDo        if (index==0) return GlobalArena.allocIndirect(self,sp,hp,context,heapSize,arraySize);
        const allocation: []Header = (while (index < self.freeLists.len) : (index += 1) {
            if (self.freeLists[index].getSlice()) |slice| break slice;
        } else GlobalArena.HeapAllocation.allocSlice(self));
        self.freeToList(allocation[totalSize..]);
        if (arraySize > 0) {
            const offs = @as([*]u64, @ptrCast(allocation.ptr)) + heapSize - 2;
            offs[1] = @intFromPtr(offs + 2);
        }
        return @as(HeapPtr, @ptrCast(allocation.ptr));
    }
    fn collect(self: *Self, sp: [*]Object, hp: HeaderArray, context: ContextPtr) AllocErrors!void {
        _ = self;
        _ = sp;
        _ = hp;
        _ = context;
        @panic("incomplete");
    }
    pub fn promote(obj: Object) !Object {
        if (!obj.isHeapAllocated()) return obj;
        if (obj.header().age == Age.static) return obj;
        unreachable;
        //       @memcpy(@ptrCast([*]u8,result),@ptrCast([*]const u8,ptr),totalSize*8);
        //       return result.asObject();
    }
    fn findAllocationList(self: *Self, target: usize) u7 {
        if (target > Header.maxLength) return 0;
        if (target < self.minAllocation) return self.minFreeList;
        return bitsToRepresent(target - 1);
    }
    inline fn boundaryCalc(space: []Header) usize {
        const po2: usize = smallerPowerOf2(space.len);
        const mask = @as(usize, @bitCast(-@as(isize, @intCast(po2 * @sizeOf(Header)))));
        const alignedLen = ((@intFromPtr(space.ptr + space.len) & mask) - @intFromPtr(space.ptr)) / @sizeOf(Header);
        return alignedLen;
    }
    fn freeToList(self: *Self, space: []Header) void {
        const alignedLen = boundaryCalc(space);
        if (alignedLen < space.len) self.freeToList(space[alignedLen..]);
        var free = space[0..alignedLen];
        while (free.len > 0) {
            const len = smallerPowerOf2(free.len);
            const end = free.len - len;
            FreeList.addToFree(self, @as(u12, @intCast(len)), @as(HeapPtr, @ptrCast(free.ptr + end)));
            free = free[0..end];
        }
    }
    fn freeSpace(self: *Self) usize {
        var sum: usize = 0;
        for (self.freeLists) |fl| {
            sum += fl.freeSpace();
        }
        return sum;
    }
    fn freeOfSize(self: *Self, size: u16) usize {
        return self.freeLists[bitsToRepresent(size - 1)].freeSpace();
    }
    fn allocatedSpace(self: *Self) usize {
        var sum: usize = 0;
        var ptr: ?*HeapAllocation = self.heapAllocations;
        while (ptr) |ha| {
            sum += ha.mem.len;
            ptr = ha.next;
        }
        return sum;
    }
    pub inline fn allocObject(self: *Self, classIndex: ClassIndex, ivSize: usize) Object {
        var result = self.asSelf().allocObject(([0]Object{})[0..], ([0]Header{})[0..], &junkContext, classIndex, ivSize) catch @panic("allocObject failed");
        return result.allocated.asObject();
    }
    pub inline fn allocArray(self: *Self, classIndex: ClassIndex, arraySize: usize, comptime T: type) Object {
        var result = self.asSelf().allocArray(([0]Object{})[0..], ([0]Header{})[0..], &junkContext, classIndex, 0, arraySize, T) catch @panic("allocArray failed");
        return result.allocated.asObject();
    }
    pub inline fn allocStruct(self: *Self, classIndex: ClassIndex, comptime T: type, extra: usize, comptime T2: type) *T {
        var result = self.asSelf().allocStruct(([0]Object{})[0..], ([0]Header{})[0..], &junkContext, classIndex, T, extra, T2) catch @panic("allocStruct failed");
        return @as(*T, @ptrFromInt(@intFromPtr(result.allocated)));
    }
    const HeapAllocation = extern struct {
        flags: u64,
        next: ?*HeapAllocation,
        mem: [size]Header,
        const field_size = @sizeOf(u64) + @sizeOf(?*HeapAllocation);
        const heap_allocation_size = std.mem.page_size; //64*1024;
        const size = (heap_allocation_size - field_size) / @sizeOf(Header);
        const maxObjects = size / @sizeOf(Header);
        const returnType = []u8;
        fn getAligned() []align(heap_allocation_size) u8 { // ToDo: align larger size without wasting 1/2 the space
            //var buf = std.heap.page_allocator.alloc(u8, heap_allocation_size*2-std.mem.page_size) catch @panic("page allocator failed");
            //const base = @intFromPtr(buf.ptr) & (heap_allocation_size-1);
            //const offs = if (base==0) 0 else heap_allocation_size-base;
            //if (!std.heap.page_allocator.resize(buf,offs+heap_allocation_size)) @panic("resize failed");
            //return @alignCast(heap_allocation_size,buf[offs..offs+page_allocation_size]);
            return @alignCast(heap_allocation_size, std.heap.page_allocator.alloc(u8, heap_allocation_size) catch @panic("page allocator failed"));
        }
        fn allocSlice(self: *GlobalArena) []Header {
            var ha = HeapAllocation.alloc(self);
            return ha.mem[0..];
        }
        fn alloc(self_: *GlobalArena) *HeapAllocation {
            var space = getAligned();
            const self = @as(*HeapAllocation, @ptrCast(space.ptr));
            self.flags = 0;
            var prev = self.heapAllocations;
            while (true) {
                self.next = prev;
                if (@cmpxchgWeak(?*HeapAllocation, &self.heapAllocations, prev, self, SeqCst, SeqCst)) |old| {
                    prev = old;
                    continue;
                }
                return self;
            }
        }
        fn freeAll(self: *HeapAllocation) void {
            var ptr: ?*HeapAllocation = self;
            while (ptr) |ha| {
                ptr = ha.next;
                ha.free();
            }
        }
        fn free(self: *HeapAllocation) void {
            std.heap.page_allocator.free(@as([*]u8, @ptrCast(self))[0..heap_allocation_size]);
        }
        fn sweep(self: *HeapAllocation) void {
            var ptr = @as(HeaderArray, @ptrCast(&self.mem));
            const end = ptr + maxObjects;
            while (ptr < end) {
                unreachable;
            }
        }
    };
    const FreeList = struct {
        size: u16,
        list: FreeListPtr,
        inline fn addToFree(ga: *GlobalSelf, len: u12, ptr: HeapPtr) void {
            ptr.* = header(len, Format.none, 0, 0, Age.free);
            if (len >= ga.minAllocation) {
                const self = @as(*FreeListElement, @ptrCast(ptr));
                const freeListIndex = ga.findAllocationList(len);
                if (freeListIndex > 0) {
                    var myList = &ga.freeLists[freeListIndex].list;
                    var prev = myList.*;
                    while (true) {
                        self.next = prev;
                        if (@cmpxchgWeak(FreeListPtr, myList, prev, self, SeqCst, SeqCst)) |old| {
                            prev = old;
                        } else break;
                    }
                }
            }
        }
        fn getSlice(self: *FreeList) ?[]Header {
            var myList = &self.list;
            var prev = myList.*;
            while (true) {
                if (prev) |fle| {
                    const next = fle.next;
                    if (@cmpxchgWeak(FreeListPtr, myList, prev, next, SeqCst, SeqCst)) |old| {
                        prev = old;
                    } else return @as(HeaderArray, @ptrCast(fle))[0..self.size];
                } else return null;
            }
        }
        fn init(comptime n: comptime_int) [n]FreeList {
            var initial_value: [n]FreeList = undefined;
            for (initial_value[0..], 0..) |*fl, index| {
                fl.size = @as(u16, 1) << @as(u4, @intCast(index));
                fl.list = null;
            }
            return initial_value;
        }
        fn freeSpace(self: *const FreeList) usize {
            if (self.list) |fpe| return self.size * fpe.count();
            return 0;
        }
    };
    const FreeListPtr = ?*FreeListElement;
    const FreeListElement = struct {
        header: Header,
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
};
test "freeList structure" {
    const ee = std.testing.expectEqual;
    const fls = GlobalSelf.FreeList.init(12);
    try ee(fls[0].size, 1);
    try ee(fls[9].size, 512);
    try ee(GlobalSelf.nFreeLists, switch (std.mem.page_size) {
        4096 => 9,
        16384 => 11,
        else => std.mem.page_size,
    });
}
test "check HeapAllocations" {
    const ee = std.testing.expectEqual;
    var ga = GlobalSelf.init();
    defer ga.deinit();
    var ha = GlobalSelf.HeapAllocation.alloc(&ga);
    try ee(ga.allocatedSpace(), heapAllocationSize);
    try ee(ga.freeSpace(), 0);
    try ee(GlobalSelf.boundaryCalc(ha.mem[0..14]), 14);
    try ee(GlobalSelf.boundaryCalc(ha.mem[14..31]), 16);
    try ee(GlobalSelf.boundaryCalc(ha.mem[14..30]), 16);
    try ee(GlobalSelf.boundaryCalc(ha.mem[62..75]), 8);
    try ee(GlobalSelf.boundaryCalc(ha.mem[126..158]), 32);
    try ee(GlobalSelf.boundaryCalc(ha.mem[0..]), ha.mem.len);
    ga.freeToList(ha.mem[0..14]);
    try ee(ga.freeOfSize(8), 8);
    try ee(ga.freeOfSize(4), 4);
    try ee(ga.freeOfSize(2), 2);
    ga.freeToList(ha.mem[14..45]);
    try ee(ga.freeOfSize(16), 16);
    try ee(ga.freeOfSize(8), 16);
    try ee(ga.freeOfSize(4), 8);
    try ee(ga.freeOfSize(2), 4);
    try ee(ga.freeOfSize(1), 0);
    ga.freeToList(ha.mem[45..]);
    try ee(ga.freeSpace(), heapAllocationSize - 2); // ignored the 2x 1-word allocations
}
test "check GlobalSelf alloc object" {
    const ee = std.testing.expectEqual;
    const err = std.testing.expectError;
    var ga = GlobalSelf.init();
    defer ga.deinit();
    var o1 = ga.allocObject(17, 5);
    try ee(ga.allocatedSpace(), heapAllocationSize);
    try ee(o1.inHeapSize(), 6);
    try ee(ga.freeSpace(), heapAllocationSize - 6);
    try err(error.NotIndexable, o1.size());
}
test "check alloc array" {
    const ee = std.testing.expectEqual;
    const err = std.testing.expectError;
    const allocSize = (Header.maxLength >> 1) - 1;
    var ga = GlobalSelf.init();
    defer ga.deinit();
    var o1 = ga.allocArray(17, allocSize, u64);
    try ee(ga.allocatedSpace(), heapAllocationSize);
    try ee(o1.inHeapSize(), allocSize + 1);
    const a1 = o1.arrayAsSlice(u64);
    try ee(a1.len, allocSize);
    try ee(ga.freeSpace(), heapAllocationSize - (allocSize + 1));
    var o2 = ga.allocArray(42, allocSize, u64);
    try ee(ga.allocatedSpace(), heapAllocationSize);
    try ee(o2.inHeapSize(), allocSize + 1);
    const a2 = o2.arrayAsSlice(u64);
    try ee(a2.len, allocSize);
    try ee(ga.freeSpace(), heapAllocationSize - (allocSize + 1) * 2);
    var context = Context.init();
    var result = ga.asSelf().allocArray(([0]Object{})[0..], ([0]Header{})[0..], &context, 19, 42, 0, u8) catch @panic("allocArray failed");
    try err(error.NotIndexable, result.allocated.asObject().size());
}
test "findAllocationList" {
    const ee = std.testing.expectEqual;
    var ga = GlobalSelf.init();
    defer ga.deinit();
    try ee(ga.findAllocationList(1), 1);
    try ee(ga.findAllocationList(2), 1);
    try ee(ga.findAllocationList(3), 2);
    try ee(ga.findAllocationList(4), 2);
    try ee(ga.findAllocationList(17), 5);
    try ee(ga.findAllocationList(Header.maxLength), GlobalSelf.nFreeLists);
    try ee(ga.findAllocationList(Header.maxLength + 1), 0);
}
test "allocStruct" {
    const Test_S = extern struct {
        header: Header,
        superclass: Object,
        methodDict: Object,
        format: Object,
    };
    const ee = std.testing.expectEqual;
    var ga = GlobalSelf.init();
    defer ga.deinit();
    var test1 = ga.allocStruct(17, Test_S, 0, Object);
    try ee(test1.superclass, Nil);
    const h1: HeapPtr = &test1.header;
    try ee(h1.inHeapSize(), 4);
    try std.testing.expectError(error.NotIndexable, h1.arrayAsSlice(u8));
    var test2 = ga.allocStruct(17, Test_S, 8, u8);
    try ee(test2.methodDict, Nil);
    const h2: HeapPtr = &test2.header;
    try ee(h2.inHeapSize(), 7);
    const a2: []u8 = try h2.arrayAsSlice(u8);
    try ee(a2.len, 8);
}
test "check alloc indirect" {
    const ee = std.testing.expectEqual;
    const err = std.testing.expectError;
    var ga = GlobalSelf.init();
    defer ga.deinit();
    var context = Context.init();
    var o1 = ga.allocObject(17, 5);
    try ee(ga.allocatedSpace(), heapAllocationSize);
    try ee(o1.inHeapSize(), 6);
    try ee(ga.freeSpace(), heapAllocationSize - 6);
    try err(error.NotIndexable, o1.size());
    const maxValid = Header.maxLength - 45;
    const result2 = ga.asSelf().allocArray(([0]Object{})[0..], ([0]Header{})[0..], &context, 19, 42, maxValid * 8 - 1, u8) catch @panic("allocArray failed");
    const h2 = result2.allocated;
    try ee(h2.isIndirect(), false);
    try ee(h2.inHeapSize(), Header.maxLength);
    try ee(h2.arraySize(), maxValid * 8 - 1);
    const result3 = ga.asSelf().allocArray(([0]Object{})[0..], ([0]Header{})[0..], &context, 19, 42, maxValid * 8 + 1, u8) catch @panic("allocArray failed");
    const h3 = result3.allocated;
    try ee(h3.isIndirect(), true);
    try ee(h3.inHeapSize(), 45);
    try ee(h3.arraySize(), maxValid * 8 + 1);
    const maxVArray = Header.maxLength - 1;
    const result4 = ga.asSelf().allocArray(([0]Object{})[0..], ([0]Header{})[0..], &context, 19, 0, maxVArray * 8 - 1, u8) catch @panic("allocArray failed");
    const h4 = result4.allocated;
    try ee(h4.isIndirect(), false);
    try ee(h4.inHeapSize(), Header.maxLength);
    try ee(h4.arraySize(), maxVArray * 8 - 1);
    const result5 = ga.asSelf().allocArray(([0]Object{})[0..], ([0]Header{})[0..], &context, 19, 0, maxVArray * 8 + 1, u8) catch @panic("allocArray failed");
    const h5 = result5.allocated;
    try ee(h5.isIndirect(), true);
    try ee(h5.inHeapSize(), 3);
    try ee(h5.arraySize(), maxVArray * 8 + 1);
}
