const std = @import("std");
const print = std.debug.print;
const math = std.math;
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
const ClassIndex = object.ClassIndex;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const Header = @import("heap.zig").Header;
const header = @import("heap.zig").header;
const Format = @import("heap.zig").Format;
const Age = @import("heap.zig").Age;
const HeaderArray = @import("heap.zig").HeaderArray;
const HeapPtr = @import("heap.zig").HeapPtr;
const AllocErrors = @import("heap.zig").AllocErrors;
const os = @import("os.zig");

const HeapFlags = extern union {
    int: u64,
    f: extern struct {
        allocators: u16,
        objectsNeedingScan: u16,
        marking: bool,
    }
};
const HeapAllocation = extern struct {
    const Self = @This();
    const nFreeLists = bitsToRepresent(Header.maxLength)+1;
    const field_size = @sizeOf(HeapFlags)+@sizeOf(?*HeapAllocation)+@sizeOf(FreeList)*@as(usize,nFreeLists);
    const heap_allocation_size = 128*1024;
    const size = (heap_allocation_size - field_size)/@sizeOf(Header);
    const minFreeList = 1;
    mem: [size]Header,
    freeLists: [nFreeLists]FreeList,
    flags: HeapFlags,
    next: ?*align(heap_allocation_size)HeapAllocation,
    var memoryAllocator = @import("os.zig").MemoryAllocator(HeapAllocation).new();
    fn getAligned() *align(heap_allocation_size)HeapAllocation {
        return @alignCast(heap_allocation_size,memoryAllocator.allocBlock() catch @panic("page allocator failed"));
    }
    fn alloc() *align(heap_allocation_size)Self {
        var self = getAligned();
        self.flags.int = 0;
        self.next = null;
        self.freeLists = FreeList.init(nFreeLists);
        print("\n&self=0x{x}",.{@ptrToInt(self)});
        self.putInFreeLists(@ptrCast([*]Header,&self.mem),0,size);
        return self;
    }
    fn freeAll(self: *align(heap_allocation_size)Self) void {
        var ptr: ?*align(heap_allocation_size)Self = self;
        while (ptr) |ha| {
            ptr = ha.next;
            ha.deinit();
        }
    }
    fn free(self: *align(heap_allocation_size)Self) void {
        memoryAllocator.unmap(@ptrCast([*]align(os.page_size)u8,self)[0..heap_allocation_size]);
    }
    fn putInFreeLists(self: *Self, ptr: [*]Header, from: usize, to: usize) void {
        if (to==from) return;
        const freeIndex = @min(nFreeLists-1,bitsToRepresent((to-from)>>1));
        const freeSize = @as(usize,1)<<@truncate(u6,freeIndex);
        const freeMask = freeSize-1;
        var start = (from+freeMask)&~freeMask;
        print("\nfreeSize={} freeIndex={} start={} from={} to={}",.{freeSize,freeIndex,start,from,to});
        if (from<start) self.putInFreeLists(ptr,from,start);
        while (start+freeSize<=to) {
            self.freeLists[freeIndex].addToFree(ptr+start);
            start += freeSize;
        }
        if (start<to) self.putInFreeLists(ptr,start,to);
    }
    fn freeSpace(self: *const Self) usize {
        var count: usize = 0;
        for (self.freeLists[0..]) |fl|
            count += fl.freeSpace();
        return count;
    }
    fn allocLarge(self: *Self, words: usize) !HeapPtr {
        _ = self; _ = words;
        return error.noSpace;
    }
    fn allocOfSize(self: *Self, words: usize) !HeapPtr {
        if (words>Header.maxLength) return self.allocLarge(words);
        var index = bitsToRepresent(words+1);
        while (index<nFreeLists) {
            if (self.freeLists[index].getSlice()) |slice| {
                const headPtr = &slice[words];
                headPtr.* = header(@truncate(u12,words),Format.notIndexable,0,0,Age.aStruct);
                self.putInFreeLists(slice.ptr,words+1,slice.len);
                return headPtr;
            }
            index += 1;
        }
        return error.noSpace;
    }
    fn sweep(self: *HeapAllocation) void {
        var ptr = @ptrCast(HeaderArray,&self.mem);
        const end = size;
        while (end>0) {
            const head = ptr[end-1];
            _ = head;
            unreachable;
        }
    }
};
test "size of HeapAllocation" {
    try std.testing.expectEqual(@sizeOf(HeapAllocation),HeapAllocation.heap_allocation_size);
}
test "check HeapAllocations" {
    const ee = std.testing.expectEqual;
    var ha = HeapAllocation.alloc();
    defer ha.free();
    try ee(ha.freeSpace(),HeapAllocation.size);
    try ee(ha.allocOfSize(Header.maxLength+1),error.noSpace);
    const alloc1 = try ha.allocOfSize(127);
    const alloc2 = try ha.allocOfSize(127);
    const alloc3 = try ha.allocOfSize(128);
    const alloc4 = try ha.allocOfSize(Header.maxLength);
    print("\nallocs={any}",.{.{alloc1,alloc2,alloc3,alloc4}});
    try ee(ha.freeSpace(),HeapAllocation.size-512+125-Header.maxLength);
//     try ee(ga.allocatedSpace(),heapAllocationSize);
//     try ee(ga.freeSpace(),0);
//     try ee(GlobalArena.boundaryCalc(ha.mem[0..14]),14);
//     try ee(GlobalArena.boundaryCalc(ha.mem[14..31]),16);
//     try ee(GlobalArena.boundaryCalc(ha.mem[14..30]),16);
//     try ee(GlobalArena.boundaryCalc(ha.mem[62..75]),8);
//     try ee(GlobalArena.boundaryCalc(ha.mem[126..158]),32);
//     try ee(GlobalArena.boundaryCalc(ha.mem[0..]),ha.mem.len);
//     ga.freeToList(ha.mem[0..14]);
//     try ee(ga.freeOfSize(8),8);
//     try ee(ga.freeOfSize(4),4);
//     try ee(ga.freeOfSize(2),2);
//     ga.freeToList(ha.mem[14..45]);
//     try ee(ga.freeOfSize(16),16);
//     try ee(ga.freeOfSize(8),16);
//     try ee(ga.freeOfSize(4),8);
//     try ee(ga.freeOfSize(2),4);
//     try ee(ga.freeOfSize(1),0);
//     ga.freeToList(ha.mem[45..]);
//     try ee(ga.freeSpace(),heapAllocationSize-2); // ignored the 2x 1-word allocations
}
const FreeList = extern struct {
    const Self = @This();
    size: u16,
    list: FreeListPtr,
    inline fn addToFree(self: *Self, ptr: HeaderArray) void {
        const fle = @intToPtr(*FreeListElement,@ptrToInt(ptr+self.size)-@sizeOf(FreeListElement));
        print("\n&fle=0x{x} size={}",.{@ptrToInt(fle),self.size});
        fle.header = header(@truncate(u12,self.size-1),Format.notIndexable,0,0,Age.free);
        if (self.size>=@sizeOf(FreeListElement)/@sizeOf(Header)) {
            var prev = self.list;
            while (true) {
                fle.next = prev;
                if (@cmpxchgWeak(FreeListPtr,&self.list,prev,fle,SeqCst,SeqCst)) |old| {
                    prev = old;
                } else
                    break;
            }
        }
    }
    fn getSlice(self: *Self) ?[]Header {
        var myList = &self.list;
        print("\ntrying {}",.{self.size});
        while (true) {
            if (myList.*) |fle| {
                const next = fle.next;
                if (@cmpxchgWeak(FreeListPtr,myList,fle,next,SeqCst,SeqCst)) |_| {
                    continue;
                } else {
                    print(" - success",.{});
                    return @intToPtr(HeaderArray,@ptrToInt(fle)+@sizeOf(FreeListElement)-self.size*@sizeOf(Header))[0..self.size];
                }
            }
            else return null;
        }
    }
    fn init(comptime n: comptime_int) [n]FreeList {
        var initial_value: [n]FreeList = undefined;
        for (initial_value[0..],0..) |*fl,index| {
            fl.size = @as(u16,1)<<@intCast(u4,index);
            fl.list = null;
        }
        return initial_value;
    }
    fn freeSpace(self: *const FreeList) usize {
        if (self.list) |fpe| return self.size*fpe.count();
        return 0;
    }
};
const FreeListPtr = ?*FreeListElement;
const FreeListElement = struct {
    next: FreeListPtr,
    header: Header,
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
test "freeList structure" {
    const ee = std.testing.expectEqual;
    const fls = FreeList.init(12);
    try ee(fls[0].size,1);
    try ee(fls[9].size,512);
    try ee(HeapAllocation.nFreeLists,13);
}

pub var globalArena = GlobalArena.init();
pub const GlobalArena = struct {
    const Self = @This();
    heapAllocations: ?*HeapAllocation,
    const allocationUnit = Header.maxLength; // size in u64 units including the header
    pub fn init() Self {
        return Self {
            .heapAllocations  = null,
        };
    }
    pub fn deinit(self: *Self) void {
        if (self.heapAllocations) |ha| ha.freeAll();
    }
    fn newHeapAllocation(self: *Self) *HeapAllocation {
        const ha = HeapAllocation.new();
        var prev = self.heapAllocations;
        while (true) {
            self.next = prev;
            if (@cmpxchgWeak(?*HeapAllocation,&self.heapAllocations,prev,ha,SeqCst,SeqCst)) |old| {prev = old;continue;}
            return self;
        }
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
        const self = @ptrCast(*Self, @alignCast(@alignOf(Self), ctx));
        _ = .{ptr_align,ret_addr};
        const obj = self.rawAlloc(0,len,ptr_align) catch return null;
        // ToDo: add obj to the allocatorKnown list for its page
        const array = obj.arrayAsSlice(u8);
        for (array) |*ptr| ptr.* = undefined;
        return array.ptr;
    }
    fn freeForAllocator (ctx: *anyopaque, buf: []u8, buf_align: u8, ret_addr: usize) void {
        const self = @ptrCast(*Self, @alignCast(@alignOf(Self), ctx));
        _ = .{self,buf,buf_align,ret_addr};
        unreachable;
    }
    // fn allocIndirect(self: *Self, sp:[*]Object, fieldsSize: usize, arraySize: usize) AllocReturn {
    //     const array = @ptrCast(HeapPtr,std.heap.page_allocator.alloc(Object, arraySize) catch @panic("page allocator failed"));
    //     var result = try GlobalArena.alloc(self,sp,hp,context,heapSize,0);
    //     const offs = @ptrCast([*]u64,result.allocated)+heapSize-2;
    //     offs[1] = @ptrToInt(array);
    //     return result;
    // }
    fn rawAlloc(self: *Self, fieldsSize: usize, arraySize: usize, ptr_align: u8) AllocErrors!HeapPtr {
        _ = ptr_align;
        const totalSize = fieldsSize + arraySize;
        var index = self.findAllocationList(totalSize);
// ToDo        if (index==0) return GlobalArena.allocIndirect(self,sp,hp,context,heapSize,arraySize);
        const allocation: []Header = (
            while (index<self.freeLists.len) : (index += 1) {
                if (self.freeLists[index].getSlice()) |slice| break slice;
            } else 
                GlobalArena.HeapAllocation.allocSlice(self));
        self.freeToList(allocation[totalSize..]);
        if (arraySize>0) {
            const offs = @ptrCast([*]u64,allocation.ptr)+fieldsSize-2;
            offs[1] = @ptrToInt(offs+2);
        }
        return @ptrCast(HeapPtr,allocation.ptr);
    }
    fn collect(self: *Self) AllocErrors!void {
        _ =  self;
        @panic("incomplete");
    }
   pub fn promote(obj: Object) !Object {
       if (!obj.isHeapAllocated()) return obj;
       if (obj.header().age==Age.static) return obj;
       unreachable;
//       @memcpy(@ptrCast([*]u8,result),@ptrCast([*]const u8,ptr),totalSize*8);
//       return result.asObject();
    }
    fn findAllocationList(self: *Self, target: usize) u7 {
        if (target > Header.maxLength) return 0;
        if (target < self.minAllocation) return self.minFreeList;
        return bitsToRepresent(target-1);
    }
    inline fn boundaryCalc(space: []Header) usize {
        const po2:usize = smallerPowerOf2(space.len);
        const mask = @bitCast(usize,-@intCast(isize,po2*@sizeOf(Header)));
        const alignedLen = ((@ptrToInt(space.ptr+space.len)&mask)-@ptrToInt(space.ptr))/@sizeOf(Header);
        return alignedLen;
    }
    fn freeToList(self: *Self, space: []Header) void {
        const alignedLen = boundaryCalc(space);
        if (alignedLen<space.len) self.freeToList(space[alignedLen..]);
        var free = space[0..alignedLen];
        while (free.len>0) {
            const len = smallerPowerOf2(free.len);
            const end = free.len - len;
            FreeList.addToFree(self,@intCast(u12,len),@ptrCast(HeapPtr,free.ptr+end));
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
        return self.freeLists[bitsToRepresent(size-1)].freeSpace();
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
};
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
//     const allocSize = (Header.maxLength>>1)-1;
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
//     // var result = ga.asSelf().allocArray(([0]Object{})[0..],([0]Header{})[0..],&context,19,42,0,u8) catch @panic("allocArray failed");
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
//     try ee(ga.findAllocationList(Header.maxLength),GlobalArena.nFreeLists);
//     try ee(ga.findAllocationList(Header.maxLength+1),0);
// }
// test "allocStruct" {
//     const Test_S = extern struct {
//         header: Header,
//         superclass: Object,
//         methodDict: Object,
//         format: Object,
//     };
//     const ee = std.testing.expectEqual;
//     var ga = GlobalArena.init();
//     defer ga.deinit();
//     var test1 = ga.allocStruct(17,Test_S,0,Object);
//     try ee(test1.superclass,Nil);
//     const h1: HeapPtr = &test1.header;
//     try ee(h1.inHeapSize(),4);
//     try std.testing.expectError(error.NotIndexable,h1.arrayAsSlice(u8));
//     var test2 = ga.allocStruct(17,Test_S,8,u8);
//     try ee(test2.methodDict,Nil);
//     const h2: HeapPtr = &test2.header;
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
//     const maxValid = Header.maxLength-45;
//     const result2 = ga.asSelf().allocArray(([0]Object{})[0..],([0]Header{})[0..],&context,19,42,maxValid*8-1,u8) catch @panic("allocArray failed");
//     const h2 = result2.allocated;
//     try ee(h2.isIndirect(),false);
//     try ee(h2.inHeapSize(),Header.maxLength);
//     try ee(h2.arraySize(),maxValid*8-1);
//     const result3 = ga.asSelf().allocArray(([0]Object{})[0..],([0]Header{})[0..],&context,19,42,maxValid*8+1,u8) catch @panic("allocArray failed");
//     const h3 = result3.allocated;
//     try ee(h3.isIndirect(),true);
//     try ee(h3.inHeapSize(),45);
//     try ee(h3.arraySize(),maxValid*8+1);
//     const maxVArray = Header.maxLength-1;
//     const result4 = ga.asSelf().allocArray(([0]Object{})[0..],([0]Header{})[0..],&context,19,0,maxVArray*8-1,u8) catch @panic("allocArray failed");
//     const h4 = result4.allocated;
//     try ee(h4.isIndirect(),false);
//     try ee(h4.inHeapSize(),Header.maxLength);
//     try ee(h4.arraySize(),maxVArray*8-1);
//     const result5 = ga.asSelf().allocArray(([0]Object{})[0..],([0]Header{})[0..],&context,19,0,maxVArray*8+1,u8) catch @panic("allocArray failed");
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
