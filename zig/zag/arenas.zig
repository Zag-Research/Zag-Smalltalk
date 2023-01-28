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
const Context = @import("context.zig").Context;
const ContextPtr = @import("context.zig").ContextPtr;
pub inline fn arenaFree(stackPointer: [*]const Object, heapPointer: HeaderArray) isize {
    return @divFloor(@bitCast(isize,(@ptrToInt(stackPointer)-%@ptrToInt(heapPointer))),@sizeOf(Object));
}
test "arenaFree" {
    const testing = std.testing;
    const stack: [10]Object align(8) =undefined;
    const s1: [*]const Object = @ptrCast([*]const Object,&stack[1]);
    const s5 = s1+4;
    const hp: HeaderArray = Header.fromObjectPtr(s1+2);
    try testing.expectEqual(arenaFree(s5,hp),2);
    try testing.expectEqual(arenaFree(s1,hp),-2);
}
test "fubar" {
    const nType = NurseryArena(1024);
    _ = nType;
}
test "object in nursery arena" {
    const nType = NurseryArena(1024);
    var nursery = nType.new();
    nursery.init();
    var hp = nursery.getHp();
    var sp = nursery.endOfStack();
    var context = Context.init();
    const a = nursery.asArena();
    const r = try a.allocObject(sp,hp,&context,42,5);
    const o = r.allocated;
    try std.testing.expect(!o.isInStack());
    try std.testing.expect(!o.isForwarded());
    try std.testing.expect(!o.isIndirect());
    try std.testing.expect(!o.isIndexable());
    try std.testing.expect(!o.isRaw());
    const ivs = o.instVars();
    try std.testing.expect(ivs.len==5);
}
const ArenaErrors = error {Fail,HeapFull,NotIndexable};
const AllocResult = struct {
    sp: [*]Object,
    hp: HeaderArray,
    context: ContextPtr,
    age: Age,
    allocated: HeapPtr,
};
const AllocReturn = ArenaErrors!AllocResult;
pub const Arena = extern struct {
    const Self = @This();
    alloc: *const fn (*Self,[*]Object,HeaderArray,ContextPtr,usize,usize) ArenaErrors!AllocResult,
    collect: *const fn (*Self,[*]Object,HeaderArray,ContextPtr) ArenaErrors!void,

    pub inline fn allocObject(self:*Self, sp:[*]Object, hp:HeaderArray, context:ContextPtr, classIndex:ClassIndex, ivSize:usize) AllocReturn {
        var result = try self.alloc(self,sp,hp,context,ivSize+1,0);
        initAllocation(result.allocated,classIndex, Format.objectNP, ivSize, result.age, Nil);
        return result;
    }
    pub fn allocArray(self:*Self, sp:[*]Object, hp:HeaderArray, context:ContextPtr, classIndex:ClassIndex, ivSize:usize, arraySize:usize, comptime T: type) AllocReturn {
        if (arraySize==0) return self.allocObject(sp,hp,context,classIndex,ivSize);
        const noIVs = ivSize==0;
        var form = (if (noIVs) Format.none else Format.objectNP).raw(T,arraySize);
        const width = @sizeOf(T);
        const aSize = (arraySize*width+objectWidth-width)/objectWidth;
        const fill = if (T==Object) Nil else object.ZERO;
        if (noIVs) {
            if (aSize<Header.maxLength) {
                var result = try self.alloc(self,sp,hp,context,aSize+1,0);
                initAllocation(result.allocated,classIndex, form, aSize, result.age, fill);
                return result;
            }
        }
        var result = try self.alloc(self,sp,hp,context,ivSize+3,aSize);
        const offs = @ptrCast([*]u64,result.allocated)+ivSize+1;
        mem.set(Object,@intToPtr([*]Object,offs[1])[0..aSize],fill);
        offs[0] = arraySize;
        initAllocation(result.allocated,classIndex, form.setObject(), ivSize, result.age, Nil);
        return result;
    }
    inline fn allocStruct(self: *Self, sp:[*]Object, hp:HeaderArray, context:ContextPtr, classIndex: class.ClassIndex, comptime T: type, extra: usize, comptime T2: type) AllocReturn {

        // should call allocObject or allocArray
        
        const ivSize = (@sizeOf(T)+objectWidth-1)/objectWidth-1;
        if (extra==0) return self.allocObject(sp,hp,context,classIndex,ivSize);
        return self.allocArray(sp,hp,context,classIndex,ivSize,extra,T2);
    }
    inline fn initAllocation(result: HeapPtr, classIndex: class.ClassIndex, form: Format, size: usize, age: Age, fill: Object) void {
        const hash = if (builtin.is_test) 0 else @truncate(u24,@truncate(u32,@ptrToInt(result))*%object.u32_phi_inverse>>8);
        mem.set(Object,result.asObjectPtr()[1..size+1],fill);
        result.*=header(@intCast(u12,size),form,classIndex,hash,age);
    }
};

pub fn NurseryArena(comptime nursery_size: comptime_int) type {
    return extern struct {
        const Self = @This();
        arena: Arena,
        hp: HeaderArray,
        sp: [*]Object,
        heapArea: [nursery_size-field_size]Header,
        const field_size = (@sizeOf(Arena)+@sizeOf(HeaderArray)+@sizeOf([*]Object))/@sizeOf(Header);
        comptime {
            if (checkEqual(@sizeOf(Self),nursery_size*@sizeOf(Header))) |s|
                @compileError("Modify NurseryArena.field_size to make @sizeOf(NurseryArena) == " ++ s);
        }
        pub fn new() Self {
            return Self {
                .arena = undefined,
                .hp = undefined,
                .sp = undefined,
                .heapArea = undefined,
            };
        }
        pub fn init(self: *Self) void {
            self.arena = Arena{.alloc=alloc,.collect=collect};
            self.hp = @ptrCast(HeaderArray,@alignCast(@alignOf(u64),&self.heapArea));
            self.sp = self.endOfStack();
        }
        pub fn asArena(self: *Self) *Arena {
            return @ptrCast(*Arena,self);
        }
        pub inline fn endOfStack(self: *Self) [*]Object {
            return @intToPtr([*]Object,@ptrToInt(&self.heapArea))+self.heapArea.len;
        }
        pub inline fn getHp(self: *Self) HeaderArray {
            return self.hp;
        }
        fn allocSlow(_: *Arena, _:[*]Object, _:HeaderArray, _:ContextPtr, _: usize, _: usize) AllocReturn {
            return error.HeapFull;
        }
        fn alloc(arena: *Arena, sp:[*]Object, hp:HeaderArray, context:ContextPtr, heapSize: usize, arraySize: usize) AllocReturn {
            const totalSize = heapSize + arraySize;
            const result = @ptrCast(HeapPtr,hp);
            const end = hp + totalSize;
            if (@ptrToInt(sp)<=@ptrToInt(end)) return allocSlow(arena,sp,hp,context,heapSize,arraySize);
            return .{.sp=sp, .hp=end, .context=context, .age=Age.nursery, .allocated=result,};
        }
        fn collect(arena: *Arena, sp:[*]Object, hp:HeaderArray, context:ContextPtr) ArenaErrors!void {
            const self = @ptrCast(*Self,arena);
            _ =  self; _ = sp; _ = hp; _ = context;
            @panic("incomplete");
        }
    };
}
// pub fn TeenArena(comptime teen_size: comptime_int) type {
//     return extern struct {
//     const Self = @This();
//     arena: Arena,
//     free: HeaderArray,
//     heapArea: [size]Header,
//     const size = teen_size-field_size;
//     const field_size = (@sizeOf(Arena)+@sizeOf(HeaderArray))/@sizeOf(Header);
//     comptime {
//         if (checkEqual(@sizeOf(TeenArena),teen_size*@sizeOf(Header))) |s|
//             @compileError("Modify TeenArena.heap to make @sizeOf(TeenArena) == " ++ s);
//     }
//     pub fn new() TeenArena {
//         return Self {
//             .arena = Arena{.alloc=alloc,.collect=collect};
//             .free = undefined,
//             .heap = undefined,
//         };
//     }
//     pub fn init(self: *Self, otherTeenHeap: *Self) void {
//         _ = otherTeenHeap;
//         self.free = @ptrCast(HeaderArray,&self.heap);
//     }
// };

pub var globalArena = GlobalArena.init();
pub const heapAllocationSize = GlobalArena.HeapAllocation.size;
pub const GlobalArena = struct {
    const Self = @This();
    arena: Arena,
    heapAllocations: ?*HeapAllocation,
    freeLists: [nFreeLists]FreeList,
    minFreeList: u7,
    minAllocation: u16,
    const nFreeLists = bitsToRepresent(Header.maxLength);
    const allocationUnit = Header.maxLength; // size in u64 units including the header
    pub fn init() Self {
        const minFreeList = 1;
        return Self {
            .arena = Arena{.alloc=alloc,.collect=collect},
            .heapAllocations  = null,
            .freeLists = FreeList.init(nFreeLists),
            .minFreeList = minFreeList,
            .minAllocation = 1<<minFreeList,
        };
    }
    pub fn deinit(self: *Self) void {
        if (self.heapAllocations) |ha| ha.freeAll();
    }
    pub fn asArena(self: *Self) *Arena {
        return @ptrCast(*Arena,self);
    }
    fn allocIndirect(arena: *Arena, sp:[*]Object, hp:HeaderArray, context:ContextPtr, heapSize: usize, arraySize: usize) AllocReturn {
        const self = @ptrCast(*Self,arena);
        _ = self;
        const array = @ptrCast(HeapPtr,std.heap.page_allocator.alloc(Object, arraySize) catch @panic("page allocator failed"));
        var result = try GlobalArena.alloc(arena,sp,hp,context,heapSize,0);
        const offs = @ptrCast([*]u64,result.allocated)+heapSize-2;
        offs[1] = @ptrToInt(array);
        return result;
    }
    fn alloc(arena: *Arena, sp:[*]Object, hp:HeaderArray, context:ContextPtr, heapSize: usize, arraySize: usize) AllocReturn {
        const self = @ptrCast(*Self,arena);
        const totalSize = heapSize + arraySize;
        var index = self.findAllocationList(totalSize);
        if (index==0) return GlobalArena.allocIndirect(arena,sp,hp,context,heapSize,arraySize);
        const allocation: []Header = (
            while (index<self.freeLists.len) : (index += 1) {
                if (self.freeLists[index].getSlice()) |slice| break slice;
            } else 
                GlobalArena.HeapAllocation.allocSlice(self));
        self.freeToList(allocation[totalSize..]);
        if (arraySize>0) {
            const offs = @ptrCast([*]u64,allocation.ptr)+heapSize-2;
            offs[1] = @ptrToInt(offs+2);
        }
        return .{.sp=sp, .hp=hp, .context=context, .age=Age.global, .allocated=@ptrCast(HeapPtr,allocation.ptr),};
    }
    fn collect(arena: *Arena, sp:[*]Object, hp:HeaderArray, context:ContextPtr) ArenaErrors!void {
        const self = @ptrCast(*Self,arena);
        _ =  self; _ = sp; _ = hp; _ = context;
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
    pub inline fn allocObject(self:*Self, classIndex:ClassIndex, ivSize:usize, context: ContextPtr) Object {
        var result = self.asArena().allocObject(([0]Object{})[0..],([0]Header{})[0..],context,classIndex,ivSize) catch @panic("allocObject failed");
        return result.allocated.asObject();
    }
    pub inline fn allocArray(self:*Self, classIndex:ClassIndex, arraySize:usize, comptime T: type, context: ContextPtr) Object {
        var result = self.asArena().allocArray(([0]Object{})[0..],([0]Header{})[0..],context,classIndex,0,arraySize,T) catch @panic("allocArray failed");
        return result.allocated.asObject();
    }
    pub inline fn allocStruct(self : *Self, classIndex : class.ClassIndex, comptime T: type, extra: usize, comptime T2: type, context: ContextPtr) *T {
        var result = self.asArena().allocStruct(([0]Object{})[0..],([0]Header{})[0..],context,classIndex,T,extra,T2) catch @panic("allocStruct failed");
        return @intToPtr(*T,@ptrToInt(result.allocated));
    }
    const HeapAllocation = extern struct {
        flags: u64,
        next: ?*HeapAllocation,
        mem: [size]Header,
        const field_size = @sizeOf(u64)+@sizeOf(?*HeapAllocation);
        const heap_allocation_size = std.mem.page_size; //64*1024;
        const size = (heap_allocation_size - field_size)/@sizeOf(Header);
        const maxObjects = size/@sizeOf(Header);
        const returnType = []u8;
        fn getAligned() []align(heap_allocation_size)u8 { // ToDo: align larger size without wasting 1/2 the space
            //var buf = std.heap.page_allocator.alloc(u8, heap_allocation_size*2-std.mem.page_size) catch @panic("page allocator failed");
            //const base = @ptrToInt(buf.ptr) & (heap_allocation_size-1);
            //const offs = if (base==0) 0 else heap_allocation_size-base;
            //if (!std.heap.page_allocator.resize(buf,offs+heap_allocation_size)) @panic("resize failed");
            //return @alignCast(heap_allocation_size,buf[offs..offs+page_allocation_size]);
            return @alignCast(heap_allocation_size,std.heap.page_allocator.alloc(u8, heap_allocation_size) catch @panic("page allocator failed"));
        }
        fn allocSlice(arena: *GlobalArena) []Header {
            var ha = HeapAllocation.alloc(arena);
            return ha.mem[0..];
        }
        fn alloc(arena: *GlobalArena) *HeapAllocation {
            var space = getAligned();
            const self = @ptrCast(*HeapAllocation,space.ptr);
            self.flags = 0;
            var prev = arena.heapAllocations;
            while (true) {
                self.next = prev;
                if (@cmpxchgWeak(?*HeapAllocation,&arena.heapAllocations,prev,self,SeqCst,SeqCst)) |old| {prev = old;continue;}
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
            std.heap.page_allocator.free(@ptrCast([*]u8,self)[0..heap_allocation_size]);
        }
        fn sweep(self: *HeapAllocation) void {
            var ptr = @ptrCast(HeaderArray,&self.mem);
            const end = ptr+maxObjects;
            while (ptr<end) {
                unreachable;
            }
        }
    };
    const FreeList = struct {
        size: u16,
        list: FreeListPtr,
        inline fn addToFree(ga: *GlobalArena, len: u12, ptr: HeapPtr) void {
            ptr.* = header(len,Format.none,0,0,Age.free);
            if (len>=ga.minAllocation) {
                const self = @ptrCast(*FreeListElement,ptr);
                const freeListIndex = ga.findAllocationList(len);
                if (freeListIndex>0) {
                    var myList = &ga.freeLists[freeListIndex].list;
                    var prev = myList.*;
                    while (true) {
                        self.next = prev;
                        if (@cmpxchgWeak(FreeListPtr,myList,prev,self,SeqCst,SeqCst)) |old| {
                            prev = old;
                        } else
                            break;
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
                    if (@cmpxchgWeak(FreeListPtr,myList,prev,next,SeqCst,SeqCst)) |old| {
                        prev = old;
                    } else
                        return @ptrCast(HeaderArray,fle)[0..self.size];
                }
                else return null;
            }
        }
        fn init(comptime n: comptime_int) [n]FreeList {
            var initial_value: [n]FreeList = undefined;
            for (initial_value) |*fl,index| {
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
    const fls = GlobalArena.FreeList.init(12);
    try ee(fls[0].size,1);
    try ee(fls[9].size,512);
    try ee(GlobalArena.nFreeLists,switch (std.mem.page_size) {
        4096 => 9,
        16384 => 11,
        else => std.mem.page_size,
    });
}
test "check HeapAllocations" {
    const ee = std.testing.expectEqual;
    var ga = GlobalArena.init();
    defer ga.deinit();
    var ha = GlobalArena.HeapAllocation.alloc(&ga);
    try ee(ga.allocatedSpace(),heapAllocationSize);
    try ee(ga.freeSpace(),0);
    try ee(GlobalArena.boundaryCalc(ha.mem[0..14]),14);
    try ee(GlobalArena.boundaryCalc(ha.mem[14..31]),16);
    try ee(GlobalArena.boundaryCalc(ha.mem[14..30]),16);
    try ee(GlobalArena.boundaryCalc(ha.mem[62..75]),8);
    try ee(GlobalArena.boundaryCalc(ha.mem[126..158]),32);
    try ee(GlobalArena.boundaryCalc(ha.mem[0..]),ha.mem.len);
    ga.freeToList(ha.mem[0..14]);
    try ee(ga.freeOfSize(8),8);
    try ee(ga.freeOfSize(4),4);
    try ee(ga.freeOfSize(2),2);
    ga.freeToList(ha.mem[14..45]);
    try ee(ga.freeOfSize(16),16);
    try ee(ga.freeOfSize(8),16);
    try ee(ga.freeOfSize(4),8);
    try ee(ga.freeOfSize(2),4);
    try ee(ga.freeOfSize(1),0);
    ga.freeToList(ha.mem[45..]);
    try ee(ga.freeSpace(),heapAllocationSize-2); // ignored the 2x 1-word allocations
}
test "check GlobalArena alloc object" {
    const ee = std.testing.expectEqual;
    const err = std.testing.expectError;
    var ga = GlobalArena.init();
    defer ga.deinit();
    var context = Context.init();
    var o1 = ga.allocObject(17,5,&context);
    try ee(ga.allocatedSpace(),heapAllocationSize);
    try ee(o1.inHeapSize(),6);
    try ee(ga.freeSpace(),heapAllocationSize-6);
    try err(error.NotIndexable,o1.size());
}
test "check alloc array" {
    const ee = std.testing.expectEqual;
    const err = std.testing.expectError;
    const allocSize = (Header.maxLength>>1)-1;
    var ga = GlobalArena.init();
    defer ga.deinit();
    var context = Context.init();
    var o1 = ga.allocArray(17,allocSize,u64,&context);
    try ee(ga.allocatedSpace(),heapAllocationSize);
    try ee(o1.inHeapSize(),allocSize+1);
    const a1 = o1.arrayAsSlice(u64);
    try ee(a1.len,allocSize);
    try ee(ga.freeSpace(),heapAllocationSize-(allocSize+1));
    var o2 = ga.allocArray(42,allocSize,u64,&context);
    try ee(ga.allocatedSpace(),heapAllocationSize);
    try ee(o2.inHeapSize(),allocSize+1);
    const a2 = o2.arrayAsSlice(u64);
    try ee(a2.len,allocSize);
    try ee(ga.freeSpace(),heapAllocationSize-(allocSize+1)*2);
    var result = ga.asArena().allocArray(([0]Object{})[0..],([0]Header{})[0..],&context,19,42,0,u8) catch @panic("allocArray failed");
    try err(error.NotIndexable,result.allocated.asObject().size());
}
test "findAllocationList" {
    const ee = std.testing.expectEqual;
    var ga = GlobalArena.init();
    defer ga.deinit();
    try ee(ga.findAllocationList(1),1);
    try ee(ga.findAllocationList(2),1);
    try ee(ga.findAllocationList(3),2);
    try ee(ga.findAllocationList(4),2);
    try ee(ga.findAllocationList(17),5);
    try ee(ga.findAllocationList(Header.maxLength),GlobalArena.nFreeLists);
    try ee(ga.findAllocationList(Header.maxLength+1),0);
}
test "allocStruct" {
    const Test_S = extern struct {
        header: Header,
        superclass: Object,
        methodDict: Object,
        format: Object,
    };
    const ee = std.testing.expectEqual;
    var ga = GlobalArena.init();
    defer ga.deinit();
    var context = Context.init();
    var test1 = ga.allocStruct(17,Test_S,0,Object,&context);
    try ee(test1.superclass,Nil);
    const h1: HeapPtr = &test1.header;
    try ee(h1.inHeapSize(),4);
    try std.testing.expectError(error.NotIndexable,h1.arrayAsSlice(u8));
    var test2 = ga.allocStruct(17,Test_S,8,u8,&context);
    try ee(test2.methodDict,Nil);
    const h2: HeapPtr = &test2.header;
    try ee(h2.inHeapSize(),7);
    const a2: []u8 = try h2.arrayAsSlice(u8);
    try ee(a2.len,8);
}
test "check alloc indirect" {
    const ee = std.testing.expectEqual;
    const err = std.testing.expectError;
    var ga = GlobalArena.init();
    defer ga.deinit();
    var context = Context.init();
    var o1 = ga.allocObject(17,5,&context);
    try ee(ga.allocatedSpace(),heapAllocationSize);
    try ee(o1.inHeapSize(),6);
    try ee(ga.freeSpace(),heapAllocationSize-6);
    try err(error.NotIndexable,o1.size());
    const maxValid = Header.maxLength-45;
    const result2 = ga.asArena().allocArray(([0]Object{})[0..],([0]Header{})[0..],&context,19,42,maxValid*8-1,u8) catch @panic("allocArray failed");
    const h2 = result2.allocated;
    try ee(h2.isIndirect(),false);
    try ee(h2.inHeapSize(),Header.maxLength);
    try ee(h2.arraySize(),maxValid*8-1);
    const result3 = ga.asArena().allocArray(([0]Object{})[0..],([0]Header{})[0..],&context,19,42,maxValid*8+1,u8) catch @panic("allocArray failed");
    const h3 = result3.allocated;
    try ee(h3.isIndirect(),true);
    try ee(h3.inHeapSize(),45);
    try ee(h3.arraySize(),maxValid*8+1);
    const maxVArray = Header.maxLength-1;
    const result4 = ga.asArena().allocArray(([0]Object{})[0..],([0]Header{})[0..],&context,19,0,maxVArray*8-1,u8) catch @panic("allocArray failed");
    const h4 = result4.allocated;
    try ee(h4.isIndirect(),false);
    try ee(h4.inHeapSize(),Header.maxLength);
    try ee(h4.arraySize(),maxVArray*8-1);
    const result5 = ga.asArena().allocArray(([0]Object{})[0..],([0]Header{})[0..],&context,19,0,maxVArray*8+1,u8) catch @panic("allocArray failed");
    const h5 = result5.allocated;
    try ee(h5.isIndirect(),true);
    try ee(h5.inHeapSize(),3);
    try ee(h5.arraySize(),maxVArray*8+1);
}

