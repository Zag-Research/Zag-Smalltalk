const std = @import("std");
const builtin = @import("builtin");
const SeqCst = std.builtin.AtomicOrder.SeqCst;
const mem = std.mem;
const checkEqual = @import("utilities.zig").checkEqual;
const thread = @import("thread.zig");
const object = @import("object.zig");
const Object = object.Object;
const objectWidth = @sizeOf(Object);
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const class = @import("class.zig");
const ClassIndex = class.ClassIndex;
const bitsToRepresent = @import("utilities.zig").bitsToRepresent;
const smallerPowerOf2 = @import("utilities.zig").smallerPowerOf2;
const largerPowerOf2Not1 = @import("utilities.zig").largerPowerOf2Not1;
const Header = @import("heap.zig").Header;
const header = @import("heap.zig").header;
const Format = @import("heap.zig").Format;
const Age = @import("heap.zig").Age;
const HeaderArray = @import("heap.zig").HeaderArray;
const HeapPtr = @import("heap.zig").HeapPtr;
//const ContextPtr = @import("execute.zig").CodeContextPtr;
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
test "with arena" {
    var t = thread.Thread.new();
    var nursery = NurseryArena.new();
    nursery.init(&t);
    var hp = nursery.getHp();
    var sp = nursery.endOfStack();
    const context = 0;
    const a = nursery.asArena();
    const r = try a.allocObject(sp,hp,context,42,5);
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
    context: u64,
    age: Age,
    allocated: HeapPtr,
};
const AllocReturn = ArenaErrors!AllocResult;
const Arena = extern struct {
    const Self = @This();
    alloc: *const fn (*Arena,[*]Object,HeaderArray,u64,u64) ArenaErrors!AllocResult,
    collect: *const fn (*Arena,[*]Object,HeaderArray,u64) ArenaErrors!void,

    pub inline fn allocObject(self:*Self, sp:[*]Object, hp:HeaderArray, context:u64, classIndex:ClassIndex, ivSize:usize) AllocReturn {
        var result = try self.alloc(self,sp,hp,context,ivSize+1);
        initAllocation(result.allocated,classIndex, Format.objectNP, ivSize, result.age, Nil);
        return result;
    }
    pub fn allocArray(self:*Self, sp:[*]Object, hp:HeaderArray, context:u64, classIndex:ClassIndex, ivSize:usize, arraySize:usize, comptime T: type) AllocReturn {
        const noIVs = ivSize==0;
        var form = (if (noIVs) Format.none else Format.objectNP).raw(T,arraySize);
        const width = @sizeOf(T);
        const aSize = (arraySize*width+objectWidth-width)/objectWidth;
        const fill = if (T==Object) Nil else object.ZERO;
        if (ivSize==0) {
            if (aSize<Header.maxLength) {
                var result = try self.alloc(self,sp,hp,context,aSize+1);
                initAllocation(result.allocated,classIndex, form, ivSize, result.age, fill);
                return result;
            }
            @panic("big array");
        }
        const totalSize = 0; //ToDo: Wrong!
        var result = try self.alloc(self,sp,hp,context,totalSize+1);
        mem.set(Object,try result.allocated.arrayAsSlice(Object),fill);
        initAllocation(result.allocated,classIndex, form, ivSize, result.age, Nil);
        return result;
    }
    inline fn allocStruct(self: *Self, sp:[*]Object, hp:HeaderArray, context:u64, classIndex: class.ClassIndex, comptime T: type, extra: usize, comptime T2: type) AllocReturn {

        // should call allocObject or allocArray
        
        const ivSize = (@sizeOf(T)+objectWidth-1)/objectWidth;
        if (extra==0) return self.allocObject(sp,hp,context,classIndex,ivSize);
        const aSize = (extra+objectWidth-1)/objectWidth;
        return self.allocArray(sp,hp,context,classIndex,ivSize,aSize,T2);
    }
    inline fn initAllocation(result: HeapPtr, classIndex: class.ClassIndex, form: Format, size: usize, age: Age, fill: Object) void {
        const hash = if (builtin.is_test) 0 else @truncate(u24,@truncate(u32,@ptrToInt(result))*%object.u32_phi_inverse>>8);
        mem.set(Object,result.asObjectPtr()[1..size+1],fill);
        result.*=header(@intCast(u12,size),form,classIndex,hash,age);
    }
};

const threadAvail = thread.avail_size;
const nursery_size = @min(threadAvail/7/@sizeOf(Object),Header.maxLength);
const teen_size = (threadAvail-@sizeOf(NurseryArena))/2/@sizeOf(Object);
pub const NurseryArena = extern struct {
    const Self = @This();
    arena: Arena,
    hp: HeaderArray,
    sp: [*]Object,
    thread: *thread.Thread,
    heapArea: [nursery_size-field_size/@sizeOf(Header)]Header,
    const field_size = @sizeOf(HeaderArray)+@sizeOf([*]Object)+@sizeOf(*thread.Thread)+@sizeOf(Arena);
    comptime {
        if (checkEqual(@sizeOf(NurseryArena),nursery_size*@sizeOf(Header))) |s|
            @compileError("Modify NurseryArena.heapArea to make @sizeOf(NurseryArena) == " ++ s);
    }
    pub fn new() NurseryArena {
        return NurseryArena {
            .arena = undefined,
            .hp = undefined,
            .sp = undefined,
            .thread = undefined,
            .heapArea = undefined,
        };
    }
    pub fn init(self: *Self, thr: *thread.Thread) void {
        self.arena = Arena{.alloc=alloc,.collect=collect};
        self.hp = @ptrCast(HeaderArray,@alignCast(@alignOf(u64),&self.heapArea[0]));
        self.sp = self.endOfStack();
        self.thread = thr;
    }
    pub fn asArena(self: *Self) *Arena {
        return @ptrCast(*Arena,self);
    }
    pub inline fn endOfStack(self: *Self) [*]Object {
        return @intToPtr([*]Object,@ptrToInt(&self.heapArea[0]))+self.heapArea.len;
    }
    pub inline fn getHp(self: *Self) HeaderArray {
        return self.hp;
    }
    fn alloc(_: *Arena, sp:[*]Object, hp:HeaderArray, context:u64, totalSize: usize) AllocReturn {
        const result = @ptrCast(HeapPtr,hp);
        const end = hp + totalSize;
        if (@ptrToInt(sp)<=@ptrToInt(end)) return error.HeapFull;
        return .{.sp=sp, .hp=end, .context=context, .age=Age.nursery, .allocated=result,};
    }
    fn collect(arena: *Arena, sp:[*]Object, hp:HeaderArray, context:u64) ArenaErrors!void {
        const self = @ptrCast(*Self,arena);
        _ =  self; _ = sp; _ = hp; _ = context;
        @panic("incomplete");
    }
};
// pub const TeenArena = extern struct {
//     const Self = @This();
//     arena: Arena,
//     free: HeaderArray,
//     heap: [size]Header,
//     const size = (teen_size-field_size)/@sizeOf(Header)
//     const field_size = @sizeOf(Arena)+@sizeOf(HeaderArray);
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
//         self.free = @ptrCast(HeaderArray,&self.heap[0]);
//     }
// };

pub var globalArena = GlobalArena.init();
pub const heapAllocationSize = GlobalArena.HeapAllocation.size;
pub const GlobalArena = struct {
    const Self = @This();
    arena: Arena,
    heapAllocations: ?*HeapAllocation,
    freeLists: [nFreeLists]FreeList,
    minFreeList: u16,
    minAllocation: u16,
    const nFreeLists = bitsToRepresent(Header.maxLength)+1;
    const allocationUnit = Header.maxLength; // size in u64 units including the header
    fn init() Self {
        const minFreeList = 1;
        return Self {
            .arena = Arena{.alloc=alloc,.collect=collect},
            .heapAllocations  = null,
            .freeLists = FreeList.init(nFreeLists),
            .minFreeList = minFreeList,
            .minAllocation = 1<<minFreeList,
        };
    }
    fn deinit(self: *Self) void {
        if (self.heapAllocations) |ha| ha.freeAll();
    }
    pub fn asArena(self: *Self) *Arena {
        return @ptrCast(*Arena,self);
    }
    fn alloc(arena: *Arena, _:[*]Object, _:HeaderArray, _:u64, totalSize: usize) AllocReturn {
        const self = @ptrCast(*Self,arena);
        _ = self; _ = totalSize;
        @panic("incomplete");
    }
    fn collect(arena: *Arena, sp:[*]Object, hp:HeaderArray, context:u64) ArenaErrors!void {
        const self = @ptrCast(*Self,arena);
        _ =  self; _ = sp; _ = hp; _ = context;
        @panic("incomplete");
    }
   pub fn promote(obj: Object) !Object {
       if (!obj.isHeapAllocated()) return obj;
       unreachable;
//       @memcpy(@ptrCast([*]u8,result),@ptrCast([*]const u8,ptr),totalSize*8);
//       return result.asObject();
    }
    fn findAllocationList(self: *Self, target: u16) ?*FreeList {
        if (target > Header.maxLength) return null;
        if (target < self.minAllocation) return &self.freeLists[self.minFreeList];
        return &self.freeLists[bitsToRepresent(target-1)];
    }
    fn freeToList(self: *Self, space: []Header) void {
        var free = space;
        while (free.len>0) {
            const len = smallerPowerOf2(free.len);
            // ToDo free not aligned properly
            FreeList.addToFree(self,len,free.ptr+len);
            free = free[0..len];
        }
    }
    fn freeSpace(self: *Self) usize {
        var sum: usize = 0;
        for (self.freeLists) |fl| {
            sum += fl.freeSpace();
        }
        return sum;
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
            var ptr = @ptrCast(HeaderArray,&self.mem[0]);
            const end = ptr+maxObjects;
            while (ptr<end) {
                unreachable;
            }
        }
    };
    pub inline fn allocObject(self:*Self, classIndex:ClassIndex, ivSize:usize) Object {
        var result = self.asArena().allocObject(([0]Object{})[0..],([0]Header{})[0..],0,classIndex,ivSize) catch @panic("allocObject failed");
        return result.allocated.asObject();
    }
    pub inline fn allocStruct(self : *Self, classIndex : class.ClassIndex, comptime T: type, extra: usize, comptime T2: type) *T {
        var result = self.asArena().allocStruct(([0]Object{})[0..],([0]Header{})[0..],0,classIndex,T,extra,T2) catch @panic("allocStruct failed");
        return @intToPtr(*T,@ptrToInt(result.allocated));
    }
    const FreeList = struct {
        size: u16,
        list: FreeListPtr,
        inline fn addToFree(ga: *GlobalArena, len: usize, ptr: HeapPtr) void {
            ptr.* = header(len,Format.none,0,0,Age.free);
            if (len>=ga.minAllocation) {
                const self = @ptrCast(FreeListPtr,ptr);
                const myFreeList = ga.findAllocationList(len);
                var prev = myFreeList.*;
                while (true) {
                    self.next = prev;
                    if (@cmpxchgWeak(FreeListPtr,myFreeList,prev,self,SeqCst,SeqCst)) |old| {prev = old;continue;}
                    break;
                }
            }
        }
        fn init(comptime n: comptime_int) [n]FreeList {
            var initial_value: [n]FreeList = undefined;
            for (initial_value) |*fl,index| {
                fl.size = @min(Header.maxLength,@as(u16,1)<<@intCast(u4,index));
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
        16384 => 12,
        else => std.mem.page_size,
    });
}
test "check allocations" {
    const ee = std.testing.expectEqual;
    var ga = GlobalArena.init();
    defer ga.deinit();
    var ha = GlobalArena.HeapAllocation.alloc(&ga);
    try ee(ga.allocatedSpace(),heapAllocationSize);
    try ee(ga.freeSpace(),0);
    _ = ha;
}
test "findAllocationList" {
    const ee = std.testing.expectEqual;
    var ga = GlobalArena.init();
    defer ga.deinit();
    try ee(ga.findAllocationList(1).?.size,2);
    try ee(ga.findAllocationList(2).?.size,2);
    try ee(ga.findAllocationList(3).?.size,4);
    try ee(ga.findAllocationList(4).?.size,4);
    try ee(ga.findAllocationList(17).?.size,32);
    try ee(ga.findAllocationList(Header.maxLength).?.size,Header.maxLength);
    try ee(ga.findAllocationList(Header.maxLength+1),null);
}
// test "slicing" {
//     const testing = std.testing;
//     var arena = TempArena(16).init();
//     const testArena = arena.asArena();
//     const hp0 = try testArena.allocObject(42,1,0,Object,Age.stack);
//     try testing.expectEqual((try hp0.arrayAsSlice(u8)).len,0);
//     const hp1 = try testArena.allocObject(42,1,2,Object,Age.stack);
//     const obj1 = hp1.asObject();
//     try testing.expect(hp1.isIndexable());
//     try testing.expect(obj1.isIndexable());
//     try testing.expectEqual((try hp1.arrayAsSlice(u8)).len,obj1.arrayAsSlice(u8).len);
//     try testing.expectEqual((try hp1.arrayAsSlice(u8)).ptr,obj1.arrayAsSlice(u8).ptr);
// //    try testing.expectEqual(hp1.arrayAsSlice(u8),obj1.arrayAsSlice(u8));
// }

// test "one object #1 allocator" {
//     const testing = std.testing;
//     const h1 = header(3,Format.object,42,0,Age.stack);
//     try testing.expectEqual(@alignCast(8,&h1).inHeapSize(),4);
//     const expected = ([_]Object{
//         h1.o(),True,Nil,False,
//     })[0..];
//     var arena = TempArena(expected.len).init();
//     const testArena = arena.asArena();
//     const obj1 = try testArena.allocObject(42,3,0,Object,Age.stack);
//     try testing.expectEqual(obj1.inHeapSize(),4);
//     const ivs1 = obj1.instVars();
//     try testing.expectEqual(ivs1.len,3);
//     ivs1[0]=True;
//     ivs1[2]=False;
//     try arena.verify(expected);
// }
// test "one object #2 allocator" {
//     const testing = std.testing;
//     const h2 = header(1,Format.both,43,0,Age.stack);
//     try testing.expectEqual(@alignCast(8,&h2).inHeapSize(),4);
//     const expected = ([_]Object{
//         h2.o(),True,Nil,@bitCast(Object,@as(u64,1)),False,Nil,Nil,
//     })[0..];
//     var arena = TempArena(expected.len).init();
//     const testArena = arena.asArena();
//     const obj2 = try testArena.allocObject(43,2,3,Object,Age.stack);
//     try testing.expectEqual(obj2.inHeapSize(),7);
//     const ivs2 = obj2.instVars();
//     try testing.expectEqual(ivs2.len,2);
//     ivs2[0]=True;
//     const idx2 = obj2.indexables(Object);
//     try testing.expectEqual(idx2.len,3);
//     idx2[0]=False;
//     try arena.verify(expected);
// }
// test "one object #3 allocator" {
//     const testing = std.testing;
//     const h3 = header(2,Format.array,44,0,Age.stack);
//     try testing.expectEqual(@alignCast(8,&h3).inHeapSize(),3);
//     const expected = ([_]Object{
//         h3.o(),Nil,True,
//     })[0..];
//     var arena = TempArena(expected.len).init();
//     const testArena = arena.asArena();
//     const obj3 = try testArena.allocObject(44,0,2,Object,Age.stack);
//     try testing.expectEqual(obj3.inHeapSize(),3);
//     const ivs3 = obj3.instVars();
//     try testing.expectEqual(ivs3.len,0);
//     const idx3 = obj3.indexables(Object);
//     try testing.expectEqual(idx3.len,2);
//     idx3[1]=True;
//     try arena.verify(expected);
// }
// test "one object #4 allocator" {
//     const testing = std.testing;
//     const h4 = header(2,Format.array,44,0,Age.stack);
//     try testing.expectEqual(@alignCast(8,&h4).inHeapSize(),3);
//     const expected = ([_]Object{
//         h4.o(),@bitCast(Object,@as(u64,0)),@bitCast(Object,@as(u64,1)),
//     })[0..];
//     var arena = TempArena(expected.len).init();
//     const testArena = arena.asArena();
//     const obj4 = try testArena.allocObject(45,0,2,u64,Age.stack);
//     try testing.expectEqual(obj4.inHeapSize(),3);
//     const ivs4 = obj4.instVars();
//     try testing.expectEqual(ivs4.len,0);
//     const idx4 = obj4.indexables(i64);
//     try testing.expectEqual(idx4.len,2);
//     idx4[1]=1;
//     try arena.verify(expected);
// }
// test "four object allocator" {
//     const testing = std.testing;
//     const h1 = header(3,Format.object,42,0,Age.stack);
//     try testing.expectEqual(@alignCast(8,&h1).inHeapSize(),4);
//     const h2 = header(1,Format.both,43,0,Age.stack);
//     try testing.expectEqual(@alignCast(8,&h2).inHeapSize(),4);
//     const h3 = header(2,Format.array,44,0,Age.stack);
//     try testing.expectEqual(@alignCast(8,&h3).inHeapSize(),3);
//     const h4 = header(2,Format.array,44,0,Age.stack);
//     try testing.expectEqual(@alignCast(8,&h4).inHeapSize(),3);
//     const expected = ([_]Object{
//         h1.o(),True,Nil,False,
//         h2.o(),True,@bitCast(Object,@as(u64,1)),False,
//         h3.o(),Nil,True,
//         h4.o(),@bitCast(Object,@as(u64,0)),@bitCast(Object,@as(u64,1)),
//     })[0..];
//     var arena = TempArena(expected.len).init();
//     const testArena = arena.asArena();
//     const obj1 = try testArena.allocObject(42,3,0,Object,Age.stack);
//     try testing.expectEqual(obj1.inHeapSize(),4);
//     const obj2 = try testArena.allocObject(43,1,1,Object,Age.stack);
//     try testing.expectEqual(obj2.inHeapSize(),4);
//     const obj3 = try testArena.allocObject(44,0,2,Object,Age.stack);
//     try testing.expectEqual(obj3.inHeapSize(),3);
//     const obj4 = try testArena.allocObject(45,0,2,u64,Age.stack);
//     try testing.expectEqual(obj4.inHeapSize(),3);
//     const ivs4 = obj4.instVars();
//     try testing.expectEqual(ivs4.len,0);
//     const idx4 = obj4.indexables(i64);
//     try testing.expectEqual(idx4.len,2);
//     idx4[1]=1;
//     const ivs3 = obj3.instVars();
//     try testing.expectEqual(ivs3.len,0);
//     const idx3 = obj3.indexables(Object);
//     try testing.expectEqual(idx3.len,2);
//     idx3[1]=True;
//     const ivs2 = obj2.instVars();
//     try testing.expectEqual(ivs2.len,1);
//     ivs2[0]=True;
//     const idx2 = obj2.indexables(Object);
//     try testing.expectEqual(idx2.len,1);
//     idx2[0]=False;
//     const ivs1 = obj1.instVars();
//     try testing.expectEqual(ivs1.len,3);
//     ivs1[0]=True;
//     ivs1[2]=False;
//     // try stdout.print("obj1*=0x{x:0>16}: {}\n",.{@ptrToInt(obj1),obj1});
//     // try stdout.print("obj2*=0x{x:0>16}: {}\n",.{@ptrToInt(obj2),obj2});
//     // try stdout.print("obj3*=0x{x:0>16}: {}\n",.{@ptrToInt(obj3),obj3});
//     // try stdout.print("obj4*=0x{x:0>16}: {}\n",.{@ptrToInt(obj4),obj4});
//     try arena.verify(expected);
// }

