const std = @import("std");
const builtin = @import("builtin");
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
const bitToRepresent = @import("utilities.zig").bitToRepresent;
const largerPowerOf2Not1 = @import("utilities.zig").largerPowerOf2Not1;
const Header = @import("heap.zig").Header;
const header = @import("heap.zig").header;
const Format = @import("heap.zig").Format;
const Age = @import("heap.zig").Age;
const HeaderArray = @import("heap.zig").HeaderArray;
const HeapPtr = @import("heap.zig").HeapPtr;
pub inline fn arenaFree(stackPointer: [*]const Object, heapPointer: HeaderArray) isize {
    return @divFloor(@bitCast(isize,(@ptrToInt(stackPointer)-%@ptrToInt(heapPointer))),@sizeOf(Object));
}
test "first" {
    std.debug.print("start\n",.{});
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

const threadAvail = thread.avail_size;
const nursery_size = threadAvail/7/@sizeOf(Object);
const teen_size = (threadAvail-@sizeOf(NurseryArena))/2/@sizeOf(Object);
pub const NurseryArena = extern struct {
    const Self = @This();
    hp: HeaderArray,
    sp: [*]Object,
    thread: *thread.Thread,
    heapArea: [nursery_size-field_size/@sizeOf(Header)]Header,
    const field_size = @sizeOf(HeaderArray)+@sizeOf([*]Object);
    comptime {
        if (checkEqual(@sizeOf(NurseryArena),nursery_size*@sizeOf(Header))) |s|
            @compileError("Modify NurseryArena.heap to make @sizeOf(NurseryArena) == " ++ s);
    }
    pub fn new() NurseryArena {
        return NurseryArena {
            .hp = undefined,
            .sp = undefined,
            .thread = undefined,
            .heapArea = undefined,
        };
    }
    pub fn init(self: *Self, thr: *thread.Thread) void {
        self.hp = @ptrCast(HeaderArray,@alignCast(@alignOf(u64),&self.heapArea[0]));
        self.sp = self.endOfStack();
        self.thread = thr;
    }
    pub inline fn endOfStack(self: *Self) [*]Object {
        return @intToPtr([*]Object,@ptrToInt(&self.heapArea[0]))+nursery_size;
    }
    pub inline fn getHp(self: *Self) HeaderArray {
        return self.hp;
    }
    inline fn alloc(self: *Self, totalSize: usize, hp: HeaderArray, sp: [*]Object) !HeapPtr {
        const result = hp;
        const end = @ptrCast(HeapPtr,result + totalSize);
        if (@ptrCast(HeaderArray,sp)<=end) return error.HeapFull;
        self.hp = end;
    }
    inline fn initAllocation(result: [*]Object, classIndex: class.ClassIndex, form: Format, size: usize, age: Age, fill: Object) Object {
        const hash = if (builtin.is_test) 0 else @truncate(u24,@truncate(u32,@ptrToInt(result))*%object.u32_phi_inverse>>8);
        var totalSize = 1+size;
        const head = header(@intCast(u12,size),form,classIndex,hash,age);
        mem.set(Object,result[1..totalSize],fill);
//        if (totalSize>size+1) result[ivSize+1] = @bitCast(Object,aSize);
        return @bitCast(Object,head);
    }
    pub fn allocObject(self : *Self, classIndex : class.ClassIndex, ivSize : usize) Object {
        var result = self.alloc(ivSize+1);
        result[0] = initAllocation(result,classIndex, Format.object, ivSize, Age.nursery, Nil);
        return Object.from(@ptrCast(HeapPtr,result));
    }
    pub fn allocArray(self : *Self, classIndex : class.ClassIndex, ivSize : usize, arraySize : usize, comptime T: type) !HeapPtr {
        var form = if (ivSize>0) Format.object else Format.none;
        const width = @sizeOf(T);
        const aSize = (arraySize*width+objectWidth-width)/objectWidth;
        return self.alloc(classIndex, form, ivSize, aSize, object.ZERO);
    }
};

// pub const TeenArena = extern struct {
//     const Self = @This();
//     free: HeaderArray,
//     heap: [teen_size-field_size/@sizeOf(Header)]Header,
//     const field_size = @sizeOf(HeaderArray);
//     comptime {
//         if (checkEqual(@sizeOf(TeenArena),teen_size*@sizeOf(Header))) |s|
//             @compileError("Modify TeenArena.heap to make @sizeOf(TeenArena) == " ++ s);
//     }
//     pub fn new() TeenArena {
//         return Self {
//             .free = undefined,
//             .heap = undefined,
//         };
//     }
//     pub fn init(self: *Self, otherTeenHeap: *Self) void {
//         _ = otherTeenHeap;
//         self.free = @ptrCast(HeaderArray,&self.heap[0]);
//     }
// };
// const HeapAllocation = extern struct {
//     flags: u64,
//     next: ?*HeapAllocation,
//     mem: [size]u8,
//     const Self = @This();
//     const field_size = @sizeOf(u64)+@sizeOf(?*HeapAllocation);
//     const page_allocation_size = 64*1024;
//     const size = page_allocation_size - field_size;
//     const maxObjects = size/@sizeOf(Header);
//     fn init() Self {
//         return Self {
//             .flags = 0,
//             .mem = undefined,
//         };
//     }
//     fn sweep(self: *Self) void {
//         var ptr = @ptrCast(HeaderArray,&self.mem[0]);
//         const end = ptr+maxObjects;
//         while (ptr<end) {
//             unreachable;
//         }
//     }
// };
pub var globalArena = GlobalArena.init();
pub const GlobalArena = struct {
    const Self = @This();
//     freeLists: [nFreeLists]FreeListPtr,
//     const nFreeLists = bitToRepresent(Header.maxLength);
//     const FreeList = struct {
//         header: Header,
//         next: FreeListPtr,
//     };
//     const FreeListPtr = ?*FreeList;
//     const allocationUnit = Header.maxLength; // size in u64 units including the header
    fn init() Self {
        return Self {
//             .freeLists = [_]FreeListPtr{null}**nFreeLists,
        };
    }
//     fn alloc(arena: *Self, totalSize: usize) !HeapPtr {
//         _ = arena; _ = totalSize;
//         unreachable;
//     }
//    pub fn promote(obj: Object) !Object {
//        if (!obj.isHeap()) return obj;
//        unreachable;
// //       @memcpy(@ptrCast([*]u8,result),@ptrCast([*]const u8,ptr),totalSize*8);
// //       return result.asObject();
//     }
//     fn findAllocationList(target: u16) usize {
//         if (target > Header.maxLength) return 0;
//         if (target < 2) return 1;
//         return bitToRepresent(target-1);
//     }
};
// test "findAllocationList" {
//     const ee = std.testing.expectEqual;
//     const findAllocationList = GlobalArena.findAllocationList;
//     try ee(findAllocationList(1),1);
//     try ee(findAllocationList(2),1);
//     try ee(findAllocationList(3),2);
//     try ee(findAllocationList(4),2);
//     try ee(findAllocationList(17),5);
//     try ee(findAllocationList(Header.maxLength),12);
//     try ee(findAllocationList(Header.maxLength+1),0);
// }
// // inline fn initAllocation(result: [*]Object, classIndex: class.ClassIndex, form: Format, ivSize: usize, aSize: usize, fill: Object) !HeapPtr{
// //     var totalSize = 1+ivSize;
// //     const size = 0;
// //     const hash = if (builtin.is_test) 0 else @truncate(u24,@truncate(u32,@ptrToInt(result))*%object.u32_phi_inverse>>8);
// //     const head = header(@intCast(u12,size),form,classIndex,hash,age);
// //     mem.set(Object,result[1..totalSize],fill);
// //     if (totalSize>size+1) result[ivSize+1] = @bitCast(Object,aSize);
// //     result[0] = @bitCast(Object,head);
// //     return @ptrCast(HeapPtr,result);
// // }
// // inline fn allocObject(self : *Self, classIndex : class.ClassIndex, ivSize : usize) !HeapPtr {
// //     return self.alloc(classIndex, Format.object, ivSize, 0, object.Nil);
// // }
// // inline fn allocArray(self : *Self, classIndex : class.ClassIndex, ivSize : usize, arraySize : usize, comptime T: type) !HeapPtr {
// //     var form = if (ivSize>0) Format.object else Format.none;
// //     const width = @sizeOf(T);
// //     const aSize = (arraySize*width+objectWidth-width)/objectWidth;
// //     return self.alloc(classIndex, form, ivSize, aSize, object.ZERO);
// // }
// // inline fn allocStruct(self : *Self, classIndex : class.ClassIndex, comptime T: type, extra: usize, fill: Object) !*T {
// //     const ivSize = (@sizeOf(T)+objectWidth-1)/objectWidth;
// //     const aSize = (extra+objectWidth-1)/objectWidth;
// //     return @ptrCast(*T,@alignCast(@alignOf(T),try self.alloc(classIndex, if (aSize>0) Format.both else Format.object, ivSize, aSize, fill)));
// // }
// fn TempArena(comptime size: usize) type {
//     return extern struct{
//         const Self = @This();
//         heap: [size]Object,
//         nextFree: usize,
//         pub fn init() Self {
//             return Self {
//                 .heap = undefined,
//                 .nextFree = 0,
//             };
//         }
//         fn alloc(self: *Self, totalSize: usize) !HeapPtr {
//             if (self.nextFree+totalSize>size) {
//                 std.debug.print("nextFree = {} totalSize = {} size = {}\n",.{self.nextFree,totalSize,size});
//                 return error.HeapFull;
//             }
//             const result = @ptrCast(HeapPtr,@alignCast(@alignOf(HeapPtr),&self.heap[self.nextFree]));
//             self.nextFree += totalSize;
//             return result;
//         }
//         fn verify(self: *Self, expected: []Object) !void {
//             if (self.nextFree!=size) {
//                 std.debug.print("nextFree = {} expected {}\n",.{self.nextFree,size});
//                 return error.HeapDidnotEndUpAtEnd;
//             }
//             var ok=true;
//             for (expected) |item, index| {
//                 if (!self.heap[index].equals(item) and
//                         !(item.isDouble() and std.math.fabs(self.heap[index].to(f64)-item.to(f64))<0.00001)) {
//                     std.debug.print("comparing[{}] expected={} output={}\n",.{index,item,self.heap[index]});
//                     ok = false;
//                 }
//             }
//             if (!ok) return error.OutputDiffered;
//         }
//     };
// }
// test "temp arena" {
//     const testing = std.testing;
//     var arena = TempArena(5).init();
//     const testArena = arena.asArena();
//     const obj1 : HeapPtr = try testArena.allocObject(42,3,0,Object,Age.stack);
//     std.debug.print("obj1: 0x{x:0>16}\n",.{obj1.o().u()});
//     try testing.expectEqual(@alignOf(@TypeOf(obj1)),Object.alignment);
//     try testing.expect(obj1.asObject().isHeap());
//     try testing.expectEqual(obj1.inHeapSize(),4);
//     try testing.expectEqual(obj1.asObject().inHeapSize(),4);
//     try testing.expectEqual(Nil.inHeapSize(),0);
//     try testing.expectError(error.HeapFull,testArena.allocObject(42,3,0,Object,Age.stack));
// }
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

