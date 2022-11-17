const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const checkEqual = @import("utilities.zig").checkEqual;
const thread = @import("thread.zig");
const object = @import("object.zig");
const Object = object.Object;
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
    vtable:  Arena.Vtable,
    hp: HeaderArray,
    sp: [*]Object,
    heapArea: [nursery_size-field_size/@sizeOf(Header)]Header,
    const field_size = @sizeOf(Arena.Vtable)+@sizeOf(HeaderArray)+@sizeOf([*]Object);
    comptime {
        if (checkEqual(@sizeOf(NurseryArena),nursery_size*@sizeOf(Header))) |s|
            @compileError("Modify NurseryArena.heap to make @sizeOf(NurseryArena) == " ++ s);
    }
    pub fn setThread(_: *Self, _: *thread.Thread) void {
    }
    pub fn new() NurseryArena {
        return NurseryArena {
            .vtable = vtable,
            .heapArea = undefined,
            .hp = undefined,
            .sp = undefined,
        };
    }
    pub fn init(self: *Self) void {
        self.hp = @ptrCast(HeaderArray,@alignCast(@alignOf(u64),&self.heapArea[0]));
        self.sp = self.endOfStack();
    }
    pub inline fn endOfStack(self: *Self) [*]Object {
        return @intToPtr([*]Object,@ptrToInt(&self.heapArea[0]))+nursery_size;
    }
    const vtable =  Arena.Vtable {
        .alloc = alloc,
    };
    pub inline fn asArena(self: *Self) *Arena {
        return @ptrCast(*Arena,self);
    }
    pub inline fn getHp(self: *Self) HeaderArray {
        return self.hp;
    }
    fn alloc(arena: *Arena, totalSize: usize) !HeapPtr {
        _ = arena; _ = totalSize;
        unreachable;
        // const self = @ptrCast(*Self,@alignCast(@alignOf(Self),arena));
        // const result = @alignCast(@alignOf(HeaderArray),self.hp);
        // const end = @ptrCast(HeapPtr,result + totalSize);
        // if (self.space()<0) {
        //     const stdout = std.io.getStdOut().writer();
        //     stdout.print("classIndex={} totalSize={} end=0x{X:0>16} toh=0x{X:0>16}\n",.{classIndex,totalSize,@ptrToInt(end),@ptrToInt(self.toh)}) catch unreachable;
        //     return error.HeapFull;
        // }
        // self.hp = end;
        // const hash = if (builtin.is_test) 0 else @truncate(u24,@truncate(u32,@ptrToInt(result))*%object.u32_phi_inverse>>8);
        // const head = header(@intCast(u12,size),form,classIndex,hash,age);
        // result.* = @bitCast(Object,head);
        // mem.set(Object,@ptrCast([*]Object,result)[1..totalSize],fill);
        // if (totalSize>size+1) @ptrCast([*]Object,result)[iv_size+1] = @bitCast(Object,asize);
        // return @ptrCast(HeapPtr,result);
    }
};

pub const TeenArena = extern struct {
    const Self = @This();
    vtable:  Arena.Vtable,
    free: HeaderArray,
    heap: [teen_size-field_size/@sizeOf(Header)]Header,
    const field_size = @sizeOf(Arena.Vtable)+@sizeOf(HeaderArray);
    comptime {
        if (checkEqual(@sizeOf(TeenArena),teen_size*@sizeOf(Header))) |s|
            @compileError("Modify TeenArena.heap to make @sizeOf(TeenArena) == " ++ s);
    }
    pub fn new() TeenArena {
        return Self {
            .vtable = vtable,
            .free = undefined,
            .heap = undefined,
        };
    }
    pub fn init(self: *Self, otherTeenHeap: *Self) void {
        _ = otherTeenHeap;
        self.free = @ptrCast(HeaderArray,&self.heap[0]);
    }
    const vtable =  Arena.Vtable {
        .alloc = alloc,
    };
    pub inline fn asArena(self: *Self) *Arena {
        return @ptrCast(Arena,self);
    }
    fn alloc(arena: *Arena, totalSize: usize) !HeapPtr {
        _ = arena; _ = totalSize;
        unreachable;
    }
};
const HeapAllocation = extern struct {
    flags: u64,
    next: ?*HeapAllocation,
    mem: [size]u8,
    const Self = @This();
    const size = std.mem.page_size - @sizeOf(?*u8);
    const maxObjects = size/@sizeOf(Header);
    fn sweep(self: *Self) void {
        var ptr = @ptrCast(HeaderArray,&self.mem[0]);
        const end = ptr+maxObjects;
        while (ptr<end) {
            unreachable;
        }
    }
};
pub var globalArena = GlobalArena.init();
pub const GlobalArena = struct {
    const Self = @This();
    vtable:  Arena.Vtable,
    freeLists: [nFreeLists]FreeListPtr,
    pub fn init() GlobalArena {
        var result = GlobalArena {
            .vtable = Arena.Vtable {
                .alloc = alloc,
            },
            .freeLists = [_]FreeListPtr{null}**nFreeLists,
        };
        return result;
    }
    pub inline fn asArena(self: *Self) *Arena {
        return @ptrCast(*Arena,self);
    }
    fn alloc(arena: *Arena, totalSize: usize) !HeapPtr {
        _ = arena; _ = totalSize;
        unreachable;
    }
   pub fn promote(obj: Object) !Object {
       if (!obj.isHeap()) return obj;
       unreachable;
//       @memcpy(@ptrCast([*]u8,result),@ptrCast([*]const u8,ptr),totalSize*8);
//       return result.asObject();
    }
    const FreeList = struct {
        header: Header,
        next: FreeListPtr,
    };
    const FreeListPtr = ?*FreeList;
    const nFreeLists = bitToRepresent(@as(u16,Header.maxLength));
    comptime {std.debug.print("nFreeLists={}\n",.{nFreeLists});}
    const allocationUnit = Header.maxLength; // size in u64 units including the header
    fn findAllocationList(target: u16) usize {
        if (target > 1<<(nFreeLists-1)) return 0;
        return bitToRepresent(target);
    }
};
test "findAllocationList" {
    const ee = std.testing.expectEqual;
    const findAllocationList = GlobalArena.findAllocationList;
    try ee(findAllocationList(1),1);
    try ee(findAllocationList(2),1);
    try ee(findAllocationList(3),2);
    try ee(findAllocationList(4),2);
    if (std.mem.page_size < 8192)
        {try ee(findAllocationList(400),0);}
    else if (std.mem.page_size < 16384)
        {try ee(findAllocationList(800),0);}
    else
        try ee(findAllocationList(1400),0);
}
pub const Arena = extern struct {
    vtable: Vtable,
    const Self = Arena;
    const objectWidth = @sizeOf(Object);
    const Vtable = extern struct {
        alloc: *const fn(self: *Self, totalSize: usize) anyerror!HeapPtr,
    };
    fn alloc(self: *Arena, classIndex: class.ClassIndex, form: Format, iv_size: usize, asize: usize, size: usize, totalSize: usize, fill: Object, age: Age) !HeapPtr{
        const result = @ptrCast([*]Object,try self.vtable.alloc(self,totalSize));
        const hash = if (builtin.is_test) 0 else @truncate(u24,@truncate(u32,@ptrToInt(result))*%object.u32_phi_inverse>>8);
        const head = header(@intCast(u12,size),form,classIndex,hash,age);
        mem.set(Object,result[1..totalSize],fill);
        if (totalSize>size+1) result[iv_size+1] = @bitCast(Object,asize);
        result[0] = @bitCast(Object,head);
        return @ptrCast(HeapPtr,result);
    }
    pub fn allocObjectX(self : *Self, classIndex : class.ClassIndex, format: Format, iv_size : usize, array_size : usize, age: Age) !HeapPtr {
        var form = format.noBase();
        var totalSize = iv_size + array_size + 1;
        if (iv_size>0) form=form.object();
        if (array_size>0) {
            form=form.array();
            if (array_size>32766) form=form.object(); // big arrays use Format.both
        }
        if (format.isWeak()) form=form.weak();
        if (form.hasBoth()) totalSize += 1;
        const size = if (form.hasInstVars()) iv_size else array_size;
        return self.alloc(classIndex, form, iv_size, array_size, size, totalSize, object.Nil, age);
    }
    pub fn allocObject(self : *Self, classIndex : class.ClassIndex, iv_size : usize, array_size : usize, comptime T: type, age: Age) !HeapPtr {
        _ = iv_size;
        const width = @sizeOf(T);
        const asize = (array_size*width+objectWidth-width)/objectWidth;
        const form = Format.none.raw(T,array_size);
        const size = @min(asize,Header.maxLength);
        var totalSize = asize+@as(usize,if (size<asize) 2 else 1);
        return self.alloc(classIndex, form, 0, asize, size, totalSize, object.ZERO, age);
    }
    pub fn allocStruct(self : *Self, classIndex : class.ClassIndex, comptime T: type, fill: Object, age: Age) !*T {
        const asize = (@sizeOf(T)+objectWidth-1)/objectWidth;
        const form = Format.object;
        return @ptrCast(*T,@alignCast(8,try self.alloc(classIndex, form, asize, 0, asize, asize+1, fill, age)));
    }
};
fn TempArena(comptime size: usize) type {
    return extern struct{
        const Self = @This();
        vtable : Arena.Vtable,
        heap: [size]Object,
        ptr: usize,
        pub fn init() Self {
            return Self {
                .vtable = Arena.Vtable {
                    .alloc = alloc,
                },
                .heap = undefined,
                .ptr = 0,
            };
        }
        pub inline fn asArena(self: *Self) *Arena {
            return @ptrCast(*Arena,self);
        }
        fn alloc(arena: *Arena, totalSize: usize) !HeapPtr {
            const self = @ptrCast(*Self,@alignCast(@alignOf(Self),arena));
            if (self.ptr+totalSize>size) {
                std.debug.print("ptr = {} totalSize = {} size = {}\n",.{self.ptr,totalSize,size});
                return error.HeapFull;
            }
            const result = @ptrCast(HeapPtr,@alignCast(@alignOf(HeapPtr),&self.heap[self.ptr]));
            self.ptr += totalSize;
            return result;
        }
        fn verify(self: *Self, expected: []Object) !void {
            if (self.ptr!=size) {
                std.debug.print("ptr = {} expected {}\n",.{self.ptr,size});
                return error.HeapDidnotEndUpAtEnd;
            }
            var ok=true;
            for (expected) |item, index| {
                if (!self.heap[index].equals(item) and
                        !(item.isDouble() and std.math.fabs(self.heap[index].to(f64)-item.to(f64))<0.00001)) {
                    std.debug.print("comparing[{}] expected={} output={}\n",.{index,item,self.heap[index]});
                    ok = false;
                }
            }
            if (!ok) return error.OutputDiffered;
        }
    };
}
test "temp arena" {
    const testing = std.testing;
    var arena = TempArena(5).init();
    const testArena = arena.asArena();
    const obj1 : HeapPtr = try testArena.allocObject(42,3,0,Object,Age.stack);
    try testing.expectEqual(@alignOf(@TypeOf(obj1)),Object.alignment);
    try testing.expect(obj1.asObject().isHeap());
    try testing.expectEqual(obj1.inHeapSize(),4);
    try testing.expectError(error.HeapFull,testArena.allocObject(42,3,0,Object,Age.stack));
}
test "slicing" {
    const testing = std.testing;
    var arena = TempArena(16).init();
    const testArena = arena.asArena();
    const hp0 = try testArena.allocObject(42,1,0,Object,Age.stack);
    try testing.expectEqual(hp0.arrayAsSlice(u8).len,0);
    const hp1 = try testArena.allocObject(42,1,2,Object,Age.stack);
    const obj1 = hp1.asObject();
    try testing.expectEqual(hp1.arrayAsSlice(u8).len,16);
    try testing.expectEqual(obj1.arrayAsSlice(u8).len,16);
    try testing.expectEqual(hp1.arrayAsSlice(u8),obj1.arrayAsSlice(u8));
}

test "one object #1 allocator" {
    const testing = std.testing;
    const h1 = header(3,Format.object,42,0,Age.stack);
    try testing.expectEqual(@alignCast(8,&h1).inHeapSize(),4);
    const expected = ([_]Object{
        h1.o(),True,Nil,False,
    })[0..];
    var arena = TempArena(expected.len).init();
    const testArena = arena.asArena();
    const obj1 = try testArena.allocObject(42,3,0,Object,Age.stack);
    try testing.expectEqual(obj1.inHeapSize(),4);
    const ivs1 = obj1.instVars();
    try testing.expectEqual(ivs1.len,3);
    ivs1[0]=True;
    ivs1[2]=False;
    try arena.verify(expected);
}
test "one object #2 allocator" {
    const testing = std.testing;
    const h2 = header(1,Format.both,43,0,Age.stack);
    try testing.expectEqual(@alignCast(8,&h2).inHeapSize(),4);
    const expected = ([_]Object{
        h2.o(),True,Nil,@bitCast(Object,@as(u64,1)),False,Nil,Nil,
    })[0..];
    var arena = TempArena(expected.len).init();
    const testArena = arena.asArena();
    const obj2 = try testArena.allocObject(43,2,3,Object,Age.stack);
    try testing.expectEqual(obj2.inHeapSize(),7);
    const ivs2 = obj2.instVars();
    try testing.expectEqual(ivs2.len,2);
    ivs2[0]=True;
    const idx2 = obj2.indexables(Object);
    try testing.expectEqual(idx2.len,3);
    idx2[0]=False;
    try arena.verify(expected);
}
test "one object #3 allocator" {
    const testing = std.testing;
    const h3 = header(2,Format.array,44,0,Age.stack);
    try testing.expectEqual(@alignCast(8,&h3).inHeapSize(),3);
    const expected = ([_]Object{
        h3.o(),Nil,True,
    })[0..];
    var arena = TempArena(expected.len).init();
    const testArena = arena.asArena();
    const obj3 = try testArena.allocObject(44,0,2,Object,Age.stack);
    try testing.expectEqual(obj3.inHeapSize(),3);
    const ivs3 = obj3.instVars();
    try testing.expectEqual(ivs3.len,0);
    const idx3 = obj3.indexables(Object);
    try testing.expectEqual(idx3.len,2);
    idx3[1]=True;
    try arena.verify(expected);
}
test "one object #4 allocator" {
    const testing = std.testing;
    const h4 = header(2,Format.array,44,0,Age.stack);
    try testing.expectEqual(@alignCast(8,&h4).inHeapSize(),3);
    const expected = ([_]Object{
        h4.o(),@bitCast(Object,@as(u64,0)),@bitCast(Object,@as(u64,1)),
    })[0..];
    var arena = TempArena(expected.len).init();
    const testArena = arena.asArena();
    const obj4 = try testArena.allocObject(45,0,2,u64,Age.stack);
    try testing.expectEqual(obj4.inHeapSize(),3);
    const ivs4 = obj4.instVars();
    try testing.expectEqual(ivs4.len,0);
    const idx4 = obj4.indexables(i64);
    try testing.expectEqual(idx4.len,2);
    idx4[1]=1;
    try arena.verify(expected);
}
test "four object allocator" {
    const testing = std.testing;
    const h1 = header(3,Format.object,42,0,Age.stack);
    try testing.expectEqual(@alignCast(8,&h1).inHeapSize(),4);
    const h2 = header(1,Format.both,43,0,Age.stack);
    try testing.expectEqual(@alignCast(8,&h2).inHeapSize(),4);
    const h3 = header(2,Format.array,44,0,Age.stack);
    try testing.expectEqual(@alignCast(8,&h3).inHeapSize(),3);
    const h4 = header(2,Format.array,44,0,Age.stack);
    try testing.expectEqual(@alignCast(8,&h4).inHeapSize(),3);
    const expected = ([_]Object{
        h1.o(),True,Nil,False,
        h2.o(),True,@bitCast(Object,@as(u64,1)),False,
        h3.o(),Nil,True,
        h4.o(),@bitCast(Object,@as(u64,0)),@bitCast(Object,@as(u64,1)),
    })[0..];
    var arena = TempArena(expected.len).init();
    const testArena = arena.asArena();
    const obj1 = try testArena.allocObject(42,3,0,Object,Age.stack);
    try testing.expectEqual(obj1.inHeapSize(),4);
    const obj2 = try testArena.allocObject(43,1,1,Object,Age.stack);
    try testing.expectEqual(obj2.inHeapSize(),4);
    const obj3 = try testArena.allocObject(44,0,2,Object,Age.stack);
    try testing.expectEqual(obj3.inHeapSize(),3);
    const obj4 = try testArena.allocObject(45,0,2,u64,Age.stack);
    try testing.expectEqual(obj4.inHeapSize(),3);
    const ivs4 = obj4.instVars();
    try testing.expectEqual(ivs4.len,0);
    const idx4 = obj4.indexables(i64);
    try testing.expectEqual(idx4.len,2);
    idx4[1]=1;
    const ivs3 = obj3.instVars();
    try testing.expectEqual(ivs3.len,0);
    const idx3 = obj3.indexables(Object);
    try testing.expectEqual(idx3.len,2);
    idx3[1]=True;
    const ivs2 = obj2.instVars();
    try testing.expectEqual(ivs2.len,1);
    ivs2[0]=True;
    const idx2 = obj2.indexables(Object);
    try testing.expectEqual(idx2.len,1);
    idx2[0]=False;
    const ivs1 = obj1.instVars();
    try testing.expectEqual(ivs1.len,3);
    ivs1[0]=True;
    ivs1[2]=False;
    // try stdout.print("obj1*=0x{x:0>16}: {}\n",.{@ptrToInt(obj1),obj1});
    // try stdout.print("obj2*=0x{x:0>16}: {}\n",.{@ptrToInt(obj2),obj2});
    // try stdout.print("obj3*=0x{x:0>16}: {}\n",.{@ptrToInt(obj3),obj3});
    // try stdout.print("obj4*=0x{x:0>16}: {}\n",.{@ptrToInt(obj4),obj4});
    try arena.verify(expected);
}

