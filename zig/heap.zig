const std = @import("std");
const mem = std.mem;
const Allocator = mem.Allocator;
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const class = @import("class.zig");
const native_endian = @import("builtin").target.cpu.arch.endian();
pub const Format = enum(u8) {
    none = 0,
    object = InstVars,
    array = Indexable,
    both = InstVars + Indexable,
    weak = InstVars + Indexable + Weak,
    objectNP = InstVars + PointerFree,
    arrayNP = Indexable + PointerFree,
    bothNP = InstVars + Indexable + PointerFree,
    raw64 = Indexable_64,
    _,
    const Self = @This();
    const InstVars : u8 = 1;
    const Indexable : u8 = 2;
    const Weak : u8 = 4;
    const PointerFree : u8 = 8;
    const RawData : u8 = 16;
    const Indexable_64 : u8 = 17;
    const Indexable_32 : u8 = 18;
    const Indexable_16 : u8 = 20;
    const Indexable_8 : u8 = 24;
    const BaseFormat : u8 = 31;
    const Immutable : u8 = 32;
    pub inline fn weak(self: Self) Self {
        return @intToEnum(Self,(@bitCast(u8,self) & ~BaseFormat) + InstVars + Indexable + Weak);
    }
    pub inline fn base(self: Self) Self {
        return @intToEnum(Self,(@bitCast(u8,self) & ~BaseFormat));
    }
    pub inline fn object(self: Self) Self {
        return @intToEnum(Self,@bitCast(u8,self) + InstVars);
    }
    pub inline fn array(self: Self) Self {
        return @intToEnum(Self,@bitCast(u8,self) + Indexable);
    }
    pub inline fn raw(self: Self, T : type, size : usize) Self {
        switch (T) {
            u8,i8 => {return @intToEnum(Self,(@bitCast(u8,self) & ~BaseFormat) + Indexable_8 + ((-@intCast(isize,size))&7));},
            u16,i16 => {return @intToEnum(Self,(@bitCast(u16,self) & ~BaseFormat) + Indexable_16 + ((-@intCast(isize,size))&3));},
            u32,i32,f32 => {return @intToEnum(Self,(@bitCast(u32,self) & ~BaseFormat) + Indexable_32 + ((-@intCast(isize,size))&1));},
            u64,i64,f64 => {return @intToEnum(Self,(@bitCast(u64,self) & ~BaseFormat) + Indexable_64);},
            else => {return self;},
        }
    }
    pub inline fn hasInstVars(self: Self) bool {
        return @bitCast(u8, self) & InstVars != 0 and @bitCast(u8, self) & BaseFormat < RawData;
    }
    pub inline fn isIndexable(self: Self) bool {
        return @bitCast(u8, self) & Indexable != 0 or @bitCast(u8, self) & BaseFormat >= RawData;
    }
    pub inline fn hasBoth(self: Self) bool {
        return @bitCast(u8, self) & InstVars+Indexable == InstVars+Indexable and @bitCast(u8, self) & BaseFormat < RawData;
    }
    pub inline fn isWeak(self: Self) bool {
        return @bitCast(u8, self) & Weak != 0 and @bitCast(u8, self) & BaseFormat < RawData;
    }
    pub inline fn isPointerFree(self: Self) bool {
        return @bitCast(u8, self) & BaseFormat >= PointerFree;
    }
    pub inline fn hasPointers(self: Self) bool {
        return ~self.isPointerFree();
    }
    pub inline fn isImmutable(self: Self) bool {
        return @bitCast(u8, self) & Immutable != 0;
    }
    pub inline fn is_64(self: Self) bool {
        return @bitCast(u8, self) & RawData != 0 and @bitCast(u8, self) & BaseFormat == Indexable_64;
    }
    pub inline fn is_32(self: Self) bool {
        return @bitCast(u8, self) & RawData != 0 and @bitCast(u8, self) & BaseFormat >= Indexable_32 and @bitCast(u8, self) & BaseFormat <= Indexable_32 + 1;
    }
    pub inline fn is_16(self: Self) bool {
        return @bitCast(u8, self) & RawData != 0 and @bitCast(u8, self) & BaseFormat >= Indexable_16 and @bitCast(u8, self) & BaseFormat <= Indexable_16 + 3;
    }
    pub inline fn is_8(self: Self) bool {
        return @bitCast(u8, self) & RawData != 0 and @bitCast(u8, self) & BaseFormat >= Indexable_8 and @bitCast(u8, self) & BaseFormat <= Indexable_8 + 7;
    }
};
pub const HeapPtr = *Header;
const heapMethods = struct {
    pub inline fn asObject(self: Header) Object {
        return @bitCast(Object,self);
    }
    pub inline fn setHash(self: *Header,hash: u24) Header {
        self.hash=hash;
        return self.*;
    }
    pub inline fn setNArgs(self: *Header,args: u8) Header {
        self.hash=(self.hash & 0xffff)+(args<<16);
        return self.*;
    }
    pub inline fn instVars(self: HeapPtr) []Object {
        if (self.format.hasInstVars()) {
            const size = self.length;
            return self.asObjectArray()[1..size+1];
        } else return &[0]Object{};
    }
    pub inline fn indexables(self: HeapPtr,comptime T:type) ![]T {
        if (self.format.isIndexable()) {
            switch (T) {
                Object => {
                    const size = self.length;
                    if (self.format.hasInstVars()) {
                        const oa = self.asObjectArray();
                        const array_size = @bitCast(usize,oa[size+1]);
                        return oa[size+2..size+array_size+2];
                    } else {
                        return self.asObjectArray()[1..size+1];
                    }
                },
                else => {return error.NotImplemented;},
            }
        } else return error.NotIndexable;
    }
    pub inline fn asObjectArray(self: HeapPtr) [*]Object {
        return @ptrCast([*]Object,self);
    }
    pub inline fn derefForwarded(self: HeapPtr) HeapPtr {
        return @intToPtr(HeapPtr,-@bitCast(i64,self.*));
    }
    pub inline fn totalSize(self: HeapPtr) usize {
        if (self.format.hasBoth()) unreachable;
        var size = self.length;
        return size+1;
    }
};
const Header = switch (native_endian) {
    .Big => packed struct {
        length: u16,
        format: Format,
        hash: u24,
        classIndex: u16,
        usingnamespace heapMethods;
    },
    .Little => packed struct {
        classIndex: u16,
        hash: u24,
        format: Format,
        length: u16,
        usingnamespace heapMethods;
    },
};
pub inline fn header(length : u16, format : Format, classIndex : u16, hash: u24) Header {
    return Header {
        .length = length,
        .format = format,
        .hash = hash,
        .classIndex = classIndex,
    };
}
test "Header structure" {
    const expect = @import("std").testing.expect;
    try expect(@sizeOf(Header) == 8);
    const hdr = header(17, Format.object, 35,0x123);
    try expect(@bitCast(u64, hdr) == 0x0011010001230023);
}
const ArenaType = enum {
    Nursery,
    Teen,
    Global,
    Test,
};
const Arena = struct {
    heap: HeapPtr,
    tos: [*]Object,
    allocated: []Object,
    allocator: Allocator,
    arenaType: ArenaType,
    const Self = @This();
    pub fn init(allocator: Allocator,size:usize, arenaType: ArenaType) !Self {
        const allocated = allocator.alloc(Object,size) catch |err| return err;
        //try @import("std").io.getStdOut().writer().print("allocated ptr=0x{x} len={}\n",.{@ptrToInt(allocated.ptr),allocated.len});
        return Self {
            .tos = allocated.ptr+allocated.len,
            .heap = @ptrCast(HeapPtr,allocated.ptr),
            .allocated = allocated,
            .allocator = allocator,
            .arenaType = arenaType,
        };
    }
    pub fn deinit(self : *Self) void {
        //@import("std").io.getStdOut().writer().print("deallocate ptr=0x{x} len={}\n",.{@ptrToInt(self.allocated.ptr),self.allocated.len}) catch unreachable;
        self.allocator.free(self.allocated);
        self.* = undefined;
    }
    pub fn alloc(self : *Self, classIndex : class.ClassIndex, format: Format, iv_size : usize, array_size : usize, fill: anytype) !HeapPtr {
        var form = format.base();
        var size = iv_size + array_size;
        var totalSize = size+1;
        var asize = array_size;
        var fillPattern = @bitCast(Object,@as(u64,0));
        const objectWidth = @sizeOf(Object);
        var width : usize = objectWidth;
        switch (@TypeOf(fill)) {
            Object => {
                fillPattern = fill;
                if (iv_size>0) form=form.object();
                if (array_size>0 or format.isIndexable()) {
                    form=form.array();
                    if (array_size>32766) form=form.object(); // big arrays use Format.both
                }
                if (format.isWeak()) form=form.weak();
                if (form.hasBoth()) {
                    totalSize += 1;
                    size = iv_size;
                }
            },
            u8,i8,u16,i16,u32,i32,f32,u64,i64,f64 => |T| {
                width = @sizeOf(T);
                size = (size+objectWidth-width)/objectWidth;
                form = form.raw(T,size);
                asize = size;
                totalSize=size+1;
                if (size>32766) {
                    size=32767;
                    totalSize+=1;
                }
            },
            else => {},
        }
        const result = self.heap;
        self.heap = @ptrCast(HeapPtr,@ptrCast([*]Header,self.heap) + totalSize);
        const hash = if (self.arenaType==.Test) 0 else @intCast(u24,@ptrToInt(result)%16777213);
        const head = header(@intCast(u16,size),form,classIndex,hash);
        result.* = @bitCast(Object,head);
        const instVars = result.instVars();
        for (instVars) |_,index| {
            instVars[index]=fill;
        }
        if (totalSize>size+1) @ptrCast([*]Object,result)[iv_size+1] = @bitCast(Object,asize);
        if (form.isIndexable()) {
            var indexables = try result.indexables(@TypeOf(fill));
            for (indexables) |_,index| {
                indexables[index]=fill;
            }
        }
        return @ptrCast(HeapPtr,result);
    }
    pub inline fn allocObject(self : *Self, classIndex : class.ClassIndex, iv_size : usize) !HeapPtr {
        return self.alloc(classIndex, Format.none, iv_size, 0, object.Nil);
    }
    pub inline fn allocRaw(self : *Self, classIndex : class.ClassIndex, array_size : usize, fill: anytype) !HeapPtr {
        return self.alloc(classIndex, Format.none, 0, array_size, fill);
    }
};
const TestArena = struct {
    expected: []const Object,
    output: []Object,
    arena : Arena,
    const Self=@This();
    const testing = @import("std").testing;
    fn init(expected:[]const Object) !Self {
        const output = try testing.allocator.alloc(Object,expected.len);
        const allocator = @import("std").heap.FixedBufferAllocator.init(mem.sliceAsBytes(output)).allocator();
        return Self {
            .expected = expected,
            .output = output,
            .arena = try Arena.init(allocator,expected.len,ArenaType.Test),
        };
    }
    fn deinit(self: *Self) void {
        self.arena.deinit();
        testing.allocator.free(self.output);
        self.* = undefined;
        //testing.allocator.deinit();
    }
    inline fn allocObject(self : *Self, classIndex : class.ClassIndex, iv_size : usize) !HeapPtr {
        return self.arena.allocObject(classIndex, iv_size);
    }
    inline fn allocRaw(self : *Self, classIndex : class.ClassIndex, array_size : usize, fill: anytype) !HeapPtr {
        return self.arena.allocRaw(classIndex, array_size, fill);
    }
    inline fn alloc(self : *Self, classIndex : class.ClassIndex, format: Format, iv_size : usize, array_size : usize, fill: anytype) !HeapPtr {
        return self.arena.alloc(classIndex,format,iv_size,array_size,fill);
    }
    fn isEqual(self: Self) bool {
        for (self.expected) |item, index| {
            if (@bitCast(u64,self.output[index]) != @bitCast(u64,item)) {
                @import("std").io.getStdOut().writer().print("comparing[{}] expected=0x{x} output=0x{x}\n",
                                                             .{index,@bitCast(u64,item),@bitCast(u64,self.output[index])}) catch unreachable;
                return false;
            }
        }
        return true;
    }
    fn finish(self: *Self) void {
        testing.expect(self.isEqual()) catch @panic("output was not expected");
        self.deinit();
    }
};
test "simple object allocator" {
    const h = header(3,Format.object,42,0);
    const expected = [_]Object{h.asObject(),True,Nil,False};
    var testArena = try TestArena.init(expected[0..]);
    defer testArena.finish();
    const obj = try testArena.allocObject(42,3);
    const ivs = obj.instVars();
    ivs[0]=True;
    ivs[2]=False;
}
test "three object allocator" {
    const testing = @import("std").testing;
    const h1 = header(3,Format.object,42,0);
    const h2 = header(1,Format.both,43,0);
    const h3 = header(2,Format.array,44,0);
    const expected = [_]Object{h1.asObject(),True,Nil,False,h2.asObject(),True,@bitCast(Object,@as(u64,1)),False,h3.asObject(),Nil,True};
    var testArena = try TestArena.init(expected[0..]);
    defer testArena.finish();
    const obj1 = try testArena.allocObject(42,3);
    const ivs1 = obj1.instVars();
    try testing.expectEqual(ivs1.len,3);
    ivs1[0]=True;
    ivs1[2]=False;
    const obj2 = try testArena.alloc(43,Format.both,1,1,Nil);
    const ivs2 = obj2.instVars();
    try testing.expectEqual(ivs2.len,1);
    ivs2[0]=True;
    const idx2 = try obj2.indexables(Object);
    try testing.expectEqual(idx2.len,1);
    idx2[0]=False;
    const obj3 = try testArena.alloc(44,Format.array,0,2,Nil);
    const ivs3 = obj3.instVars();
    try testing.expectEqual(ivs3.len,0);
    const idx3 = try obj3.indexables(Object);
    try testing.expectEqual(idx3.len,2);
    idx3[1]=True;
    // with the following line commented, the test crashes in the testing.allocator.free()
    // I've printed out the allocated slice after allocation and before freeing, and they are the same
    // uncommented, it works fine
    @import("std").io.getStdOut().writer().print("idx3 ptr=0x{x} len={}\n",.{@ptrToInt(idx3.ptr),idx3.len}) catch unreachable;
}