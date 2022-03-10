const page_size = @import("std").mem.page_size/@sizeOf(Header);
const object = @import("object.zig");
const Object = object.Object;
const mem = @import("std").mem;
const Allocator = mem.Allocator;
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
            const size = self.length;
            return self.asObjectArray()[1..size+1];
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
        return Self {
            .tos = allocated.ptr+allocated.len,
            .heap = @ptrCast(HeapPtr,allocated.ptr),
            .allocated = allocated,
            .allocator = allocator,
            .arenaType = arenaType,
        };
    }
    pub fn deinit(self : *Self) void {
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
    heap : Arena,
    const Self=@This();
    const testing = @import("std").testing;
    fn init(expected:[]const Object) !Self {
        const output = try testing.allocator.alloc(Object,expected.len);
        const allocator = @import("std").heap.FixedBufferAllocator.init(mem.sliceAsBytes(output)).allocator();
        return Self {
            .expected = expected,
            .output = output,
            .heap = try Arena.init(allocator,expected.len,ArenaType.Test),
        };
    }
    fn deinit(self: *Self) void {
        self.heap.deinit();
        testing.allocator.free(self.output);
        self.* = undefined;
        //testing.allocator.deinit();
    }
    inline fn allocObject(self : *Self, classIndex : class.ClassIndex, iv_size : usize) !HeapPtr {
        return self.heap.allocObject(classIndex, iv_size);
    }
    inline fn allocRaw(self : *Self, classIndex : class.ClassIndex, array_size : usize, fill: anytype) !HeapPtr {
        return self.heap.allocRaw(classIndex, array_size, fill);
    }
    inline fn alloc(self : *Self, classIndex : class.ClassIndex, format: Format, iv_size : usize, array_size : usize, fill: anytype) !HeapPtr {
        return self.heap.alloc(classIndex,format,iv_size,array_size,fill);
    }
    fn isEqual(self: Self) bool {
        for (self.expected) |item, index| {
            if (@bitCast(u64,self.output[index]) != @bitCast(u64,item)) return false;
        }
        return true;
    }
    fn expectEqual(self: Self) !void {
        try testing.expect(self.isEqual());
    }
};
test "simple object allocator" {
    const h = header(3,Format.object,42,0);
    const expected = [_]Object{h.asObject(),object.True,object.Nil,object.False};
    var testArena = try TestArena.init(expected[0..]);
    defer testArena.deinit();
    const obj = try testArena.allocObject(42,3);
    const ivs = obj.instVars();
    ivs[0]=object.True;
    ivs[2]=object.False;
    try testArena.expectEqual();
}
