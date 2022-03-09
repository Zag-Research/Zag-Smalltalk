const page_size = @import("std").mem.page_size/@sizeOf(Header);
const Object = @import("object.zig").Object;
const native_endian = @import("builtin").target.cpu.arch.endian();
pub const Format = enum(u8) {
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
pub inline fn header(length : u16, format : Format, classIndex : u16) Header {
    return Header {
        .length = length,
        .format = format,
        .hash = 0,
        .classIndex = classIndex,
    };
}
const heapMethods = struct {
    pub inline fn setHash(self: *Header,hash: u24) Header {
        self.hash=hash;
        return self.*;
    }
    pub inline fn instVars(self: HeapPtr) []Object {
        if (self.format.hasInstVars()) {
            const size = self.length;
            return self.asObjectArray()[1..size+1];
        } else return &[0]Object{};
    }
    pub inline fn indexables(self: HeapPtr,T:type) ![]T {
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
test "Header structure" {
    const expect = @import("std").testing.expect;
    try expect(@sizeOf(Header) == 8);
    const hdr = header(17, Format.object, 35).setHash(0x123);
    try expect(@bitCast(u64, hdr) == 0x0011010001230023);
}
