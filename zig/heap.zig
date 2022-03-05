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
    const Self = @This();
    const InstVars = 1;
    const Indexable = 2;
    const Weak = 4;
    const PointerFree = 8;
    const RawData = 16;
    const Indexable_64 = 17;
    const Indexable_32 = 18;
    const Indexable_16 = 20;
    const Indexable_8 = 24;
    const BaseFormat = 31;
    const Immutable = 32;
    pub fn hasInstVars(self: Self) bool {
        return @as(u8, self) & InstVars != 0 and @as(u8, self) & BaseFormat < RawData;
    }
    pub fn isIndexable(self: Self) bool {
        return @as(u8, self) & Indexable != 0 or @as(u8, self) & BaseFormat >= RawData;
    }
    pub fn isWeak(self: Self) bool {
        return @as(u8, self) & Weak != 0 and @as(u8, self) & BaseFormat < RawData;
    }
    pub fn isPointerFree(self: Self) bool {
        return @as(u8, self) & BaseFormat >= PointerFree;
    }
    pub fn isImmutable(self: Self) bool {
        return @as(u8, self) & Immutable != 0;
    }
    pub fn is_64(self: Self) bool {
        return @as(u8, self) & RawData != 0 and @as(u8, self) & BaseFormat == Indexable_64;
    }
    pub fn is_32(self: Self) bool {
        return @as(u8, self) & RawData != 0 and @as(u8, self) & BaseFormat >= Indexable_32 and @as(u8, self) & BaseFormat <= Indexable_32 + 1;
    }
    pub fn is_16(self: Self) bool {
        return @as(u8, self) & RawData != 0 and @as(u8, self) & BaseFormat >= Indexable_16 and @as(u8, self) & BaseFormat <= Indexable_16 + 3;
    }
    pub fn is_8(self: Self) bool {
        return @as(u8, self) & RawData != 0 and @as(u8, self) & BaseFormat >= Indexable_8 and @as(u8, self) & BaseFormat <= Indexable_8 + 7;
    }
};
pub const Header = switch (native_endian) {
    .Big => packed struct {
        length: u16,
        format: Format,
        hash: u24,
        classIndex: u16,
    },
    .Little => packed struct {
        classIndex: u16,
        hash: u24,
        format: Format,
        length: u16,
    },
};
test "Header structure" {
    const expect = @import("std").testing.expect;
    try expect(@sizeOf(Header) == 8);
    var header = Header{ .length = 17, .format = Format.object, .hash = 0x123, .classIndex = 35 };
    var asInt = @bitCast(u64, header);
    try expect(asInt == 0x0011010001230023);
}
pub const HeapObject = packed struct {
    header: Header,
    objects: [0]Object,
    const Self = @This();
    pub inline fn instVars(self: Self) []Object {
        if (self.header.format.hasInstVars()) {
            const size = self.header.length;
            return self.objects[0..size];
        } else return &[0]Object{};
    }
    pub inline fn indexables(self: Self,T:type) ![]T {
        if (self.header.format.isIndexable()) {
            const size = self.header.length;
            return self.objects[0..size];
        } else return error.NotIndexable;
    }
};

test "sizeof" {
    const expect = @import("std").testing.expect;
    try expect(@sizeOf(HeapObject) == 8);
}
