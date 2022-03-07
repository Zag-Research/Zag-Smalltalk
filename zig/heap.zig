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
    pub inline fn instVars(self: HeapPtr) []Object {
        if (self.format.hasInstVars()) {
            const size = self.length;
            return self.asObjectArray[1..size+1];
        } else return &[0]Object{};
    }
    pub inline fn indexables(self: HeapPtr,T:type) ![]T {
        if (self.format.isIndexable()) {
            const size = self.length;
            return self.asObjectArray[1..size+1];
        } else return error.NotIndexable;
    }
    pub inline fn asObjectArray(self: HeapPtr) [*]Object {
        return @bitCast([*]Object,self);
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
    var header = Header{ .length = 17, .format = Format.object, .hash = 0x123, .classIndex = 35 };
    var asInt = @bitCast(u64, header);
    try expect(asInt == 0x0011010001230023);
}
pub const dummyHO = Header{ .length = 0, .format = Format.object, .hash = 0x123, .classIndex = 35 };
