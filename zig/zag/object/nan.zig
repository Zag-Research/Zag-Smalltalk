const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const math = std.math;
const zag = @import("../zag.zig");
const config = zag.config;
const assert = std.debug.assert;
const debugError = false;
const object = zag.object;
const ClassIndex = object.ClassIndex;
const heap = zag.heap;
const HeapObjectPtr = heap.HeapObjectPtr;
pub const Object = packed struct(u64) {
    h0: u16, // align(8),
    h1: u16,
    classIndex: ClassIndex,
    tag: Group,
    pub const Group = enum(u16) {
        immediates = 0xfff0,
        heapThunk = 0xfff5,
        nonLocalThunk,
        heap,
        smallIntMin,
        smallIntNeg_2,
        smallIntNeg_3,
        smallIntNeg_4,
        smallInt0,
        smallIntPos_6,
        smallIntPos_7,
        smallIntMax,
        _,
        const Self = @This();
        inline fn base(cg: Self) u64 {
            return @as(u64, @intFromEnum(cg)) << 48;
        }
        inline fn u(cg: Self) u16 {
            return @intFromEnum(cg);
        }
    };
    const Negative_Infinity: u64 = g(.immediates); //0xfff0000000000000;
    const Start_of_Heap_Objects: u64 = g(.heap);
    inline fn of(comptime v: u64) object.Object {
        return @bitCast(v);
    }
    inline fn oImm(c: ClassIndex, h: u32) object.Object {
        return @bitCast(imm(c, h));
    }
    inline fn imm(c: ClassIndex, h: u32) u64 {
        return g(.immediates) | (@as(u64, @intFromEnum(c)) << 32) | h;
    }
    inline fn g(grp: Group) u64 {
        return grp.base();
    }
    const nonIndexSymbol = 0xffffffff800000ff;
    pub inline fn indexSymbol0(uniqueNumber: u16) object.Object {
        return oImm(.Symbol, 0xf000000 | @as(u32, uniqueNumber));
    }
    pub inline fn indexSymbol1(uniqueNumber: u16) object.Object {
        return oImm(.Symbol, 0xf800000 | @as(u32, uniqueNumber));
    }
    pub inline fn isIndexSymbol0(self: object.Object) bool {
        return (self.rawU() & nonIndexSymbol) == (comptime indexSymbol0(0).rawU() & nonIndexSymbol);
    }
    pub inline fn isIndexSymbol1(self: object.Object) bool {
        return (self.rawU() & nonIndexSymbol) == (comptime indexSymbol1(0).rawU() & nonIndexSymbol);
    }
    pub inline fn indexNumber(self: object.Object) u24 {
        return @truncate(self.rawU() & nonIndexSymbol >> 8);
    }
    pub const invalidHeapPointer = of(Start_of_Heap_Objects);
    pub const ZERO = of(0);
    pub const False = oImm(.False, 0);
    pub const True = oImm(.True, 0);
    pub const Nil = oImm(.UndefinedObject, 0);
    const u64_ZERO = g(.smallInt0);
    pub inline fn makeGroup(grp: Group, low48: u48) object.Object {
        return cast(grp.base() | low48);
    }
    pub inline fn low16(self: object.Object) u16 {
        return self.h0;
    }
    pub inline fn mid16(self: object.Object) u16 {
        return self.h1;
    }
    pub inline fn high16(self: object.Object) u16 {
        return @intFromEnum(self.classIndex);
    }
    pub inline fn makeImmediate(cls: ClassIndex, low32: u32) object.Object {
        return @bitCast(Group.immediates.base() | (@as(u64, @intFromEnum(cls)) << 32) | low32);
    }
    pub inline fn hash24(self: object.Object) u24 {
        return @truncate(self.rawU() >> 8);
    }
    pub inline fn hash32(self: object.Object) u32 {
        return @truncate(self.rawU());
    }
    pub inline fn numArgs(self: object.Object) u8 {
        return @truncate(self.rawU());
    }
    pub inline fn withOffsetx(self: object.Object, offset: u32) object.Object {
        return cast(@as(u64, offset) << 32 | self.hash32());
    }
    pub inline fn cast(v: anytype) object.Object {
        // stored using little-endian order
        return @bitCast(v);
    }
    pub inline fn untaggedInt(self: object.Object) u64 {
        return self.toNatNoCheck();
    }
    pub inline fn tagged(tag: Group, low: u3, addr: u64) object.Object {
        return cast((object.Object{ .tag = tag, .classIndex = .none, .h1 = 0, .h0 = low }).rawU() + addr);
    }
    pub inline fn tagbits(self: object.Object) u32 {
        return @truncate(self.rawU() >> 32);
    }
    pub inline fn hashEquals(self: object.Object, other: object.Object) bool {
        //@truncate(u24,self.rawU()^other.rawU())==0;
        return self.hash32() == other.hash32();
    }
    pub inline fn selectorEquals(self: object.Object, other: object.Object) bool {
        //        return (self.rawU()^other.rawU())&0xffffffffffff == 0; // may be false positive
        return self.rawU() == other.rawU();
    }
    pub inline fn isInt(self: object.Object) bool {
        return switch (self.tag) {
            .smallIntMin, .smallIntNeg_2, .smallIntNeg_3, .smallIntNeg_4, .smallInt0, .smallIntPos_6, .smallIntPos_7, .smallIntMax => true,
            else => false,
        };
    }
    pub inline fn atLeastInt(self: object.Object) bool { // only useful if you have a Smallinteger-u51
        return @intFromEnum(self.tag) >= @intFromEnum(Group.smallIntMin);
    }
    pub inline fn atMostInt(self: object.Object) bool { // only useful if you have a Smallinteger+u51
        return if (@intFromEnum(Group.smallIntMax) == 0xffff)
            self.atLeastInt()
        else
            @intFromEnum(self.tag) <= @intFromEnum(Group.smallIntMax);
    }
    pub inline fn isNat(self: object.Object) bool {
        return switch (self.tag) {
            .smallInt0, .smallIntPos_6, .smallIntPos_7, .smallIntMax => true,
            else => false,
        };
    }
    pub inline fn isDouble(self: object.Object) bool {
        return self.rawU() <= Negative_Infinity;
    }
    pub inline fn isNonLocalThunk(self: object.Object) bool {
        return switch (self.tag) {
            .nonLocalThunk => true,
            else => false,
        };
    }
    pub inline fn isMemoryAllocated(self: object.Object) bool {
        return switch (self.tag) {
            .heapThunk, .nonLocalThunk, .heap => true,
            else => false,
        };
    }
    pub inline fn isBlock(self: object.Object) bool {
        return switch (self.tag) {
            .heapThunk, .nonLocalThunk => true,
            .immediates => switch (self.classIndex) {
                .ThunkSmallInteger, .ThunkFloat, .ThunkImmediate, .ThunkHeap, .ThunkReturnSelf, .ThunkReturnTrue, .ThunkReturnFalse, .ThunkReturnNil, .ThunkReturn_1, .ThunkReturn0, .ThunkReturn1, .ThunkReturn2 => true,
                else => false,
            },
            else => false,
        };
    }
    pub inline fn toBoolNoCheck(self: object.Object) bool {
        return @as(u1, @truncate(self.rawU())) == 1;
    }
    pub inline fn toIntNoCheck(self: object.Object) i64 {
        return @as(i64, @bitCast(self.rawU() -% u64_ZERO));
    }
    pub inline fn toNatNoCheck(self: object.Object) u64 {
        return self.rawU() -% u64_ZERO;
    }
    pub inline fn rawWordAddress(self: object.Object) u64 {
        return self.rawU() & 0xffff_ffff_fff8;
    }
    pub inline fn toDoubleNoCheck(self: object.Object) f64 {
        return @bitCast(self);
    }
    pub inline fn from(value: anytype) object.Object {
        return fromWithError(value) catch unreachable;
    }
    pub inline fn fromWithError(value: anytype) !object.Object {
        const T = @TypeOf(value);
        if (T == object.Object) return value;
        switch (@typeInfo(T)) {
            .int, .comptime_int => return cast(@as(u64, @bitCast(@as(i64, value))) +% u64_ZERO),
            .float => return cast(value),
            .comptime_float => return cast(@as(f64, value)),
            .bool => return if (value) object.Object.True else object.Object.False,
            .null => return object.Object.Nil,
            .pointer => |ptr_info| {
                switch (ptr_info.size) {
                    .One => {
                        return cast(@as(u48, @truncate(@intFromPtr(value))) + Start_of_Heap_Objects);
                    },
                    else => {},
                }
            },
            else => {},
        }
        @compileError("Can't convert \"" ++ @typeName(T) ++ "\"");
    }
    inline fn which_class(self: object.Object, comptime full: bool) ClassIndex {
        return switch (self.tag) {
            .heapThunk => .BlockClosure,
            .nonLocalThunk => @panic("nonLocalThunk"),
            .immediates => self.classIndex,
            .heap => if (full) self.to(HeapObjectPtr).*.getClass() else .Object,
            .smallIntMin, .smallIntNeg_2, .smallIntNeg_3, .smallIntNeg_4, .smallInt0, .smallIntPos_6, .smallIntPos_7, .smallIntMax => .SmallInteger,
            else => .Float,
        };
    }
    pub usingnamespace object.ObjectFunctions;
};
test "all generated NaNs are positive" {
    // test that all things that generate NaN generate positive ones
    // otherwise we'd need to check in any primitive that could create a NaN
    // because a negative one could look like one of our tags (in particular a large positive SmallInteger)
    const inf = @as(f64, 1.0) / 0.0;
    const zero = @as(f64, 0);
    const one = @as(f64, 1);
    const fns = struct {
        const nanU: u64 = @bitCast(math.nan(f64));
        fn d(x: anytype) !void {
            try std.testing.expect(object.Object.from(x).isDouble());
        }
        fn v(x: anytype) !void {
            try d(x);
            try std.testing.expectEqual(nanU, @as(u64, @bitCast(x)));
        }
    };
    const d = fns.d;
    const v = fns.v;
    try v(@sqrt(-one));
    try v(@log(-one));
    try v(zero / zero);
    try v((-inf) * 0.0);
    try d((-inf) * inf);
    try v((-inf) + inf);
    try v(inf - inf);
    try v(inf * 0.0);
    try v(std.math.nan(f64));
}
