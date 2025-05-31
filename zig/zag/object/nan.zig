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
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
const HeapHeader = heap.HeapHeader;
pub const Object = packed struct(u64) {
    h0: u16, // align(8),
    h1: u16,
    classIndex: ClassIndex,
    tag: Group,
    pub const Group = enum(u16) {
        heap= 0xfff0,
        thunkReturnLocal,
        thunkReturnInstance,
        thunkReturnSmallInteger,
        thunkReturnImmediate,
        thunkLocal,
        thunkInstance,
        picPointer,
        thunkHeap,
        thunkImmediate,
        _reserved,
        immediates,
        smallInteger,
        _,
        const Self = @This();
        inline fn base(cg: Self) u64 {
            return @as(u64, @intFromEnum(cg)) << 48;
        }
        inline fn tag(cg: Self, low: u48) u64 {
            return cg.base() | low;
        }
        inline fn u(cg: Self) u16 {
            return @intFromEnum(cg);
        }
    };
    pub const ZERO = of(0);
    pub const False = oImm(.False, 0);
    pub const True = oImm(.True, 0);
    pub const Nil = oImm(.UndefinedObject, 0);
    pub const LowTagType = void;
    pub const LowTagSmallInteger = {};
    pub const HighTagType = u14;
    pub const HighTagSmallInteger: HighTagType = Group.u(.smallInteger) >> 2;
    const TagAndClassType = u32;
    const tagAndClassBits = enumBits(Group) + enumBits(ClassIndex);
    comptime {
        assert(tagAndClassBits == @bitSizeOf(TagAndClassType));
    }
    pub const testU = rawU;
    pub const testI = rawI;
    fn rawU(self: Object) u64 {
        return @bitCast(self);
    }
    fn rawI(self: Object) i64 {
        return @bitCast(self);
    }
    pub inline fn tagbits(self: object.Object) TagAndClassType {
        return @truncate(self.rawU() >> (64 - tagAndClassBits));
    }
    fn enumBits(T: type) usize {
        return @typeInfo(@typeInfo(T).@"enum".tag_type).int.bits;
    }
    pub inline fn tagMethod(o: object.Object) ?object.Object {
        return @bitCast(@as(u64,@bitCast(o)) | 1);
    }
    pub inline fn tagMethodValue(self: object.Object) object.Object {
        return @bitCast(@as(u64,@bitCast(self)) >> 1 << 1);
    }
    pub inline fn isTaggedMethod(self: object.Object) bool {
        return (@as(u64,@bitCast(self)) & 1) != 0;
    }
    const Negative_Infinity: u64 = g(.immediates); //0xfff0000000000000;
    const Start_of_Heap_Objects: u64 = g(.heap);
    inline fn of(comptime v: u64) object.Object {
        return @bitCast(v);
    }
    inline fn oPtr(grp: Group, p: u48) object.Object {
        return @bitCast(grp.tag(p));
    }
    pub inline fn isImmediateClass(self: object.Object, class: ClassIndex) bool {
        return self.tagbits() == oImm(class, 0).tagbits();
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
    const u64_ZERO = g(.smallInteger);
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
        return @truncate(self.rawU());
    }
    pub inline fn hash32(self: object.Object) u32 {
        return @truncate(self.rawU());
    }
    pub inline fn withOffsetx(self: object.Object, offset: u32) object.Object {
        return cast(@as(u64, offset) << 32 | self.hash32());
    }
    pub inline fn cast(v: anytype) object.Object {
        // stored using little-endian order
        return @bitCast(v);
    }
    pub inline fn untaggedI(self: object.Object) ?i64 {
        if (self.isInt()) return self.untaggedI_noCheck();
        return null;
    }
    pub inline fn untaggedI_noCheck(self: object.Object) i64 {
        return @bitCast(self.rawU() << 14);
    }
    pub inline fn taggedI(self: object.Object) ?i64 {
        if (self.isInt()) return taggedI_noCheck(self);
        return null;
    }
    pub const taggedI_noCheck = untaggedI_noCheck;
    pub const fromTaggedI = fromUntaggedI;
    pub inline fn fromUntaggedI(i: i64) object.Object {
        var o = cast(@as(u64, @bitCast(i)) >> 14);
        o.tag = .smallInteger;
        return o;
    }
    // pub inline fn cast(v: anytype) object.Object {
    //     // stored using little-endian order
    //     return @bitCast(v);
    // }
    pub inline fn symbol40(self: object.Object) u40 {
        return @truncate(self.rawU());
    }
    pub inline fn untaggedInt(self: object.Object) u64 {
        return self.toNatNoCheck();
    }
    pub inline fn tagged(tag: Group, low: u3, addr: u64) object.Object {
        return cast((object.Object{ .tag = tag, .classIndex = .none, .h1 = 0, .h0 = low }).rawU() + addr);
    }
    pub inline fn hashEquals(self: object.Object, other: object.Object) bool {
        //@truncate(u24,self.rawU()^other.rawU())==0;
        return self.hash32() == other.hash32();
    }
    pub inline fn nativeI(self: object.Object) ?i64 {
        if (self.isInt()) return self.nativeI_noCheck();
        return null;
    }
    inline fn nativeI_noCheck(self: object.Object) i64 {
        return @as(i64, @bitCast(self.rawU() << 14)) >> 14;
    }
    pub inline fn nativeU(self: object.Object) ?u64 {
        if (self.isInt()) return self.nativeU_noCheck();
        return null;
    }
    inline fn nativeU_noCheck(self: object.Object) u64 {
        return self.rawU() << 14 >> 14;
    }
    pub inline fn selectorEquals(self: object.Object, other: object.Object) bool {
        //        return (self.rawU()^other.rawU())&0xffffffffffff == 0; // may be false positive
        return self.rawU() == other.rawU();
    }
    pub inline fn isInt(self: object.Object) bool {
        return @intFromEnum(self.tag) >= @intFromEnum(Group.smallInteger);
    }
    pub inline fn isNat(self: object.Object) bool {
        if (self.untaggedI()) |value|return value >= 0;
        return false;
    }
    pub inline fn pointer(self: object.Object, T: type) ?T {
        if (true) unreachable;
        switch (self.tag) {
            .heap => return @ptrFromInt(self.rawU()),
            .immediates => switch (self.class) {
                .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnSmallInteger, .ThunkReturnImmediate, .ThunkReturnCharacter, .ThunkReturnFloat, .ThunkHeap, .ThunkLocal, .ThunkInstance, .BlockAssignLocal, .BlockAssignInstance, .PICPointer => return self.highPointer(T),
                else => {},
            },
            else => {},
        }
        return null;
    }

    pub inline fn isDouble(self: object.Object) bool {
        return self.rawU() <= Negative_Infinity;
    }
    pub inline fn isNonLocalThunkX(self: object.Object) bool {
        return switch (self.tag) {
            .nonLocalThunk => true,
            else => false,
        };
    }
    pub inline fn isMemoryAllocated(self: object.Object) bool {
        return switch (self.tag) {
            .heap,
            .thunkReturnLocal,
            .thunkReturnInstance,
            .thunkReturnSmallInteger,
            .thunkReturnImmediate,
            .thunkLocal,
            .thunkInstance,
            .picPointer,
            .thunkHeap => true,
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
    pub inline fn rawWordAddress(self: object.Object) u64 {
        return self.rawU() & 0xffff_ffff_fff8;
    }
    pub inline fn toDoubleNoCheck(self: object.Object) f64 {
        return @bitCast(self);
    }
    pub inline fn from(value: anytype, _: anytype) object.Object {
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
                    .one => {
                        return cast(@as(u48, @truncate(@intFromPtr(value))) + Start_of_Heap_Objects);
                    },
                    else => {},
                }
            },
            else => {},
        }
        @compileError("Can't convert \"" ++ @typeName(T) ++ "\"");
    }
    pub fn toWithCheck(self: object.Object, comptime T: type, comptime check: bool) T {
        switch (T) {
            f64 => {
                if (!check or self.isImmediateDouble()) return self.toDoubleNoCheck();
                if (!check or self.isMemoryDouble()) return self.toDoubleFromMemory();
            },
            i64 => {
                if (!check or self.isInt()) return self.nativeI_noCheck();
            },
            u64 => {
                if (!check or self.isNat()) return self.nativeU_noCheck();
            },
            bool => {
                if (!check or self.isBool()) return self.toBoolNoCheck();
            },
            object.PackedObject => {
                if (!check or self.isInt()) return @as(T, @bitCast(self));
            },

            //u8  => {return @intCast(u8, self.hash & 0xff);},
            else => {
                switch (@typeInfo(T)) {
                    .pointer => |ptrInfo| {
                        switch (@typeInfo(ptrInfo.child)) {
                            .@"fn" => {},
                            .@"struct" => {
                                if (!check or (self.isMemoryAllocated() and (!@hasDecl(ptrInfo.child, "ClassIndex") or self.to(HeapObjectConstPtr).classIndex == ptrInfo.child.ClassIndex))) {
                                    if (@hasField(ptrInfo.child, "header") or (@hasDecl(ptrInfo.child, "includesHeader") and ptrInfo.child.includesHeader)) {
                                        return @as(T, @ptrFromInt(@as(usize, @bitCast(self))));
                                    } else {
                                        return @as(T, @ptrFromInt(@sizeOf(HeapHeader) + (@as(usize, @bitCast(self)))));
                                    }
                                }
                            },
                            else => {},
                        }
                    },
                    else => {},
                }
            },
        }
        @panic("Trying to convert Object to " ++ @typeName(T));
    }
    pub inline fn which_class(self: object.Object, comptime full: bool) ClassIndex {
        return switch (self.tag) {
            .heapThunk => .BlockClosure,
            .nonLocalThunk => @panic("nonLocalThunk"),
            .immediates => self.classIndex,
            .heap => if (full) self.to(HeapObjectPtr).*.getClass() else .Object,
            .smallInteger => .SmallInteger,
            else => .Float,
        };
    }
    pub inline fn isHeapObject(self: object.Object) bool {
        return self.tag == .heap;
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
