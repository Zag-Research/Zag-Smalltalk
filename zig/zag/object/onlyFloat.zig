const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const math = std.math;
const zag = @import("../zag.zig");
const config = zag.config;
const assert = std.debug.assert;
const object = zag.object;
const ClassIndex = object.ClassIndex;

pub const Object = packed struct(u64) {
    float: u64,

    const Self = @This();
    pub const ZERO = of(0.0);

    pub fn False() Object {
        return @bitCast(@as(u64, 0));
    }

    pub fn True() Object {
        return @bitCast(@as(u64, 1));
    }

    pub fn Nil() Object {
        return @bitCast(@as(u64, 2));
    }

    pub const maxInt = 0x7fff_ffff_ffff_ffff;
    pub const tagged0: i64 = 0;
    pub const LowTagType = void;
    pub const lowTagSmallInteger = {};
    pub const HighTagType = void;
    pub const highTagSmallInteger = {};
    pub const PackedTagType = u3;
    pub const packedTagSmallInteger = 0;
    pub const intTag = @import("zag.zig").Object.intTag;
    pub const immediatesTag = 0;

    pub inline fn untaggedI(_: object.Object) ?i64 {
        @panic("not implemented");
    }

    pub inline fn untaggedI_noCheck(_: object.Object) i64 {
        @panic("not implemented");
    }

    pub const taggedI = untaggedI;
    pub const taggedI_noCheck = untaggedI_noCheck;

    pub inline fn fromTaggedI(_: i64, _: anytype, _: anytype) object.Object {
        @panic("not implemented");
    }

    pub const fromUntaggedI = fromTaggedI;

    pub inline fn symbol40(self: object.Object) u40 {
        return @truncate(self.rawU());
    }

    pub inline fn nativeI(_: object.Object) ?i64 {
        @panic("not implemented");
    }

    pub inline fn nativeU(_: object.Object) ?u64 {
        @panic("not implemented");
    }

    pub inline fn nativeF(self: object.Object) ?f64 {
        return @bitCast(self);
    }

    pub inline fn isFloat(_: object.Object) bool {
        return true;
    }

    pub inline fn nativeF_noCheck(self: object.Object) f64 {
        return @bitCast(self);
    }

    pub inline fn fromNativeF(f: f64, _: anytype, _: anytype) object.Object {
        return @bitCast(f);
    }

    pub inline fn symbolHash(self: object.Object) ?u24 {
        return @truncate(self.hash32());
    }

    pub inline fn extraValue(self: object.Object) object.Object {
        return @bitCast(self.rawU() >> 8);
    }

    pub inline fn extraI(self: object.Object) i8 {
        _ = .{ self, unreachable };
    }

    pub const testU = rawU;
    pub const testI = rawI;

    pub inline fn rawU(self: object.Object) u64 {
        return @bitCast(self);
    }

    inline fn rawI(self: object.Object) i64 {
        return @bitCast(self);
    }

    pub inline fn invalidObject(_: object.Object) ?u64 {
        // there are no invalid objects in this encoding
        return null;
    }
    inline fn of(comptime v: f64) object.Object {
        return @bitCast(v);
    }

    pub inline fn thunkImmediate(o: Object) ?Object {
        _ = .{ o, unreachable };
    }

    pub inline fn thunkImmediateValue(self: Self) Object {
        _ = .{ self, unreachable };
    }

    pub inline fn isImmediateClass(_: Object, comptime _: ClassIndex) bool {
        return false;
    }

    pub inline fn isHeap(_: Object) bool {
        return false;
    }

    pub inline fn isMemoryDouble(_: object.Object) bool {
        return false;
    }

    pub inline fn isInt(_: Object) bool {
        return false;
    }

    pub inline fn isNat(_: Object) bool {
        return false;
    }

    pub inline fn isDouble(_: Object) bool {
        return true;
    }

    pub inline fn hasPointer(_: Object) bool {
        return false;
    }

    pub inline fn highPointer(_: Object, T: type) ?T {
        @panic("Not implemented");
    }

    pub inline fn pointer(_: Object, T: type) ?T {
        @panic("Not implemented");
    }

    pub inline fn toBoolNoCheck(self: Object) bool {
        return self == Object.True();
    }

    pub inline fn toIntNoCheck(_: Object) i64 {
        @panic("not implemented");
    }

    pub inline fn toNatNoCheck(_: Object) u64 {
        @panic("not implemented");
    }

    pub inline fn withPrimitive(self: object.Object, prim: u64) object.Object {
        return @bitCast(self.rawU() | prim << 40);
    }

    pub inline fn withClass(self: Object, class: ClassIndex) Object {
        return @bitCast((self.rawU() & 0xffffffffff) | (@as(u64, @intFromEnum(class)) << 40));
    }

    pub inline fn rawWordAddress(self: Object) u64 {
        return self.rawU() & 0xffff_ffff_fff8;
    }

    inline fn toDoubleFromMemory(_: object.Object) f64 {
        @panic("Not implemented");
    }

    pub inline fn toDoubleNoCheck(self: Object) f64 {
        return @bitCast(self);
    }

    pub inline fn makeImmediate(_: ClassIndex.Compact, hash: u64) Object {
        return @bitCast(hash);
    }

    pub inline fn makeThunk(cls: ClassIndex.Compact, ptr: anytype, extra: u8) Object {
        _ = .{ cls, ptr, extra, unreachable };
    }

    pub inline fn hash24(self: Object) u24 {
        return @truncate(self.rawU());
    }

    pub inline fn hash32(self: Object) u32 {
        return @truncate(self.rawU());
    }

    pub inline fn isSymbol(_: object.Object) bool {
        return true;
    }

    pub inline fn fromAddress(value: anytype) Object {
        return @bitCast(@intFromPtr(value));
    }
    pub const StaticObject = struct {
        pub fn init(_: *StaticObject, comptime value: anytype) object.Object {
            switch (@typeInfo(@TypeOf(value))) {
                .float, .comptime_float => return @bitCast(@as(f64, value)),
                .bool => return if (value) object.Object.True() else object.Object.False(),
                else => @panic("Unsupported type for compile-time object creation"),
            }
        }
    };
    pub inline fn from(value: anytype, _: anytype, _: anytype) Object {
        const T = @TypeOf(value);
        if (T == Object) return value;
        switch (@typeInfo(T)) {
            .float, .comptime_float => return @bitCast(@as(f64, value)),
            .bool => return if (value) Object.True() else Object.False(),
            .null => return Object.Nil(),
            else => return undefined,
        }
    }

    pub fn toWithCheck(self: Object, comptime T: type, comptime _: bool) T {
        switch (T) {
            f64 => {
                return self.toDoubleNoCheck();
            },
            bool => {
                return self.toBoolNoCheck();
            },
            else => {},
        }
        @panic("Trying to convert Object to " ++ @typeName(T));
    }

    pub inline fn which_class(_: Object) ClassIndex {
        return .Float;
    }

    pub inline fn isMemoryAllocated(_: Object) bool {
        return false;
    }

    pub inline fn isHeapObject(_: Object) bool {
        return false;
    }

    const OF = object.ObjectFunctions;
    pub const arrayAsSlice = OF.arrayAsSlice;
    pub const asMemoryObject = OF.asMemoryObject;
    pub const asObjectArray = OF.asObjectArray;
    pub const asZeroTerminatedString = OF.asZeroTerminatedString;
    pub const compare = OF.compare;
    pub const empty = OF.empty;
    pub const equals = OF.equals;
    pub const format = OF.format;
    pub const getField = OF.getField;
    pub const get_class = OF.get_class;
    pub const isBool = OF.isBool;
    pub const isIndexable = OF.isIndexable;
    pub const isNil = OF.isNil;
    pub const isUnmoving = OF.isUnmoving;
    pub const numArgs = OF.numArgs;
    pub const promoteToUnmovable = OF.promoteToUnmovable;
    pub const rawFromU = OF.rawFromU;
    pub const setField = OF.setField;
    pub const to = OF.to;
    pub const toUnchecked = OF.toUnchecked;
    pub const asVariable = zag.Context.asVariable;
    pub const PackedObject = object.PackedObject;
    pub const signature = zag.execute.Signature.signature;
    pub const tests = OF.tests;
};
