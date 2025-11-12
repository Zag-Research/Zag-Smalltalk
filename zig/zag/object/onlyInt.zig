const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const math = std.math;
const zag = @import("../zag.zig");
const config = zag.config;
const assert = std.debug.assert;
const debugError = false;
const InMemory = zag.InMemory;
const object = zag.object;
const ClassIndex = object.ClassIndex;
const heap = zag.heap;
const HeapHeader = heap.HeapHeader;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
const Process = zag.Process;
pub const Object = packed struct(u64) {
    int: u64,
    const Self = @This();
    pub const ZERO = of(0);
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
    pub const packedTagSmallInteger = 1;
    pub const intTag = @import("zag.zig").Object.intTag;
    pub const immediatesTag = 1;
    pub inline fn untaggedI(self: object.Object) ?i64 {
        return @bitCast(self);
    }
    pub inline fn untaggedI_noCheck(self: object.Object) i64 {
        return @bitCast(self);
    }
    pub const taggedI = untaggedI;
    pub const taggedI_noCheck = untaggedI_noCheck;
    pub inline fn fromTaggedI(i: i64, _: *Process) object.Object {
        return @bitCast(i);
    }
    pub const fromUntaggedI = fromTaggedI;
    pub inline fn symbol40(_: object.Object) u40 {
        return 0;
    }
    pub inline //
    fn nativeI(self: object.Object) ?i64 {
        return @bitCast(self);
    }
    pub inline fn nativeU(self: object.Object) ?u64 {
        return @bitCast(self);
    }
    pub inline fn nativeF(_: object.Object) ?f64 {
        @panic("not implemented");
    }
    pub inline fn isFloat(_: object.Object) bool {
        return false;
    }
    pub inline fn nativeF_noCheck(_: object.Object) f64 {
        @panic("not implemented");
    }
    pub inline fn fromNativeF(_: f64, _: *Process) object.Object {
        @panic("not implemented");
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
    pub inline //
    fn rawU(self: object.Object) u64 {
        return @bitCast(self);
    }
    inline fn rawI(self: object.Object) i64 {
        return @bitCast(self);
    }
    inline fn of(comptime v: u64) object.Object {
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
    pub inline //
    fn isInt(_: Object) bool {
        return true;
    }
    pub inline fn isNat(self: Object) bool {
        return self.isInt() and self.rawI() >= 0;
    }
    pub inline fn isDouble(_: Object) bool {
        return false;
    }
    // pub inline fn oImm(c: ClassIndex.Compact, h: u56) Self {
    //     return Self{ .tag = .immediates, .class = c, .hash = h };
    // }
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
    pub inline fn toIntNoCheck(self: Object) i64 {
        return @bitCast(self);
    }
    pub inline fn toNatNoCheck(self: Object) u64 {
        return @bitCast(self);
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
    pub inline fn toDoubleNoCheck(_: Object) f64 {
        @panic("Not implemented");
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
    pub inline //
    fn fromAddress(value: anytype) Object {
        return @bitCast(@intFromPtr(value));
    }
    pub inline fn from(value: anytype, _: *Process) Object {
        const T = @TypeOf(value);
        if (T == Object) return value;
        switch (@typeInfo(T)) {
            .int, .comptime_int => return @bitCast(@as(i64,value)),
            .bool => return if (value) Object.True() else Object.False(),
            .null => return Object.Nil(),
            else => return undefined,
        }
    }
    pub fn toWithCheck(self: Object, comptime T: type, comptime check: bool) T {
        switch (T) {
            i64 => {
                return self.toIntNoCheck();
            },
            u64 => {
                return self.toNatNoCheck();
            },
            bool => {
                return self.toBoolNoCheck();
            },
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
    pub inline //
    fn which_class(_: Object) ClassIndex {
        return .SmallInteger;
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
