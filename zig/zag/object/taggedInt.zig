//! This module implements Object encoding for integers flagged
const std = @import("std");
const math = std.math;
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;
const zag = @import("../zag.zig");
const trace = zag.config.trace;
const object = zag.object;
const Process = zag.Process;
const SP = Process.SP;
const Context = zag.Context;
const testing = std.testing;
const ClassIndex = object.ClassIndex;
const heap = zag.heap;
const HeapHeader = heap.HeapHeader;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
const InMemory = zag.InMemory;

pub const Object = packed union {
    ref: *InMemory.PointedObject,
    immediate: packed struct(u64) {
        tag: Group,
        hash: u63,
    },

    const Group = enum(LowTagType) {
        pointer = 0,
        smallInteger = 1,
        inline fn u(cg: Group) u1 {
            return @intFromEnum(cg);
        }
    };

    const PointerTag = Group.u(.pointer);
    const SmallIntegerTag = Group.u(.smallInteger);
    const TagMask = SmallIntegerTag;

    const Self = @This();
    pub const ZERO: Object = @bitCast(@as(u64, 0));
    pub const LowTagType = TagAndClassType;
    pub const lowTagSmallInteger = makeImmediate(.SmallInteger, 0).tagbits();
    pub const HighTagType = void;
    pub const highTagSmallInteger = {};
    pub const PackedTagType = u3;
    pub const packedTagSmallInteger = intTag;
    pub const intTag = @intFromEnum(Group.smallInteger);
    pub const immediatesTag = 1;
    pub const maxInt = 0x3fffffffffffffff;
    pub const tagged0: i64 = 1; // SmallInteger 0 in spur encoding
    const TagAndClassType = u1;

    // Static constructor functions
    pub inline fn False() Object {
        return Object.fromAddress(&InMemory.False);
    }

    pub inline fn True() Object {
        return Object.fromAddress(&InMemory.True);
    }

    pub inline fn Nil() Object {
        return Object.fromAddress(&InMemory.Nil);
    }

    pub inline fn tagbits(self: Self) TagAndClassType {
        return @truncate(self.rawU());
    }

    pub inline fn untaggedI(self: object.Object) ?i64 {
        if (self.isInt()) return untaggedI_noCheck(self);
        return null;
    }

    pub inline fn untaggedI_noCheck(self: Object) i64 {
        return @bitCast(self.rawU() - SmallIntegerTag);
    }

    pub inline fn taggedI(self: Object) ?i64 {
        if (self.isInt()) return taggedI_noCheck(self);
        return null;
    }

    pub inline fn taggedI_noCheck(self: Object) i64 {
        return @bitCast(self);
    }

    pub inline fn fromTaggedI(i: i64, _: anytype) Object {
        return @bitCast(i);
    }

    pub inline fn fromUntaggedI(i: i64, _: anytype) Object {
        return @bitCast(@as(u64, @bitCast(i)) | SmallIntegerTag);
    }

    // Spur SmallInteger
    pub inline fn isInt(self: Object) bool {
        return (self.rawU() & SmallIntegerTag) != 0;
    }
    pub inline fn isNat(self: Object) bool {
        return self.isInt() and self.nativeI_noCheck() >= 0;
    }
    pub inline fn symbol40(self: object.Object) u40 {
        return @truncate(self.ref.data.unsigned);
    }
    pub inline fn nativeI(self: Object) ?i64 {
        if (self.isInt()) return self.nativeI_noCheck();
        return null;
    }
    pub inline fn nativeI_noCheck(self: Object) i64 {
        return @as(i64, self.rawI()) >> 1 << 1;
    }
    pub inline fn nativeU(self: Object) ?u64 {
        if (self.isNat()) return self.nativeU_noCheck();
        return null;
    }
    pub inline fn nativeU_noCheck(self: Object) u64 {
        return @as(u64, self.rawU()) >> 1 << 1;
    }
    pub inline fn fromSmallInteger(i: i64) Object {
        return @bitCast((@as(u64, @bitCast(i)) << 1) | SmallIntegerTag);
    }

    pub inline fn isHeap(self: Object) bool {
        return (self.rawU() & TagMask) == PointerTag;
    }
    pub inline fn isHeapObject(self: Object) bool {
        return self.isHeap();
    }
    pub inline fn pointer(self: Object, T: type) ?T {
        if (self.isHeap()) return @ptrFromInt(self.rawU());
        return null;
    }
    pub inline fn fromPointer(ptr: anytype) Object {
        const Foo = packed struct { ref: *u64 };
        const foo = Foo{ .ref = @constCast(@ptrCast(ptr)) };
        return @bitCast(foo);
    }

    pub inline fn isImmediate(self: Object) bool {
        return !self.isHeap();
    }

    pub inline fn isImmediateClass(self: object.Object, comptime class: ClassIndex) bool {
        if (self.isInt()) return class == .SmallInteger;
        return false;
    }

    pub const MaxImmediateCharacter = 0x10FFFF;

    pub inline fn isCharacter(_: Object) bool {
        return false;
    }

    pub inline fn characterValue(self: Self) ?u32 {
        if (self.isCharacter())
            return @intCast(self.raw >> 3);
        return null;
    }

    pub inline fn isBool(self: Object) bool {
        return self.rawU() == Object.True().rawU() or self.rawU() == Object.False().rawU();
    }
    pub inline fn toBoolNoCheck(self: Object) bool {
        return self.rawU() == Object.True().rawU();
    }
    pub inline fn isSymbol(self: Object) bool {
        // symbols are heap objects
        return self.isHeap() and self.to(HeapObjectPtr).*.getClass() == .Symbol;
    }
    pub inline fn isNil(self: Object) bool {
        return self.rawU() == Object.Nil().rawU();
    }

    inline fn oImm(c: Group, h: u61) Self {
        return Self{ .immediate = .{ .tag = c, .hash = h } };
    }
    pub inline fn makeImmediate(cls: ClassIndex, hash: u61) object.Object {
        // Map ClassIndex to appropriate Group
        const group = switch (cls) {
            .SmallInteger => Group.smallInteger,
            .Character => Group.character,
            .Float => Group.float,
            else => Group.pointer, // heap objects
        };
        return oImm(group, hash);
    }

    pub inline fn nativeF(self: object.Object) ?f64 {
        if (self.isMemoryDouble()) return self.toDoubleFromMemory();
        return null;
    }
    pub inline fn isFloat(self: object.Object) bool {
        return self.isMemoryDouble();
    }
    pub inline fn nativeF_noCheck(self: object.Object) f64 {
        return self.toDoubleFromMemory();
    }
    pub inline fn fromNativeF(t: f64, sp: SP, context: *Context) object.Object {
        return from(t, sp, context);
    }

    // Hash helpers
    pub inline fn hash24(self: Object) u24 {
        return self.ref.header.hash;
    }
    pub inline fn hash32(self: Object) u32 {
        return @truncate(self.ref.data.unsigned >> 8);
    }
    pub inline fn hash48(self: Object) u48 {
        return @truncate(self.rawU());
    }
    pub inline fn hash56(self: Object) u56 {
        return @truncate(self.rawU());
    }

    // Raw access
    pub const testU = rawU;
    pub const testI = rawI;
    pub inline fn rawU(self: Object) u64 {
        return @bitCast(self);
    }

    inline fn rawI(self: Object) i64 {
        return @bitCast(self.rawU());
    }
    pub inline fn rawWordAddress(self: Object) u64 {
        return self.rawU() & ~TagMask;
    }
    pub inline fn invalidObject(self: object.Object) ?u64 {
        const value: u64 = @bitCast(self);
        if (value == 0) return value;
        if (value & 6 != 0) return value;
        return null;
    }

    fn memoryFloat(value: f64, sp: SP, context: *Context) object.Object {
        if (math.isNan(value)) return object.Object.fromAddress(&InMemory.nanMemObject);
        if (math.inf(f64) == value) return object.Object.fromAddress(&InMemory.pInfMemObject);
        if (math.inf(f64) == -value) return object.Object.fromAddress(&InMemory.nInfMemObject);
        return InMemory.float(value, sp, context);
    }

    pub fn fromAddress(value: anytype) Object {
        return @bitCast(@intFromPtr(value));
    }
    // Conversion from Zig types
    pub inline fn from(value: anytype, sp: SP, context: *Context) Object {
        const T = @TypeOf(value);
        if (T == Object) return value;
        switch (@typeInfo(T)) {
            .int, .comptime_int => return Self.fromSmallInteger(value),
            .float => return memoryFloat(value, sp, context),
            .comptime_float => return from(@as(f64, value), sp, context),
            .bool => return if (value) Object.True() else Object.False(),
            .null => return Object.Nil(),
            .pointer => |ptr_info| {
                switch (ptr_info.size) {
                    .one, .many => return Self.fromPointer(value),
                    else => {},
                }
            },
            else => {},
        }
        @compileError("Can't convert \"" ++ @typeName(T) ++ "\"");
    }

    pub inline fn isMemoryDouble(self: object.Object) bool {
        return self.isMemoryAllocated() and self.to(HeapObjectPtr).*.getClass() == .Float;
    }

    inline fn toDoubleFromMemory(self: object.Object) f64 {
        return self.to(*InMemory.MemoryFloat).*.value;
    }

    // Conversion to Zig types (partial)
    pub fn toWithCheck(self: Object, comptime T: type, comptime check: bool) T {
        switch (T) {
            f64 => {
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
            else => {},
        }
        @panic("Trying to convert Object to " ++ @typeName(T));
    }

    // Class detection (stub)
    pub inline fn which_class(self: Object) ClassIndex {
        if (self.isInt()) {@branchHint(.likely);
            return .SmallInteger;
        }
        return self.to(HeapObjectPtr).*.getClass();
    }
    pub inline fn isMemoryAllocated(self: object.Object) bool {
        return self.isHeap();
    }

    // Add symbolHash method
    pub inline fn symbolHash(self: Object) ?u24 {
        if (self.isSymbol()) {
            return @as(u24, self.to(HeapObjectPtr).*.header.hash);
        }
        return null;
    }

    // Add missing methods
    pub inline fn signature(_: Object) ?zag.execute.Signature {
        // Spur doesn't use immediate signatures like other encodings
        return null;
    }

    pub inline fn isDouble(self: Object) bool {
        return self.isMemoryDouble();
    }

    pub inline fn asObject(self: Object) Object {
        return self;
    }

    pub inline fn withPrimitive(self: Object, prim: u64) Object {
        // For spur encoding, we can't easily embed primitives in objects
        // However, this is only done for signature objects, which already aren't quite valid
        return @bitCast(self.rawU() | prim << 40);
    }

    pub inline fn extraValue(self: Object) Object {
        // For spur encoding, extract value from immediate objects
        if (self.isImmediate()) {
            return Object.from(@as(i64, @intCast(self.immediate.hash)), null);
        }
        return self;
    }

    const OF = object.ObjectFunctions;
    pub const PackedObject = object.PackedObject;
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
    pub const isIndexable = OF.isIndexable;
    pub const isUnmoving = OF.isUnmoving;
    pub const numArgs = OF.numArgs;
    pub const promoteToUnmovable = OF.promoteToUnmovable;
    pub const rawFromU = OF.rawFromU;
    pub const to = OF.to;
    pub const toUnchecked = OF.toUnchecked;
    pub const asVariable = zag.Context.asVariable;
    pub const setField = OF.setField;
    pub const tests = OF.tests;
};
