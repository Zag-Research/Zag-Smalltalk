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

const Tag = enum(Object.LowTagType) {
    pointer = 0,
    smallInteger = 1,
    inline fn u(cg: Tag) u1 {
        return @intFromEnum(cg);
    }
};
pub const Object = packed union {
    ref: *InMemory.PointedObject,
    immediate: packed struct(u64) {
        tag: Tag,
        hash: u63,
    },

    const PointerTag = Tag.u(.pointer);
    const SmallIntegerTag = Tag.u(.smallInteger);
    const TagMask = SmallIntegerTag;

    const Self = @This();
    pub const maxInt = 0x3fff_ffff_ffff_ffff;
    pub const ZERO: Object = @bitCast(@as(u64, 0));
    pub inline fn False() Object {
        return Object.fromAddress(&InMemory.False);
    }

    pub inline fn True() Object {
        return Object.fromAddress(&InMemory.True);
    }

    pub inline fn Nil() Object {
        return Object.fromAddress(&InMemory.Nil);
    }

    pub const LowTagType = TagAndClassType;
    pub const lowTagSmallInteger = makeImmediate(.SmallInteger, 0).tagbits();
    pub const HighTagType = void;
    pub const highTagSmallInteger = {};
    pub const PackedTagType = u3;
    pub const packedTagSmallInteger = intTag;
    pub const intTag = Tag.u(.smallInteger);
    pub const immediatesTag = Tag.u(.smallInteger);
    pub const tagged0: i64 = Tag.u(.smallInteger); // SmallInteger 0 in spur encoding
    const TagAndClassType = u1;
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

    pub inline fn fromTaggedI(i: i64, _:anytype, _: anytype) Object {
        return @bitCast(i);
    }

    pub inline fn fromUntaggedI(i: i64, _: anytype, _: anytype) Object {
        return @bitCast(@as(u64, @bitCast(i)) + SmallIntegerTag);
    }

    // Spur SmallInteger
    pub inline fn isInt(self: Object) bool {
        return (self.rawU() & SmallIntegerTag) != 0;
    }
    pub inline fn isNat(self: Object) bool {
        return self.isInt() and self.rawI() >= 0;
    }
    pub inline fn symbol40(self: object.Object) u40 {
        return @truncate(self.ref.data.unsigned);
    }
    pub inline fn nativeI(self: Object) ?i64 {
        if (self.isInt()) return self.nativeI_noCheck();
        return null;
    }
    pub inline fn nativeI_noCheck(self: Object) i64 {
        return @as(i64, self.rawI()) >> 1;
    }
    pub inline fn fromNativeI(i: i63, _: anytype, _: anytype) Object {
        return @bitCast((@as(u64, @bitCast(@as(i64, i))) << 1) + SmallIntegerTag);
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
    pub inline fn isImmediate(self: Object) bool {
        return !self.isHeap();
    }

    pub inline fn isImmediateClass(self: object.Object, comptime class: ClassIndex) bool {
        if (self.isInt()) return class == .SmallInteger;
        return self.ref.header.classIndex == class;
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
        return self.isImmediateClass(.Symbol);
    }
    pub inline fn isNil(self: Object) bool {
        return self.rawU() == Object.Nil().rawU();
    }

    inline fn oImm(c: Tag, h: u61) Self {
        return Self{ .immediate = .{ .tag = c, .hash = h } };
    }
    pub inline fn makeImmediate(cls: ClassIndex, hash: u61) object.Object {
        // Map ClassIndex to appropriate Tag
        const group = switch (cls) {
            .SmallInteger => Tag.smallInteger,
            .Character => Tag.character,
            .Float => Tag.float,
            else => Tag.pointer, // heap objects
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
        return memoryFloat(t, sp, context);
    }

    // Hash helpers
    pub inline fn hash24(self: Object) u24 {
        return self.ref.header.hash;
    }
    pub inline fn hash32(self: Object) u32 {
        return @truncate(self.ref.data.unsigned);
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
    pub const StaticObject = struct {
        obj: InMemory.PointedObject,
        pub fn init(self: *StaticObject, comptime value: anytype) object.Object {
            const ptr: *InMemory.PointedObject = @ptrCast(self);
            switch (@typeInfo(@TypeOf(value))) {
                .int, .comptime_int => return fromNativeI(value, null, null),
                .comptime_float => return fromAddress(ptr.set(.Float, value)),
                .bool => return if (value) object.Object.True() else object.Object.False(),
                else => @panic("Unsupported type for compile-time object creation"),
            }
        return fromAddress(ptr);
    }
    };
    // Conversion from Zig types
    pub inline fn from(value: anytype, sp: SP, context: *Context) Object {
        const T = @TypeOf(value);
        if (T == Object) return value;
        switch (@typeInfo(T)) {
            .int, .comptime_int => return fromNativeI(value, sp, context),
            .float, .comptime_float => return fromNativeF(value, sp, context),
            .bool => return if (value) Object.True() else Object.False(),
            .null => return Object.Nil(),
            .pointer => |ptr_info| {
                switch (ptr_info.size) {
                    .one, .many => return fromAddress(value),
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
            bool => {
                if (!check or self.isBool()) return self.toBoolNoCheck();
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

    // Class detection (stub)
    pub inline fn which_class(self: Object) ClassIndex {
        if (self.isInt()) {@branchHint(.likely);
            return .SmallInteger;
        }
        return self.ref.header.classIndex;
    }
    pub inline fn isMemoryAllocated(self: object.Object) bool {
        return self.isHeap();
    }

    // Add symbolHash method
    pub inline fn symbolHash(self: Object) ?u24 {
        if (self.isImmediateClass(.Symbol)) return self.ref.header.hash;
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
    pub inline fn heapObject(self: object.Object) ?*InMemory.PointedObject {
        if (self.isHeap()) return self.ref;
        return null;
    }

    pub inline fn extraValue(self: Object) Object {
        // For spur encoding, extract value from immediate objects
        // if (self.isImmediate()) {
        //     return fromSmallInteger(self.immediate.hash);
        // }
        // return self;
        _ = .{ self, @panic("Not implemented") };
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
