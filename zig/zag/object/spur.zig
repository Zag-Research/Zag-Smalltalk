const std = @import("std");
const zag = @import("../zag.zig");
const object = zag.object;
const testing = std.testing;
const ClassIndex = object.ClassIndex;
const heap = zag.heap;
const HeapHeader = heap.HeapHeader;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
const InMemory = @import("inMemory.zig");

pub const Object = packed union {
    ref: *InMemory.PointedObject,
    immediate: packed struct(u64) {
        tag: Group,
        hash: u61,
    },
    const Group = enum(LowTagType) {
        pointer = 0,
        smallInteger = 0b001,
        character = 0b010,
        float = 0b100,
        _,
        inline fn u(cg: Group) u3 {
            return @intFromEnum(cg);
        }
    };

    const PointerTag = Group.u(.pointer);
    const SmallIntegerTag = Group.u(.smallInteger);
    const CharacterTag = Group.u(.character);
    const FloatTag = Group.u(.float);
    const TagMask = SmallIntegerTag | CharacterTag | FloatTag;

    const Self = @This();
    pub const inMemorySymbols = true;
    pub const ZERO: Object = @bitCast(@as(u64, 0));
    pub const False = Object.from(&InMemory.False, null);
    pub const True = Object.from(&InMemory.True, null);
    pub const Nil = Object.from(&InMemory.Nil, null);
    pub const LowTagType = TagAndClassType;
    pub const LowTagSmallInteger = makeImmediate(.smallInteger, 0).tagbits();
    pub const HighTagType = void;
    pub const HighTagSmallInteger = {};
    pub const PackedTagType = u3;
    pub const PackedTagSmallInteger = @intFromEnum(Group.smallInteger);
    const TagAndClassType = u3;

    pub inline fn tagbits(self: Self) TagAndClassType {
        return @truncate(self.rawU());
    }
    // Spur SmallInteger
    pub inline fn isInt(self: Object) bool {
        return (self.rawU() & SmallIntegerTag) != 0;
    }
    pub inline fn isNat(self: Object) bool {
        return self.isInt() and self.nativeI_noCheck() >= 0;
    }
    pub inline fn nativeI(self: Object) ?i64 {
        if (self.isInt()) return self.nativeI_noCheck();
        return null;
    }
    pub inline fn nativeI_noCheck(self: Object) i64 {
        return @as(i64, self.rawI()) >> 1;
    }
    pub inline fn nativeU(self: Object) ?u64 {
        if (self.isNat()) return self.nativeU_noCheck();
        return null;
    }
    pub inline fn nativeU_noCheck(self: Object) u64 {
        return @as(u64, self.rawU()) >> 1;
    }
    pub inline fn fromSmallInteger(i: i64) Object {
        return @bitCast((@as(u64, @bitCast(i)) << 1) | SmallIntegerTag);
    }

    // Spur Pointer
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

    // Immediate checks
    pub inline fn isImmediate(self: Object) bool {
        return !self.isHeap();
    }

    pub inline fn isImmediateClass(self: object.Object, class: ClassIndex) bool {
        if (self.isHeap()) return false;
        if (self.isInt()) return class == .SmallInteger;
        if (self.isFloat()) return class == .Float;
        return class == .Character;
    }

    // Character (not fully implemented)
    /// Maximum codepoint for immediate Character
    pub const MaxImmediateCharacter = 0x10FFFF; // Unicode scalar value range

    pub inline fn isCharacter(self: Object) bool {
        return (self.rawU() & CharacterTag) != 0;
    }

    // TODO: immediates only
    pub inline fn fromCharacter(codepoint: u32) Self {
        if (codepoint > MaxImmediateCharacter)
            @panic("Codepoint out of immediate Character range");
        return Self{ .raw = (@as(u64, codepoint) << 3) | CharacterTag };
    }

    // TODO: heap character check is missing
    pub inline fn characterValue(self: Self) ?u32 {
        if (self.isCharacter())
            return @intCast(self.raw >> 3);
        return null;
    }

    // Floats
    pub inline fn isImmediateFloat(self: Object) bool {
        return (self.rawU() & FloatTag) != 0;
    }
    pub inline fn isDouble(self: Object) bool {
        return self.isImmediateFloat(); // Spur also supports heap floats, not implemented here
    }

    const SIGN_MASK: u64 = 0x8000000000000000;
    const EXPONENT_MASK: u64 = 0x7FF0000000000000;
    const MANTISSA_MASK: u64 = 0x000FFFFFFFFFFFFF;
    const EXPONENT_BIAS = 896;
    const MAX_EXPONENT = EXPONENT_BIAS + 0xFF;

    pub const FloatError = error{
        NonFiniteValue,
        Underflow,
        Overflow,
    };

    pub fn encode(value: f64) FloatError!Object {
        const bits: u64 = @bitCast(value);
        const sign = (bits & SIGN_MASK) >> 63;
        const exponent = (bits & EXPONENT_MASK) >> 52;
        const mantissa = bits & MANTISSA_MASK;

        if (exponent == 0) {
            if (mantissa == 0) {
                // Positive or negative zero - encode as special zero
                return Object.makeImmediate(.float, 0);
            } else {
                // Subnormal numbers require heap allocation
                return FloatError.Underflow;
            }
        }

        if (exponent == 0x7FF) {
            // Infinity or NaN - both require heap allocation
            return FloatError.NonFiniteValue;
        }

        if (exponent <= EXPONENT_BIAS) {
            return FloatError.Underflow;
        }
        if (exponent >= MAX_EXPONENT) {
            return FloatError.Overflow;
        }

        // immediate float: [exponent(8)][mantissa(52)][sign(1)][tag(3)]
        const adjusted_exponent = exponent - EXPONENT_BIAS;
        return Object.makeImmediate(.float, @truncate((adjusted_exponent << 53) | (mantissa << 1) | sign));
    }

    pub fn decode(self: Object) f64 {
        if (!self.isFloat()) {
            @panic("Attempting to decode non-float object as float");
        }

        const hash = self.immediate.hash;
        if (hash == 0) return 0.0;

        const sign: u64 = hash & 1;
        const mantissa: u64 = (hash >> 1) & MANTISSA_MASK;
        const adjusted_exponent: u64 = hash >> 53;

        const exponent = adjusted_exponent + EXPONENT_BIAS;
        return @bitCast((sign << 63) | (exponent << 52) | mantissa);
    }

    pub inline fn isFloat(self: Object) bool {
        return (self.rawU() & TagMask) == FloatTag;
    }

    // Boolean, Nil, Symbol (stubs)
    pub inline fn isBool(self: Object) bool {
        return self.rawU() == Object.True.rawU() or self.rawU() == Object.False.rawU();
    }
    pub inline fn toBoolNoCheck(self: Object) bool {
        return self.rawU() == Object.True.rawU();
    }
    pub inline fn isSymbol() bool {
        // Spur-encoded symbols are heap objects; this is a stub
        return false;
    }
    pub inline fn isNil(self: Object) bool {
        return self.rawU() == Object.Nil.rawU();
    }

    inline fn oImm(c: Group, h: u61) Self {
        return Self{ .immediate = .{ .tag = c, .hash = h } };
    }
    pub inline fn makeImmediate(cls: Group, hash: u61) object.Object {
        return oImm(cls, hash);
    }

    // Hash helpers (stubbed)
    pub inline fn hash24(self: Object) u24 {
        return @truncate(self.rawU());
    }
    pub inline fn hash32(self: Object) u32 {
        return @truncate(self.rawU());
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
    inline fn rawU(self: Object) u64 {
        return @intFromPtr(self.ref);
    }
    inline fn rawI(self: Object) i64 {
        return @bitCast(self.rawU());
    }
    pub inline fn rawWordAddress(self: Object) u64 {
        return self.rawU() & ~TagMask;
    }

    // Conversion from Zig types
    pub inline fn from(value: anytype, maybeProcess: ?*zag.Process) Object {
        const T = @TypeOf(value);
        if (T == Object) return value;
        switch (@typeInfo(T)) {
            .int, .comptime_int => return Self.fromSmallInteger(value),
            .float => return encode(value) catch InMemory.float(value, maybeProcess),
            .comptime_float => return from(@as(f64, value), maybeProcess),
            .bool => return if (value) Object.True else Object.False,
            .null => return Object.Nil,
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

    // Conversion to Zig types (partial)
    pub fn toWithCheck(self: Object, comptime T: type, comptime check: bool) T {
        switch (T) {
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
    pub inline fn which_class(self: Object, comptime full: bool) ClassIndex {
        if (self.isInt()) return .SmallInteger;
        if (self.isCharacter()) return .Character;
        if (self.isFloat()) return .Float;
        if (full) return self.to(HeapObjectPtr).*.getClass();
        return .Object;
    }

    pub inline fn tagMethod(o: object.Object) ?Object {
        return @bitCast(@as(u64, @bitCast(o)) | 1);
    }

    pub inline fn tagMethodValue(self: Self) Object {
        return @bitCast(@as(u64, @bitCast(self)) >> 1 << 1);
    }

    pub inline fn isTaggedMethod(self: object.Object) bool {
        return (@as(u64, @bitCast(self)) & 1) != 0;
    }

    pub inline fn isMemoryAllocated(self: Object) bool {
        return if (self.isHeap()) true else false;
    }

    // TODO: Constants
    // pub const ZERO = Self.fromSmallInteger(0);
    // these have to point to valid headers... see ptr.zag (although it's not right at the moment)
    // pub const True = @bitCast(@as(u64, 0xFFFFFFFFFFFFFFFF));
    // pub const False = @bitCast(@as(u64, 0xFFFFFFFFFFFFFFFE));
    // pub const Nil = @bitCast(@as(u64, 0));

    pub usingnamespace object.ObjectFunctions;
};

test "float conversions" {
    const cases = [_]struct {
        value: f64,
        expectHeap: bool,
    }{
        // Normal numbers
        .{ .value = 5.5, .expectHeap = false },
        .{ .value = 1.0, .expectHeap = false },
        .{ .value = -1.0, .expectHeap = false },
        .{ .value = 3.14159, .expectHeap = false },

        // Zeros
        .{ .value = 0.0, .expectHeap = false },
        .{ .value = -0.0, .expectHeap = false },

        // Edge Cases
        .{ .value = std.math.floatMin(f64), .expectHeap = true },
        .{ .value = -std.math.floatMin(f64), .expectHeap = true },
        .{ .value = std.math.floatMax(f64), .expectHeap = true },
        .{ .value = -std.math.floatMax(f64), .expectHeap = true },

        // Infinities
        .{ .value = std.math.inf(f64), .expectHeap = true },
        .{ .value = -std.math.inf(f64), .expectHeap = true },

        // NaNs
        .{ .value = std.math.nan(f64), .expectHeap = true },
        .{ .value = -std.math.nan(f64), .expectHeap = true },
    };

    for (cases) |case| {
        std.debug.print("Testing value: {d}\n", .{case.value});
        const result = Object.encode(case.value);

        if (case.expectHeap) {
            try testing.expect(result == error.NonFiniteValue or
                result == error.Underflow or
                result == error.Overflow);
        } else {
            if (result) |encoded| {
                const decoded = encoded.decode();
                if (std.math.isNan(case.value)) {
                    try testing.expect(std.math.isNan(decoded));
                } else {
                    try testing.expectEqual(case.value, decoded);
                }
            } else |_| {
                try testing.expect(false); // Should not error for non-heap cases
            }
        }
    }
}
