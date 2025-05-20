const zag = @import("../zag.zig");
const object = zag.object;
const Object = object.Object;
const ClassIndex = object.ClassIndex;

pub const SpurObject = packed struct(u64) {
    raw: u64,

    pub const SmallIntegerTag = 0b1;
    pub const PointerTag = 0b0;
    pub const CharacterTag = 0b11;
    pub const FloatTag = 0b101;
    pub const TagMask = 0b111;

    const Self = @This();

    // Spur SmallInteger
    pub inline fn isInt(self: Object) bool {
        return (self.rawU() & SmallIntegerTag) == SmallIntegerTag;
    }
    pub inline fn isNat(self: Object) bool {
        return self.isInt() and self.nativeI_noCheck() >= 0;
    }
    pub inline fn nativeI(self: Object) ?i64 {
        if (self.isInt()) return self.nativeI_noCheck();
        return null;
    }
    pub inline fn nativeI_noCheck(self: Object) i64 {
        return @as(i64, self.rawU()) >> 1;
    }
    pub inline fn nativeU(self: Object) ?u64 {
        if (self.isNat()) return self.nativeU_noCheck();
        return null;
    }
    pub inline fn nativeU_noCheck(self: Object) u64 {
        return @as(u64, self.rawU()) >> 1;
    }
    pub inline fn fromSmallInteger(i: i64) Object {
        return @bitCast((@as(u64, i) << 1) | SmallIntegerTag);
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
    pub inline fn fromPointer(ptr: usize) Object {
        return @bitCast(ptr & ~TagMask);
    }

    // Immediate checks
    pub inline fn isImmediate(self: Object) bool {
        return !self.isHeap();
    }

    // Character (not fully implemented)
    /// Maximum codepoint for immediate Character
    pub const MaxImmediateCharacter = 0x10FFFF; // Unicode scalar value range

    pub inline fn isCharacter(self: Object) bool {
        return (self.rawU() & TagMask) == CharacterTag;
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

    // TODO: Float
    pub inline fn isImmediateFloat(self: Object) bool {
        return (self.rawU() & TagMask) == FloatTag;
    }
    pub inline fn isDouble(self: Object) bool {
        return self.isImmediateFloat(); // Spur also supports heap floats, not implemented here
    }

    // TODO: Encoding and Decoding Float Values are missing
    // TODO: Heap support for Float is missing as well

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
    pub inline fn rawU(self: Object) u64 {
        return @bitCast(self);
    }
    pub inline fn rawI(self: Object) i64 {
        return @bitCast(self);
    }
    pub inline fn rawWordAddress(self: Object) u64 {
        return self.rawU() & ~TagMask;
    }

    // Conversion from Zig types
    pub inline fn from(value: anytype) Object {
        const T = @TypeOf(value);
        if (T == Object) return value;
        switch (@typeInfo(T)) {
            .int, .comptime_int => return Self.fromSmallInteger(value),
            .bool => return if (value) Object.True else Object.False,
            .null => return Object.Nil,
            .pointer => |ptr_info| {
                switch (ptr_info.size) {
                    .one, .many => return Self.fromPointer(@intFromPtr(value)),
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
    pub inline fn which_class(self: Object) ClassIndex {
        if (self.isInt()) return .SmallInteger;
        if (self.isHeap()) return .Object; // Could be improved
        return .Object;
    }

    // TODO: Constants
    // pub const ZERO = Self.fromSmallInteger(0);
    // pub const True = @bitCast(@as(u64, 0xFFFFFFFFFFFFFFFF));
    // pub const False = @bitCast(@as(u64, 0xFFFFFFFFFFFFFFFE));
    // pub const Nil = @bitCast(@as(u64, 0));

    pub usingnamespace object.ObjectFunctions;
};
