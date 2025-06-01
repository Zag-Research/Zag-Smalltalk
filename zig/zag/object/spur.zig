const zag = @import("../zag.zig");
const object = zag.object;
const ClassIndex = object.ClassIndex;
const heap = zag.heap;
const HeapHeader = heap.HeapHeader;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;

pub const Object = packed struct(u64) {
    tag: Group,
    hash: u61,
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
    pub const LowTagType = TagAndClassType;
    pub const LowTagSmallInteger = makeImmediate(.smallInteger, 0).tagbits();
    pub const HighTagType = void;
    pub const HighTagSmallInteger = {};
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

    // TODO: Float
    pub inline fn isImmediateFloat(self: Object) bool {
        return (self.rawU() & FloatTag) != 0;
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
    inline fn oImm(c: Group, h: u61) Self {
        return Self{ .tag = c, .hash = h };
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
    pub inline fn from(value: anytype, _: ?zag.Process) Object {
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
    pub inline fn which_class(self: Object, comptime full: bool) ClassIndex {
        if (self.isInt()) return .SmallInteger;
        if (self.isCharacter()) return .Character;
        if (self.isFloat()) return .Float;
        if (full) self.to(HeapObjectPtr).*.getClass();
        return .Object;
    }

    // TODO: Constants
    // pub const ZERO = Self.fromSmallInteger(0);
    // these have to point to valid headers... see ptr.zag (although it's not right at the moment)
    // pub const True = @bitCast(@as(u64, 0xFFFFFFFFFFFFFFFF));
    // pub const False = @bitCast(@as(u64, 0xFFFFFFFFFFFFFFFE));
    // pub const Nil = @bitCast(@as(u64, 0));

    pub usingnamespace object.ObjectFunctions;
};
