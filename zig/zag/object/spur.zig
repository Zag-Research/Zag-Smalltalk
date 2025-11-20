const std = @import("std");
const math = std.math;
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;
const zag = @import("../zag.zig");
const trace = zag.config.trace;
const object = zag.object;
const Process = zag.Process;
const Context = zag.Context;
const SP = Process.SP;
const testing = std.testing;
const ClassIndex = object.ClassIndex;
const heap = zag.heap;
const HeapHeader = heap.HeapHeader;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
const InMemory = zag.InMemory;
const encode = @import("floatSpur.zig").encode;
const decode = @import("floatSpur.zig").decode;

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
    const tagBits = 3;

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
    const TagAndClassType = u3;

    // Static constructor functions
    pub fn False() Object {
        if (@inComptime()) {
            return Object{ .ref = undefined };
        }
        return Object.fromAddress(&InMemory.False);
    }

    pub fn True() Object {
        if (@inComptime()) {
            return Object{ .ref = undefined };
        }
        return Object.fromAddress(&InMemory.True);
    }

    pub fn Nil() Object {
        if (false and @inComptime()) {
            return Object{ .ref = undefined };
        }
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

    pub inline fn fromTaggedI(i: i64, _: anytype, _: anytype) Object {
        return @bitCast(i);
    }

    pub inline fn fromUntaggedI(i: i64, _: anytype, _: anytype) Object {
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
        return @as(i64, self.rawI()) >> 3;
    }
    pub inline fn nativeU(self: Object) ?u64 {
        if (self.isNat()) return self.nativeU_noCheck();
        return null;
    }
    pub inline fn nativeU_noCheck(self: Object) u64 {
        return @as(u64, self.rawU()) >> 3;
    }
    pub inline fn nativeF(self: Object) ?f64 {
        if (self.isFloat()) return self.toDoubleNoCheck();
        if (self.isMemoryDouble()) return self.toDoubleFromMemory();
        return null;
    }
    pub inline fn nativeF_noCheck(self: Object) f64 {
        if (self.isFloat()) return self.toDoubleNoCheck();
        return self.toDoubleFromMemory();
    }
    pub inline fn fromSmallInteger(i: i64) Object {
        return @bitCast((@as(u64, @bitCast(i)) << 3) | SmallIntegerTag);
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
        @setRuntimeSafety(false);
        const Foo = packed struct { ref: *u64 };
        const foo = Foo{ .ref = @ptrCast(@constCast(ptr)) };
        return @bitCast(foo);
    }

    pub inline fn isImmediate(self: Object) bool {
        return !self.isHeap();
    }

    pub inline fn isImmediateClass(self: object.Object, comptime class: ClassIndex) bool {
        if (self.isHeap()) return false;
        if (self.isInt()) return class == .SmallInteger;
        if (self.isFloat()) return class == .Float;
        return class == .Character;
    }

    pub const MaxImmediateCharacter = 0x10FFFF;

    pub inline fn isCharacter(self: Object) bool {
        return (self.rawU() & CharacterTag) != 0;
    }

    pub inline fn fromCharacter(codepoint: u32) Self {
        if (codepoint > MaxImmediateCharacter)
            @panic("Codepoint out of immediate Character range");
        return Self{ .raw = (@as(u64, codepoint) << 3) | CharacterTag };
    }

    pub inline fn characterValue(self: Self) ?u32 {
        if (self.isCharacter())
            return @intCast(self.raw >> 3);
        return null;
    }

    pub inline fn isFloat(self: Object) bool {
        return (self.rawU() & TagMask) == FloatTag;
    }

    pub inline fn isBool(self: Object) bool {
        return self.rawU() == Object.True().rawU() or self.rawU() == Object.False().rawU();
    }
    pub inline fn toBoolNoCheck(self: Object) bool {
        return self.rawU() == Object.True().rawU();
    }
    pub inline fn isSymbol(self: Object) bool {
        // Spur-encoded symbols are heap objects
        return self.isHeap() and self.ref.header.classIndex == .Symbol;
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

    // Add fromNativeF for compatibility
    pub inline fn fromNativeF(t: f64, sp: SP, context: *Context) object.Object {
        return from(t, sp, context);
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
    pub inline fn rawWordAddress(self: Object) u64 {
        return self.rawU() & ~TagMask;
    }
    pub inline fn invalidObject(self: object.Object) ?u64 {
        const value: u64 = @bitCast(self);
        if (value == 0) return value;
        if (@popCount(value & 7) > 1) return value;
        return null;
    }

    pub inline fn toDoubleNoCheck(self: object.Object) f64 {
        return decode(@bitCast(self));
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
            .float => {
                if (encode(value)) |encoded| {
                    return @bitCast(encoded);
                } else |_| {
                    return InMemory.float(value, sp, context);
                }
            },
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
        return self.isMemoryAllocated() and self.ref.header.classIndex == .Float;
    }

    inline fn toDoubleFromMemory(self: object.Object) f64 {
        return self.to(*InMemory.MemoryFloat).*.value;
    }

    // Conversion to Zig types (partial)
    pub fn toWithCheck(self: Object, comptime T: type, comptime check: bool) T {
        switch (T) {
            f64 => {
                if (!check or self.isFloat()) return self.toDoubleNoCheck();
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
        if (self.isInt()) {
            @branchHint(.likely);
            return .SmallInteger;
        } else if (self.isFloat()) {
            @branchHint(.likely);
            return .Float;
        } else if (self.isCharacter()) {
            @branchHint(.unlikely);
            return .Character;
        }
        return self.ref.header.classIndex;
    }

    pub inline fn isMemoryAllocated(self: Object) bool {
        return self.isHeap();
    }

    // Add symbolHash method
    pub inline fn symbolHash(self: Object) ?u24 {
        if (self.isSymbol()) {
            return @truncate(self.hash32());
        }
        return null;
    }

    // Add missing methods
    pub inline fn signature(_: Object) ?zag.execute.Signature {
        // Spur doesn't use immediate signatures like other encodings
        return null;
    }

    pub inline fn isDouble(self: Object) bool {
        return self.isFloat() or self.isMemoryDouble();
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
        if (self.isHeap() and !self.equals(Nil())) return @ptrFromInt(self.rawU());
        return null;
    }
    pub inline fn extraValue(_: Object) Object {
        @panic("Not implemented");
    }

    pub const StaticObject = struct {
        obj: InMemory.PointedObject,
        pub fn init(self: *StaticObject, value: anytype) object.Object {
            const ptr: *InMemory.PointedObject = @ptrCast(self);
            switch (@typeInfo(@TypeOf(value))) {
                .int, .comptime_int => return fromUntaggedI(value << tagBits, {}, {}),
                .comptime_float => {
                    if (encode(value)) |encoded| {
                        return @bitCast(encoded);
                    } else |_| return fromAddress(ptr.set(.Float, value));
                },
                .bool => return if (value) object.Object.True() else object.Object.False(),
                else => @panic("Unsupported type for compile-time object creation"),
            }
        }
    };

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

test "float from/to conversion" {
    std.log.err("running test", .{});

    // Test immediate float conversion
    const testValues = [_]f64{ 1.0, -1.0, 0.0, -0.0, math.pi };

    for (testValues) |value| {
        const obj = Object.fromAddress(value);
        try expect(obj.isFloat());
        try expectEqual(value, obj.toWithCheck(f64, false));
    }

    // print immediate float conversion succesfull
    std.log.err("Immediate float conversion successful\n", .{});

    // Test edge cases that should encode successfully
    const smallest: f64 = @bitCast(@as(u64, 0x3800_0000_0000_0001));
    const largest: f64 = @bitCast(@as(u64, 0x47FF_FFFF_FFFF_FFFF));

    const edgeValues = [_]f64{ smallest, -smallest, largest, -largest };

    for (edgeValues) |value| {
        const obj = Object.fromAddress(value);
        try expect(obj.isFloat());
        try expectEqual(value, obj.toWithCheck(f64, false));
    }

    std.log.err("edge float conversion successful\n", .{});

    // Test values that should fall back to memory float
    const memoryValues = [_]f64{
        @bitCast(@as(u64, 0x3800_0000_0000_0000)), // tooSmall
        @bitCast(@as(u64, 0x4800_0000_0000_0000)), // tooLarge
    };

    for (memoryValues) |value| {
        const obj = Object.fromAddress(value);
        try expect(obj.isHeap()); // Should be heap-allocated
        try expectEqual(value, obj.toWithCheck(f64, false));
    }

    std.log.err("memory float conversion successful\n", .{});
}
