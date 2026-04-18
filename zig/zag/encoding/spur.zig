//! This module implements Object encoding for Spur encoding
const std = @import("std");
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;
const assert = std.debug.assert;
const math = std.math;
const zag = @import("../zag.zig");
const trace = zag.config.trace;
const object = zag.object;
const ClassIndex = object.ClassIndex;
const Process = zag.Process;
const SP = Process.SP;
const Context = zag.Context;
const HeapHeader = zag.heap.HeapHeader;
const HeapObject = zag.heap.HeapObject;
const HeapObjectConstPtr = zag.heap.HeapObjectConstPtr;
const InMemory = zag.InMemory;
const encode = @import("floatSpur.zig").Spur.encode;
const decode = @import("floatSpur.zig").Spur.decode;

const Tag = enum(u3) {
    pointer = 0,
    smallInteger = 0b001,
    character = 0b010,
    float = 0b100,
    _,
    inline fn u(tag: Tag) u3 {
        return @intFromEnum(tag);
    }
    inline fn fromClassIndex(cls: ClassIndex) Tag {
        return switch (cls) {
            .SmallInteger => .smallInteger,
            .Character => .character,
            .Float => .float,
            else => .pointer,
        };
    }
    inline fn isSet(obj: Object, comptime tag: Tag) bool {
        if (tag == .pointer) {
            return obj.rawU() & 7 == 0;
        }
        return (obj.rawU() & @intFromEnum(tag)) != 0;
    }
    inline fn setToObject(bits: u64, comptime tag: Tag) Object {
        return @bitCast(bits + @intFromEnum(tag));
    }
    inline fn shiftToObject(bits: u64, comptime tag: Tag) Object {
        return @bitCast((bits << 3) + @intFromEnum(tag));
    }
    inline fn shiftFromObject(obj: Object) u64 {
        return @as(u64, @bitCast(obj)) >> 3;
    }
    inline fn unsetFromObject(obj: Object, comptime tag: Tag) u64 {
        return @as(u64, @bitCast(obj)) - @intFromEnum(tag);
    }
};

pub const Object = packed union {
    ref: *InMemory.PointedObject,
    immediate: packed struct(u64) {
        tag: Tag,
        hash: u61,
    },

    const Self = @This();
    pub const maxInt = 0xfff_ffff_ffff_ffff;
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
    pub const HighTagType = u0;
    pub const highTagSmallInteger = 0;
    pub const PackedTagType = Tag;
    pub const packedTagSmallInteger = Tag.smallInteger;
    pub const signatureTag = Tag.u(.smallInteger);
    const TagAndClassType = u3;

    inline fn tagbits(self: Self) TagAndClassType {
        return @truncate(self.rawU());
    }

    pub inline fn untaggedI(self: Object) ?i64 {
        if (self.isInt()) {
            @branchHint(.likely);
            return @bitCast(Tag.unsetFromObject(self, .smallInteger));
        }
        return null;
    }

    pub inline fn taggedI(self: Object) ?i64 {
        if (self.isInt()) {
            @branchHint(.likely);
            return @bitCast(self);
        }
        return null;
    }

    pub inline fn fromTaggedI(i: i64, _: anytype, _: anytype) Object {
        return @bitCast(i);
    }

    pub inline fn fromUntaggedI(i: i64, _: anytype, _: anytype) Object {
        return Tag.setToObject(@bitCast(i), .smallInteger);
    }

    inline fn isTag(self: Object, tag: Tag) bool {
        return Tag.isSet(self, tag);
    }
    inline fn isInt(self: Object) bool {
        return self.isTag(.smallInteger);
    }
    pub inline fn isNat(self: Object) bool {
        return self.isInt() and self.rawI() >= 0;
    }
    pub inline fn nativeI(self: Object) ?i64 {
        if (self.isInt()) {
            @branchHint(.likely);
            return @bitCast(Tag.shiftFromObject(self));
        }
        return null;
    }
    pub inline fn fromNativeI(i: i61, _: anytype, _: anytype) Object {
        return Tag.shiftToObject(@bitCast(@as(i64, i)), .smallInteger);
    }
    pub inline fn nativeF(self: Object) ?f64 {
        if (decode(@bitCast(self))) |flt| return flt;
        if (self.isMemoryDouble()) return self.toDoubleFromMemory();
        return null;
    }
    pub inline fn isFloat(self: Object) bool {
        return self.isImmediateDouble() or self.isMemoryDouble();
    }
    pub inline fn fromNativeF(t: f64, sp: SP, context: *Context) Object {
        return @bitCast(encode(t) catch {
            return InMemory.float(t, sp, context);
        });
    }
    pub inline fn symbolHash(self: Object) ?u24 {
        if (self.isSymbol()) return @truncate(self.hash32() >> 8);
        return null;
    }
    pub inline fn numArgs(self: Object) u4 {
        return @truncate(self.hash32());
    }
    pub fn makeSymbol(_: anytype, _: anytype, _: anytype) Object {
        @panic("not implemented");
    }
    pub inline fn isSymbol(self: Object) bool {
        // Spur-encoded symbols are heap objects
        return self.isHeapObject() and self.ref.header.classIndex == .Symbol;
    }

    inline fn isHeapObject(self: Object) bool {
        return self.isTag(.pointer);
    }

    pub inline fn extraValue(_: Object) Object {
        @panic("Not implemented");
    }
    pub inline fn highPointer(_: Object, T: type) ?T {
        @panic("Not implemented");
    }
    pub const testU = rawU;
    pub const testI = rawI;
    inline fn rawU(self: Object) u64 {
        return @bitCast(self);
    }
    inline fn rawI(self: Object) i64 {
        return @bitCast(self);
    }
    pub inline fn invalidObject(self: Object) ?u64 {
        if (@popCount(@as(u64, @bitCast(self)) & 7) != 1) return @bitCast(self);
        return null;
    }

    pub inline fn isImmediateClass(self: Object, comptime class: ClassIndex) bool {
        switch (class) {
            .SmallInteger => self.isInt(),
            .Float => self.isImmediateDouble(),
            .Character => self.isCharacter(),
            else => false,
        }
    }
    inline fn isImmediateDouble(self: Object) bool {
        return self.isTag(.float);
    }
    inline fn isMemoryDouble(self: Object) bool {
        return if (self.ifHeapObject()) |ptr|
            ptr.getClass() == .Float
        else
            false;
    }
    inline fn toDoubleFromMemory(self: Object) f64 {
        return self.to(*InMemory.MemoryFloat).*.value;
    }

    inline fn oImm(c: Tag, h: u61) Self {
        return Self{ .immediate = .{ .tag = c, .hash = h } };
    }

    pub inline fn pointer(self: Object, T: type) ?T {
        if (self.isHeapObject()) return @ptrFromInt(self.rawU());
        return null;
    }
    pub inline fn makeImmediate(cls: ClassIndex, hash: u61) Object {
        // Map ClassIndex to appropriate Tag
        return oImm(Tag.fromClassIndex(cls), hash);
    }

    // Hash helpers
    pub inline fn hash24(self: Object) u24 {
        return self.ref.header.hash;
    }
    pub inline fn hash32(self: Object) u32 {
        return @truncate(self.ref.data.unsigned);
    }

    pub fn fromAddress(value: anytype) Object {
        return @bitCast(@intFromPtr(value));
    }
    pub const StaticObject = struct {
        obj: InMemory.PointedObject,
        pub fn init(self: *StaticObject, value: anytype) Object {
            const ptr: *InMemory.PointedObject = @ptrCast(self);
            switch (@typeInfo(@TypeOf(value))) {
                .int, .comptime_int => return fromNativeI(@intCast(value), {}, {}),
                .comptime_float => {
                    if (encode(value)) |encoded| {
                        return @bitCast(encoded);
                    } else |_| return fromAddress(ptr.set(.Float, value));
                },
                .bool => return if (value) Object.True() else Object.False(),
                else => @panic("Unsupported type for compile-time object creation"),
            }
        }
    };
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

    // Conversion to Zig types (partial)
    pub fn toWithCheck(self: Object, comptime T: type, comptime check: bool) T {
        switch (T) {
            f64 => {
                if (self.nativeF()) |flt| return flt;
            },
            i64 => {
                if (self.nativeI()) |int| return int;
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
                                if (!check or (self.hasMemoryReference() and (!@hasDecl(ptrInfo.child, "ClassIndex") or self.to(HeapObjectConstPtr).classIndex == ptrInfo.child.ClassIndex))) {
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
    pub inline fn which_class(self: Object) ClassIndex {
        if (self.isTag(.smallInteger)) {
            @branchHint(.likely);
            return .SmallInteger;
        } else if (self.isTag(.float)) {
            @branchHint(.none);
            return .Float;
        } else if (self.isCharacter()) {
            @branchHint(.unlikely);
            return .Character;
        }
        return self.ref.getClass();
    }

    pub const hasMemoryReference = isHeapObject;

    pub inline fn ifHeapObject(self: Object) ?*HeapObject {
        if (self.isHeapObject()) return @ptrFromInt(self.rawU());
        return null;
    }

    pub inline fn asUntaggedI(i: i61) i64 {
        return @as(i64, i) << 3;
    }

    pub fn returnObjectClosure(_: Object, _: anytype) ?Object {
        return null;
    }
    pub fn returnLocalClosure(_: Object, _: anytype) ?Object {
        return null;
    }
    pub fn immediateClosure(_: anytype, _: anytype, _: anytype) ?Object {
        return null;
    }

    pub fn extraImmediateU(_: Object) ?u8 {
        return null;
    }

    pub fn extraImmediateI(_: Object) ?i8 {
        return null;
    }

    pub fn extraI(_: Object) i8 {
        return 0;
    }

    // Add missing methods
    pub inline fn signature(_: Object) ?zag.execute.Signature {
        // Spur doesn't use immediate signatures like other encodings
        return null;
    }

    pub const MaxImmediateCharacter = 0x10FFFF;
    pub inline fn isCharacter(self: Object) bool {
        return self.isTag(.character);
    }
    pub inline fn fromCharacter(codepoint: u24) Self {
        if (codepoint > MaxImmediateCharacter)
            @panic("Codepoint out of immediate Character range");
        return Tag.shiftToObject(codepoint, .character);
    }
    pub inline fn characterValue(self: Self) ?u24 {
        if (self.isCharacter())
            return @intCast(Tag.shiftFromObject(self));
        return null;
    }
    const OF = object.ObjectFunctions;
    pub const PackedObject = object.PackedObject;
    pub const arrayAsSlice = OF.arrayAsSlice;
    pub const asObjectArray = OF.asObjectArray;
    pub const asZeroTerminatedString = OF.asZeroTerminatedString;
    pub const compare = OF.compare;
    pub const empty = OF.empty;
    pub const equals = OF.equals;
    pub const format = OF.format;
    pub const getField = OF.getField;
    pub const get_class = OF.get_class;
    pub const isBool = OF.isBool;
    pub const toBoolNoCheck = OF.toBoolNoCheck;
    pub const isIndexable = OF.isIndexable;
    pub const isNil = OF.isNil;
    pub const isUnmoving = OF.isUnmoving;
    pub const promoteToUnmovable = OF.promoteToUnmovable;
    pub const rawFromU = OF.rawFromU;
    pub const to = OF.to;
    pub const toUnchecked = OF.toUnchecked;
    pub const asVariable = zag.Context.asVariable;
    pub const setField = OF.setField;
    pub const tests = OF.tests;
};

test "spur" {
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
        try expect(obj.isHeapObject());
        try expectEqual(value, obj.toWithCheck(f64, false));
    }

    std.log.err("memory float conversion successful\n", .{});
}
