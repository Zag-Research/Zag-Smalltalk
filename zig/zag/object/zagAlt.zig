//! This module implements Object encoding for the alternative Zag encoding
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
const HeapObjectPtr = zag.heap.HeapObjectPtr;
const HeapObjectConstPtr = zag.heap.HeapObjectConstPtr;
const InMemory = zag.InMemory;
const encode = @import("floatSpur.zig").encode;
const decode = @import("floatSpur.zig").decode;

const Tag = enum(u3) {
    pointer = 0,
    smallInteger = 0b001,
    immediates = 0b010,
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

pub const Object = packed struct(u64) {
    tag: Tag,
    class: ClassIndex.Compact,
    hash: u56,
    const Self = @This();
    pub const maxInt = 0xfff_ffff_ffff_ffff;
    pub const ZERO: Object = @bitCast(@as(u64, 0));
    pub inline fn False() Object {
        return oImm(.False, 0);
    }
    pub inline fn True() Object {
        return oImm(.True, 0);
    }
    pub inline fn Nil() Object {
        return Self{ .tag = .pointer, .class = .none, .hash = 0 };
    }
    pub const tagged0: i64 = @bitCast(oImm(.SmallInteger, 0));
    pub const LowTagType = TagAndClassType;
    pub const lowTagSmallInteger = makeImmediate(.SmallInteger, 0).tagbits();
    pub const HighTagType = void;
    pub const highTagSmallInteger = {};
    pub const PackedTagType = u8;
    pub const packedTagSmallInteger = intTag;
    pub const intTag = Tag.u(.smallInteger);
    pub const immediatesTag = Tag.u(.immediates);
    const TagAndClassType = u8;
    const tagAndClassBits = @bitSizeOf(Tag) + @bitSizeOf(ClassIndex.Compact);
    comptime {
        assert(tagAndClassBits == @bitSizeOf(TagAndClassType));
    }
    const tagAndClass = (@as(u64, 1) << tagAndClassBits) - 1;
    const extraMask = 0xff;
    const ExtraType = u8;
    pub inline fn tagbits(self: Object) TagAndClassType {
        return @truncate(self.rawU());
    }

    pub inline fn untaggedI(self: Object) ?i64 {
        if (self.isInt()) return untaggedI_noCheck(self);
        return null;
    }

    pub inline fn untaggedI_noCheck(self: Object) i64 {
        return @bitCast(Tag.unsetFromObject(self, .smallInteger));
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
        return Tag.setToObject(@bitCast(i), .smallInteger);
    }

    pub inline fn isInt(self: Object) bool {
        return Tag.isSet(self, .smallInteger);
    }
    pub inline fn isNat(self: Object) bool {
        return self.isInt() and self.rawI() >= 0;
    }
    pub inline fn symbol40(self: Object) u40 {
        return @truncate(self.rawU());
    }
    pub inline fn nativeI(self: Object) ?i64 {
        if (self.isInt()) return self.nativeI_noCheck();
        return null;
    }
    inline fn nativeI_noCheck(self: Object) i64 {
        return @bitCast(Tag.shiftFromObject(self));
    }
    pub inline fn fromNativeI(i: i61, _: anytype, _: anytype) Object {
        return Tag.shiftToObject(@bitCast(@as(i64, i)), .smallInteger);
    }
    pub inline fn nativeF(self: Object) ?f64 {
        if (self.isImmediateDouble()) return self.toDoubleNoCheck();
        if (self.isMemoryDouble()) return self.toDoubleFromMemory();
        return null;
    }
    pub inline fn isFloat(self: Object) bool {
        return self.isImmediateDouble() or self.isMemoryDouble();
    }
    pub inline fn nativeF_noCheck(self: Object) f64 {
        if (self.isImmediateDouble()) return self.toDoubleNoCheck();
        return self.toDoubleFromMemory();
    }
    pub inline fn fromNativeF(t: f64, sp: SP, context: *Context) Object {
        return @bitCast(encode(t) catch {
            return InMemory.float(t, sp, context);
        });
    }
    pub inline fn symbolHash(self: Object) ?u24 {
        if (self.isImmediateClass(.Symbol)) return @truncate(self.hash);
        return null;
    }
    pub inline fn heapObject(self: Object) ?*InMemory.PointedObject {
        if (self.isHeapObject() and !self.equals(Nil())) return @ptrFromInt(self.rawU());
        return null;
    }

    pub inline fn isHeapObject(self: Object) bool {
        return Tag.isSet(self, .pointer);
    }

    pub inline fn extraValue(self: Object) Object {
        return @bitCast(self.nativeI_noCheck() >> 8);
    }
    pub inline fn withPrimitive(self: Object, prim: u64) Object {
        // This is only done for signature objects, which already aren't quite valid
        return @bitCast(self.rawU() | prim << 40);
    }
    pub const testU = rawU;
    pub const testI = rawI;
    pub inline fn rawU(self: Object) u64 {
        return @bitCast(self);
    }
    inline fn rawI(self: Object) i64 {
        return @bitCast(self);
    }
    pub inline fn invalidObject(_: Object) ?u64 {
        // there are no invalid objects in this encoding
        return null;
    }
    pub inline fn makeThunk(class: ClassIndex.Compact, obj: anytype, tag: u8) Object {
        return oImm(class, @truncate((@intFromPtr(obj) << 8) | tag));
    }
    pub inline fn makeThunkNoArg(class: ClassIndex.Compact, value: u56) Object {
        return .oImm(class, value);
    }
    inline fn thunkImmediate(o: Object) ?Object {
        const value: i64 = @bitCast(o);
        const shifted = value >> 55;
        if (shifted == 0 or shifted == -1)
            return oImm(.ThunkImmediate, @bitCast(@as(i56, @truncate(value))));
        return null;
    }
    inline fn thunkImmediateValue(self: Self) Object {
        return @bitCast(self.rawI() >> 8);
    }
    inline fn isThunkImmediate(self: Object) bool {
        return self.isImmediateClass(.ThunkImmediate);
    }
    pub inline fn extraI(self: Object) i8 {
        return @bitCast(@as(u8, @truncate(self.hash & extraMask)));
    }
    test "ThunkImmediate" {
        const ee = std.testing.expectEqual;
        if (thunkImmediate(Object.tests[0])) |value|
            try ee(Object.tests[0], value.thunkImmediateValue());
        if (thunkImmediate(Object.from(-42, undefined, undefined))) |value|
            try ee(Object.from(-42, undefined, undefined), value.thunkImmediateValue());
        try ee(null, thunkImmediate(Object.from(@as(u64, 1) << 47, undefined, undefined)));
    }
    pub inline fn isImmediateClass(self: Object, comptime class: ClassIndex.Compact) bool {
        return self.tagbits() == oImm(class, 0).tagbits();
    }
    pub inline fn isImmediateDouble(self: Object) bool {
        return Tag.isSet(self, .float);
    }
    pub inline fn isMemoryDouble(self: Object) bool {
        return self.isMemoryAllocated() and self.to(HeapObjectPtr).*.getClass() == .Float;
    }
    inline fn oImm(c: ClassIndex.Compact, h: u56) Self {
        return Self{ .tag = .immediates, .class = c, .hash = h };
    }
    // inline fn g(grp: Tag) u64 {
    //     return grp.base();
    // }
    pub inline fn hasPointer(self: Object) bool {
        const bits = math.rotr(TagAndClassType, self.tagbits(), 3);
        return bits <= math.rotr(TagAndClassType, oImm(.ThunkHeap, 0).tagbits(), 3) and bits != 0;
    }
    pub inline fn highPointer(self: Object, T: type) ?T {
        return @ptrFromInt(self.rawU() >> 16);
    }
    pub inline fn pointer(self: Object, T: type) ?T {
        switch (self.tag) {
            .pointer => return @ptrFromInt(self.rawU()),
            .immediates => switch (self.class) {
                .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnSmallInteger, .ThunkReturnImmediate, .ThunkReturnCharacter, .ThunkReturnFloat, .ThunkHeap, .ThunkLocal, .ThunkInstance, .BlockAssignLocal, .BlockAssignInstance => return self.highPointer(T),
                else => {},
            },
            else => {},
        }
        return null;
    }
    pub inline fn toBoolNoCheck(self: Object) bool {
        return self.rawU() == Object.True().rawU();
    }
    pub inline fn withClass(self: Object, class: ClassIndex) Object {
        if (!self.isSymbol()) unreachable;
        return @bitCast((self.rawU() & 0xffffffffff) | (@as(u64, @intFromEnum(class)) << 40));
    }
    pub inline fn toDoubleNoCheck(self: Object) f64 {
        return decode(@bitCast(self));
    }
    inline fn toDoubleFromMemory(self: Object) f64 {
        return self.to(*InMemory.MemoryFloat).*.value;
    }
    pub inline fn makeImmediate(cls: ClassIndex.Compact, hash: u56) Object {
        return oImm(cls, hash);
    }
    pub inline fn hash24(self: Object) u24 {
        return @truncate(self.hash);
    }
    pub inline fn hash32(self: Object) u32 {
        return @truncate(self.hash);
    }

    pub fn fromAddress(value: anytype) Object {
        return @bitCast(@intFromPtr(value));
    }
    pub const StaticObject = struct {
        obj: InMemory.PointedObject,
        pub fn init(self: *StaticObject, comptime value: anytype) Object {
            const ptr: *InMemory.PointedObject = @ptrCast(self);
            switch (@typeInfo(@TypeOf(value))) {
                .int, .comptime_int => return fromNativeI(value, {}, {}),
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
    pub fn toWithCheck(self: Object, comptime T: type, comptime check: bool) T {
        switch (T) {
            f64 => {
                if (!check or self.isImmediateDouble()) return self.toDoubleNoCheck();
                if (!check or self.isMemoryDouble()) return self.toDoubleFromMemory();
            },
            i64 => {
                if (!check or self.isInt()) return self.nativeI_noCheck();
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
    pub inline fn which_class(self: Object) ClassIndex {
        if (self.isInt()) {
            @branchHint(.likely);
            return .SmallInteger;
        } else if (Tag.isSet(self, .float)) {
            @branchHint(.likely);
            return .Float;
        } else if (Tag.isSet(self, .immediates)) {
            @branchHint(.unpredictable);
            return self.class.classIndex();
        } else if (@as(u64, @bitCast(self)) == 0) {
            @branchHint(.unlikely);
            return .UndefinedObject;
        }
        return self.to(HeapObjectPtr).*.getClass();
    }
    pub inline fn isMemoryAllocated(self: Object) bool {
        return if (self.isHeapObject()) self != Object.Nil() else @intFromEnum(self.class) <= @intFromEnum(ClassIndex.Compact.ThunkHeap);
    }
    pub const Special = packed struct {
        imm: TagAndClassType,
        tag: u8,
        rest: u48,
        pub fn ptr(self: Special) *Object {
            return @ptrFromInt(self.rest);
        }
        pub fn objectFrom(tact: TagAndClassType, tag: u8, p: *opaque {}) Object {
            return @bitCast(Special{ .imm = tact, .tag = tag, .rest = @truncate(@intFromPtr(p)) });
        }
    };
    pub inline fn rawSpecial(self: Object) Special {
        return @bitCast(self);
    }
    pub const Scanner = struct {
        ptr: *anyopaque,
        vtable: *const VTable,
        pub const VTable = struct {
            simple: *const fn (ctx: *anyopaque, obj: Object) void = noSimple,
        };
        pub inline fn simple(self: Scanner, obj: Object) void {
            return self.vtable.simple(self.ptr, obj);
        }
        fn noSimple(ctx: *anyopaque, obj: Object) void {
            _ = .{ ctx, obj };
        }
    };
    pub inline fn isSymbol(self: Object) bool {
        return self.tagbits() == comptime makeImmediate(.Symbol, 0).tagbits();
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
    pub const header = OF.header;
    pub const asVariable = zag.Context.asVariable;
    pub const PackedObject = object.PackedObject;
    pub const signature = zag.execute.Signature.signature;
    pub const tests = OF.tests;
};
