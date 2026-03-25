//! This module implements Object encoding for Zag encoding
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
const execute = zag.execute;
const Signature = execute.Signature;
const encode = @import("floatMixed.zig").encode;
const decode = @import("floatMixed.zig").decode;

pub const Tag = enum(u3) {
    heap = 0,
    immediates,
    float2,
    float3,
    float4,
    float5,
    float6,
    float7,
    inline fn u(cg: Tag) u3 {
        return @intFromEnum(cg);
    }
};
pub const Object = packed struct(u64) {
    tag: Tag,
    class: ClassIndex.Compact,
    hash: u56,
    const Self = @This();
    pub const maxInt = 0x7f_ffff_ffff_ffff;
    pub const ZERO: Object = @bitCast(@as(u64, 0));
    pub inline fn False() Object {
        return oImm(.False, 0);
    }
    pub inline fn True() Object {
        return oImm(.True, 0);
    }
    pub inline fn Nil() Object {
        return Self{ .tag = .heap, .class = .none, .hash = 0 };
    }
    pub const tagged0: i64 = @bitCast(oImm(.SmallInteger, 0));
    pub const LowTagType = TagAndClassType;
    pub const lowTagSmallInteger = makeImmediate(.SmallInteger, 0).tagbits();
    pub const HighTagType = void;
    pub const highTagSmallInteger = {};
    pub const PackedTagType = u8;
    pub const packedTagSmallInteger = intTag;
    pub const intTag = oImm(.SmallInteger, 0).tagbits();
    pub const immediatesTag = Tag.u(.immediates);
    const TagAndClassType = u8;
    const tagAndClassBits = @bitSizeOf(Tag) + @bitSizeOf(ClassIndex.Compact);
    comptime {
        assert(tagAndClassBits == @bitSizeOf(TagAndClassType));
    }
    const tagAndClass = (@as(u64, 1) << tagAndClassBits) - 1;
    const extraMask = 0xff;
    const ExtraType = u8;
    const TaggedClass = packed struct(u64) {
        tag: TagAndClassType,
        data: u56,
        inline fn from(obj: Object) TaggedClass {
            return @bitCast(obj);
        }
        inline fn dataU(self: TaggedClass) u64 {
            return self.data;
        }
        inline fn dataI(self: TaggedClass) i64 {
            return @as(i56, @bitCast(self.data));
        }
    };
    pub inline fn tagbits(self: Self) TagAndClassType {
        return TaggedClass.from(self).tag;
    }

    pub inline fn untaggedI(self: object.Object) ?i64 {
        if (self.isInt()) return untaggedI_noCheck(self);
        return null;
    }

    pub inline fn untaggedI_noCheck(self: object.Object) i64 {
        return @bitCast(self.rawU() >> tagAndClassBits << tagAndClassBits);
        //return @bitCast(self.rawU() & ~tagAndClass);
    }

    pub inline fn taggedI(self: object.Object) ?i64 {
        if (self.isInt()) return taggedI_noCheck(self);
        return null;
    }

    pub inline fn taggedI_noCheck(self: object.Object) i64 {
        return @bitCast(self);
    }

    pub inline fn fromTaggedI(i: i64, _: anytype, _: anytype) object.Object {
        return @bitCast(i);
    }

    pub inline fn fromUntaggedI(i: i64, _: anytype, _: anytype) object.Object {
        return @bitCast(i + oImm(.SmallInteger, 0).tagbits());
    }

    pub inline fn isInt(self: object.Object) bool {
        return self.isImmediateClass(.SmallInteger);
    }
    pub inline fn isNat(self: object.Object) bool {
        return self.isInt() and self.rawI() >= 0;
    }
    pub inline fn symbol40(self: object.Object) u40 {
        return @truncate(self.rawU());
    }
    pub inline fn nativeI(self: object.Object) ?i64 {
        if (self.isInt()) return self.nativeI_noCheck();
        return null;
    }
    inline fn nativeI_noCheck(self: object.Object) i64 {
        return TaggedClass.from(self).dataI();
    }
    pub const nativeF = decode;
    pub inline fn fromNativeF(t: f64, sp: SP, context: *Context) object.Object {
        return @bitCast(encode(t) catch {
            return InMemory.float(t, sp, context);
        });
    }
    pub inline fn isFloat(self: object.Object) bool {
        return self.isImmediateDouble() or self.isMemoryDouble();
    }
    pub inline fn asUntaggedI(t: i56) i64 {
        return oImm(.none, @as(u56, @bitCast(t))).untaggedI().?;
    }
    pub inline fn fromNativeI(t: i56, _: anytype, _: anytype) Object {
        return oImm(.SmallInteger, @as(u56, @bitCast(t)));
    }
    pub inline fn symbolHash(self: object.Object) ?u24 {
        if (self.isImmediateClass(.Symbol)) return @truncate(self.hash);
        return null;
    }
    pub inline fn extraValue(self: object.Object) object.Object {
        return @bitCast(self.nativeI_noCheck() >> 8);
    }
    pub inline fn withPrimitive(self: Self, prim: u64) object.Object {
        return @bitCast(self.rawU() | prim << 40);
    }
    pub const testU = rawU;
    pub const testI = rawI;
    inline fn rawU(self: Self) u64 {
        return @bitCast(self);
    }
    inline fn rawI(self: object.Object) i64 {
        return @bitCast(self);
    }
    pub inline fn invalidObject(_: object.Object) ?u64 {
        // there are no invalid objects in this encoding
        return null;
    }
    pub inline fn makeThunk(class: ClassIndex.Compact, obj: anytype, tag: u8) Object {
        return oImm(class, @intCast((@intFromPtr(obj) << 8) | tag));
    }
    pub inline fn makeThunkNoArg(class: ClassIndex.Compact, value: u56) Object {
        return .oImm(class, value);
    }
    inline fn thunkImmediate(o: object.Object) ?object.Object {
        const value: i64 = @bitCast(o);
        const shifted = value >> 55;
        if (shifted == 0 or shifted == -1)
            return oImm(.ThunkImmediate, @bitCast(@as(i56, @truncate(value))));
        return null;
    }
    inline fn thunkImmediateValue(self: Self) object.Object {
        return @bitCast(self.rawI() >> 8);
    }
    inline fn isThunkImmediate(self: object.Object) bool {
        return self.isImmediateClass(.ThunkImmediate);
    }
    pub inline fn extraU(self: object.Object) u8 {
        return @intCast(self.hash & extraMask);
    }
    pub inline fn extraI(self: object.Object) i8 {
        return @bitCast(self.extraU());
    }
    test "ThunkImmediate" {
        var process: Process align(Process.alignment) = undefined;
        process.init();
        const sp = process.getSp();
        const context = process.getContext();
        const ee = std.testing.expectEqual;
        if (thunkImmediate(object.testObjects[0])) |value|
            try ee(object.testObjects[0], value.thunkImmediateValue());
        if (thunkImmediate(object.Object.from(-42, sp, context))) |value|
            try ee(object.Object.from(-42, sp, context), value.thunkImmediateValue());
        try ee(null, thunkImmediate(object.Object.from(@as(u64, 1) << 47, sp, context)));
    }
    pub inline fn isImmediateClass(self: object.Object, comptime class: ClassIndex.Compact) bool {
        return self.tagbits() == oImm(class, 0).tagbits();
    }
    pub inline fn isImmediateDouble(self: object.Object) bool {
        return (self.rawU() & 6) != 0;
    }
    pub inline fn isMemoryDouble(self: object.Object) bool {
        return if (self.ifHeapObject()) |ptr|
            ptr.getClass() == .Float
        else
            false;
    }
    inline fn oImm(c: ClassIndex.Compact, h: u56) Self {
        return Self{ .tag = .immediates, .class = c, .hash = h };
    }
    inline fn oImmContext(c: ClassIndex.Compact, context: *Context, e: u8) Self {
        return Self{ .tag = .immediates, .class = c, .hash = @as(u56, @intCast(@intFromPtr(context))) << 8 | e };
    }
    pub inline fn hasPointer(self: object.Object) bool {
        const bits = math.rotr(TagAndClassType, self.tagbits(), 3);
        return bits <= math.rotr(TagAndClassType, oImm(.ThunkHeap, 0).tagbits(), 3) and bits != 0;
    }
    pub inline fn highPointer(self: object.Object, T: type) ?T {
        return @ptrFromInt(self.rawU() >> 16);
    }
    pub inline fn pointer(self: object.Object, T: type) ?T {
        switch (self.tag) {
            .heap => return @ptrFromInt(self.rawU()),
            .immediates => switch (self.class) {
                .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnObject, .ThunkReturnImmediate, .ThunkReturnCharacter, .ThunkReturnFloat, .ThunkHeap, .ThunkLocal, .ThunkInstance, .BlockAssignLocal, .BlockAssignInstance => return self.highPointer(T),
                else => {},
            },
            else => {},
        }
        return null;
    }
    pub inline fn toBoolNoCheck(self: object.Object) bool {
        return self.rawU() == object.Object.True().rawU();
    }
    inline fn toDoubleFromMemory(self: object.Object) f64 {
        return self.toUnchecked(*InMemory.MemoryFloat).*.value;
    }
    pub inline fn makeImmediate(cls: ClassIndex.Compact, hash: u56) object.Object {
        return oImm(cls, hash);
    }
    pub inline fn hash24(self: object.Object) u24 {
        return @truncate(self.hash);
    }
    pub inline fn hash32(self: object.Object) u32 {
        return @truncate(self.hash);
    }

    pub fn extraImmediateU(obj: Object) ?u8 {
        if (obj.isImmediateClass(.ThunkReturnLocal) or
            obj.isImmediateClass(.ThunkReturnInstance) or
            obj.isImmediateClass(.ThunkReturnImmediate) or
            obj.isImmediateClass(.ThunkReturnCharacter) or
            obj.isImmediateClass(.ThunkReturnFloat))
        {
            return obj.extraU();
        }
        return null;
    }

    pub fn extraImmediateI(obj: Object) ?i8 {
        if (obj.isImmediateClass(.ThunkReturnObject)) {
            return obj.extraI();
        }
        return null;
    }

    pub fn returnObjectClosure(self: Object, context: *Context) ?Object {
        if (self.nativeI()) |i| {
            switch (i) {
                -128...127 => return oImmContext(.ThunkReturnObject, context, @bitCast(@as(i8, @intCast(i)))),
                else => {},
            }
        } else {
            switch (self.which_class()) {
                .False, .True => |c| return oImmContext(.ThunkReturnImmediate, context, @truncate(oImm(c.compact(), 0).rawU())),
                .UndefinedObject => return oImmContext(.ThunkReturnImmediate, context, 0),
                else => {},
            }
        }
        return null;
    }
    pub fn returnLocalClosure(self: Object, context: *Context) ?Object {
        if (self.nativeI()) |i| {
            switch (i) {
                0...255 => return oImmContext(.ThunkReturnLocal, context, @bitCast(@as(i8, @intCast(i)))),
                else => {},
            }
        }
        return null;
    }
    pub fn immediateClosure(sig: Signature, sp: SP, context: *Context) ?Object {
        const class = sig.getClass();
        _ = sp;
        return switch (class) {
            .ThunkReturnObject, .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnImmediate, .ThunkReturnCharacter, .ThunkReturnFloat => oImm(class.compact(), @intCast(@intFromPtr(context) << 8 | sig.primitive())),
            else => null,
        };
    }

    pub fn fromAddress(value: anytype) Object {
        return @bitCast(@intFromPtr(value));
    }
    pub const StaticObject = struct {
        obj: InMemory.PointedObject,
        pub fn init(self: *StaticObject, comptime value: anytype) object.Object {
            const ptr: *InMemory.PointedObject = @ptrCast(self);
            switch (@typeInfo(@TypeOf(value))) {
                .int, .comptime_int => return oImm(.SmallInteger, @as(u56, @bitCast(@as(i56, value)))),
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
    pub inline fn from(value: anytype, sp: SP, context: *Context) object.Object {
        const T = @TypeOf(value);
        if (T == object.Object) return value;
        switch (@typeInfo(T)) {
            .int, .comptime_int => return fromNativeI(value, null, null),
            .float, .comptime_float => return fromNativeF(value, sp, context),
            .bool => return if (value) object.Object.True() else object.Object.False(),
            .null => return object.Object.Nil(),
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
    pub fn toWithCheck(self: object.Object, comptime T: type, comptime check: bool) T {
        switch (T) {
            f64 => {
                if (self.nativeF()) |flt| return flt;
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
                                if (!check or (self.hasMemoryReference() and (!@hasDecl(ptrInfo.child, "ClassIndex") or self.toUnchecked(HeapObjectConstPtr).classIndex == ptrInfo.child.ClassIndex))) {
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
    const class_table = blk: {
        var table = [_]ClassIndex{.Float} ** 256;
        for (0..31) |i| {
            table[i << 3] = .none;
            table[i << 3 | 1] = @enumFromInt(i);
        }
        break :blk table;
    };
    pub inline fn which_class(self: object.Object) ClassIndex {
        const u: u64 = @bitCast(self);
        if (u & 3 == 0) { // tag = 0 or 4
            @branchHint(.likely);
            return .SmallInteger;
        } else if (decode(u)) |_| { // tag = 2 or 5
            @branchHint(.likely);
            return .Float;
        } else if (u & 1 == 0) { // tag = 6 (immediate)
            @branchHint(.unpredictable);
            return self.class.classIndex();
        } else if (u & 4 == 0) { // tag = 1 (heap) or 3 (unused)
            @branchHint(.likely);
            return self.toUnchecked(*HeapObject).*.getClass();
        } else { // tag = 7 (immediate closure)
                @branchHint(.unlikely);
                self.class.classIndex();
        }
    }
    pub const Special = packed struct {
        imm: TagAndClassType,
        tag: u8,
        rest: u48,
        pub fn ptr(self: Special) *object.Object {
            return @ptrFromInt(self.rest);
        }
        pub fn objectFrom(tact: TagAndClassType, tag: u8, p: *opaque {}) object.Object {
            return @bitCast(Special{ .imm = tact, .tag = tag, .rest = @truncate(@intFromPtr(p)) });
        }
    };
    pub inline fn rawSpecial(self: object.Object) Special {
        return @bitCast(self);
    }
    pub const Scanner = struct {
        ptr: *anyopaque,
        vtable: *const VTable,
        pub const VTable = struct {
            simple: *const fn (ctx: *anyopaque, obj: object.Object) void = noSimple,
        };
        pub inline fn simple(self: Scanner, obj: object.Object) void {
            return self.vtable.simple(self.ptr, obj);
        }
        fn noSimple(ctx: *anyopaque, obj: object.Object) void {
            _ = .{ ctx, obj };
        }
    };
    pub inline fn isSymbol(self: object.Object) bool {
        return self.tagbits() == comptime makeImmediate(.Symbol, 0).tagbits();
    }
    pub inline fn isImmediateWhenNotDouble(self: object.Object) bool {
        return self.rawU() & 1 != 0;
    }
    pub inline fn isHeapObject(self: Object) bool {
        return self.tag == .heap;
    }
    pub inline fn ifHeapObject(self: object.Object) ?*HeapObject {
        if (self.tag == .heap) return @ptrFromInt(@as(u64, @bitCast(self)));
        return null;
    }
    pub inline fn hasMemoryReference(self: Object) bool {
        return if (self.ifHeapObject()) |_|
            true
        else switch (self.class) {
            .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnObject, .ThunkReturnImmediate, .ThunkLocal, .BlockAssignLocal, .ThunkInstance, .BlockAssignInstance, .ThunkHeap, .ThunkReturnCharacter, .ThunkReturnFloat => true,
            else => false, // catches the nil case
        };
    }
    const OF = object.ObjectFunctions;
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
};
