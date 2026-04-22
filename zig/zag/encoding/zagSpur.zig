//! This module implements Object encoding for the Zag Spur encoding
//! it is a superset of Spur where the Character encoding is replaced by more general immediates
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
const floatEncoding = @import("floatEncoding.zig").Fst1;
const encode = floatEncoding.encode;
const decode = floatEncoding.decode;

const Tag = enum(u3) {
    heap = 0,
    smallInteger = 0b001,
    immediates = 0b010,
    float = 0b100,
    _,
    inline fn u(tag: Tag) u3 {
        return @intFromEnum(tag);
    }
    inline fn isSet(obj: Object, comptime tag: Tag) bool {
        if (@intFromEnum(tag) == 0) {
            return obj.rawU() & 7 == 0;
        }
        return (obj.rawU() & @intFromEnum(tag)) != 0;
    }
    inline fn setToObject(bits: u64, comptime tag: Tag) Object {
        return @bitCast(bits + @intFromEnum(tag));
    }
};

pub const Object = packed union {
    ref: ?*InMemory.PointedObject,
    immediate: packed struct(u64) {
        tag: Tag = .immediates,
        class: ClassIndex.Compact,
        hash: u56,
    },
    int: packed struct(u64) {
        tag: Tag,
        hash: u61,
    },
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
        return Self{ .ref = null };
    }
    pub const LowTagType = TagAndClassType;
    pub const lowTagSmallInteger = makeImmediate(.SmallInteger, 0).tagbits();
    pub const HighTagType = u0;
    pub const highTagSmallInteger = 0;
    pub const PackedTagType = Tag;
    pub const packedTagSmallInteger = Tag.smallInteger;
    pub const signatureTag = Tag.u(.immediates);
    const TagAndClassType = u8;
    const tagAndClassBits = @bitSizeOf(Tag) + @bitSizeOf(ClassIndex.Compact);
    comptime {
        assert(tagAndClassBits == @bitSizeOf(TagAndClassType));
    }
    const tagAndClass = (@as(u64, 1) << tagAndClassBits) - 1;
    const extraMask = 0xff;
    pub inline fn tagbits(self: Object) TagAndClassType {
        return @truncate(self.rawU());
    }
    inline fn isTag(self: Object, tag: Tag) bool {
        return Tag.isSet(self, tag);
    }

    pub inline fn untaggedI(self: Object) ?i64 {
        if (self.taggedI()) |int| return int - Tag.u(.smallInteger);
        return null;
    }
    pub inline fn taggedI(self: object.Object) ?i64 {
        if (self.isInt()) return @bitCast(self);
        return null;
    }
    pub inline fn nativeI(self: object.Object) ?i64 {
        if (self.taggedI()) |int| return int >> 3;
        return null;
    }

    pub inline fn fromTaggedI(i: i64, _: anytype, _: anytype) Object {
        return @bitCast(i);
    }
    pub inline fn fromUntaggedI(i: i64, _: anytype, _: anytype) Object {
        return @bitCast(i + @intFromEnum(Tag.smallInteger));
    }
    pub inline fn fromNativeI(i: i61, _: anytype, _: anytype) Object {
        return @bitCast((@as(i64, i) << 3) + @intFromEnum(Tag.smallInteger));
    }
    pub inline fn asUntaggedI(i: i61) i64 {
        return @as(i64, i) << 3;
    }
    inline fn isInt(self: Object) bool {
        return self.isTag(.smallInteger);
    }
    pub inline fn isNat(self: Object) bool {
        return self.isInt() and self.rawI() >= 0;
    }

    pub inline fn nativeF(self: Object) ?f64 {
        if (decode(@bitCast(self))) |flt| return flt;
        if (self.isMemoryDouble()) return self.toDoubleFromMemory();
        return null;
    }
    pub inline fn fromNativeF(t: f64, sp: SP, context: *Context) Object {
        return @bitCast(encode(t) catch {
            std.debug.print("inMemory {}\n",.{t});
            return InMemory.float(t, sp, context);
        });
    }
    pub inline fn isFloat(self: Object) bool {
        return self.isImmediateDouble() or self.isMemoryDouble();
    }
    inline fn isImmediateDouble(self: Object) bool {
        return self.isTag(.float);
    }
    pub inline fn isMemoryDouble(self: Object) bool {
        return if (self.ifHeapObject()) |ptr|
            ptr.getClass() == .Float
        else
            false;
    }
    inline fn toDoubleFromMemory(self: Object) f64 {
        return self.to(*InMemory.MemoryFloat).*.value;
    }

    pub inline fn symbolHash(self: Object) ?u24 {
        if (self.isSymbol()) return self.hash24();
        return null;
    }
    pub inline fn numArgs(self: Object) u4 {
        return @truncate(self.rawU() >> 32);
    }
    pub fn makeSymbol(class: ClassIndex.Compact, hash: u24, arity: u4) Object {
        return makeImmediate(class, @as(u32, hash) | (@as(u32, arity) << 24));
    }
    pub inline fn isSymbol(self: Object) bool {
        return self.tagbits() == comptime makeImmediate(.Symbol, 0).tagbits();
    }

    pub inline fn extraValue(self: Object) Object {
        return @bitCast(self.nativeI_noCheck() >> 8);
    }
    pub inline fn highPointer(self: Object, T: type) ?T {
        return @ptrFromInt(self.rawU() >> 16);
    }
    pub const testU = rawU;
    pub const testI = rawI;
    inline fn rawU(self: Object) u64 {
        return @bitCast(self);
    }
    inline fn rawI(self: Object) i64 {
        return @bitCast(self);
    }
    pub inline fn invalidObject(_: Object) ?u64 {
        // there are no invalid objects in this encoding
        return null;
    }
    pub inline fn isImmediateClass(self: Object, comptime class: ClassIndex.Compact) bool {
        return self.tagbits() == oImm(class, 0).tagbits();
    }
    inline fn oImm(c: ClassIndex.Compact, h: u56) Self {
        return Self{ .immediate = .{ .class = c, .hash = h } };
    }
    inline fn oImmContext(c: ClassIndex.Compact, context: *Context, e: u8) Self {
        return Self{ .immediate = .{ .class = c, .hash = @as(u56, @intCast(@intFromPtr(context))) << 8 | e } };
    }
    pub inline fn pointer(self: Object, T: type) ?T {
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
    pub inline fn makeImmediate(cls: ClassIndex.Compact, hash: u56) Object {
        return oImm(cls, hash);
    }
    pub inline fn hash24(self: Object) u24 {
        return @truncate(self.immediate.hash);
    }
    pub inline fn hash32(self: Object) u32 {
        return @truncate(self.immediate.hash);
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
                if (self.nativeF()) |flt| return flt;
            },
            i64 => {
                if (self.nativeI()) |int| return int;
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
            @branchHint(.likely);
            return .Float;
        } else if (self.isTag(.immediates)) {
            @branchHint(.unpredictable);
            return self.immediate.class.classIndex();
        } else if (self.ref) |ptr| {
            @branchHint(.likely);
            return ptr.getClass();
        }
        return .UndefinedObject;
    }
    pub inline fn hasMemoryReference(self: Object) bool {
        return if (self.isTag(.heap))
            self.rawU() != 0
        else switch (self.immediate.class) {
            .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnObject, .ThunkReturnImmediate, .ThunkLocal, .BlockAssignLocal, .ThunkInstance, .BlockAssignInstance, .ThunkHeap, .ThunkReturnCharacter, .ThunkReturnFloat => true,
            else => false, // catches the nil case
        };
    }
    pub inline fn ifHeapObject(self: Object) ?*HeapObject {
        if (self.isTag(.heap) and self.rawU() != 0) return @ptrFromInt(self.rawU());
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
    pub fn returnLocalClosure(_: Object, _: anytype) ?Object {
        return null;
    }
    pub fn immediateClosure(sig: zag.execute.Signature, sp: SP, context: *Context) ?Object {
        const class = sig.getClass();
        _ = sp;
        return switch (class) {
            .ThunkReturnObject, .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnImmediate, .ThunkReturnCharacter, .ThunkReturnFloat => oImm(class.compact(), @intCast(@intFromPtr(context) << 8 | sig.primitive())),
            else => null,
        };
    }

    pub fn extraImmediateU(obj: Object) ?u11 {
        switch (obj.immediate.class) {
            .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnImmediate, .ThunkReturnCharacter, .ThunkReturnFloat => {
                return obj.extraU();
            },
            else => {},
        }
        return null;
    }

    pub fn extraImmediateI(obj: Object) ?i11 {
        switch (obj.immediate.class) {
            .ThunkReturnObject => {
                return obj.extraI();
            },
            else => {},
        }
        return null;
    }

    pub const Special = packed struct {
        imm: TagAndClassType,
        tag: u8,
        rest: u48,
        pub fn ptr(self: Special) *Object {
            return @ptrFromInt(self.rest);
        }
        pub fn objectFrom(tact: TagAndClassType, tag: u8, p: *opaque {}) Object {
            return @bitCast(Special{ .imm = tact, .tag = tag, .rest = @intCast(@intFromPtr(p)) });
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
    pub inline fn makeThunk(class: ClassIndex.Compact, obj: anytype, tag: u8) Object {
        return oImm(class, @truncate((@intFromPtr(obj) << 8) | tag));
    }
    pub inline fn makeThunkNoArg(class: ClassIndex.Compact, value: u56) Object {
        return .oImm(class, value);
    }
    pub inline fn extraU(self: object.Object) u8 {
        return @intCast(self.immediate.hash & extraMask);
    }
    pub inline fn extraI(self: object.Object) i8 {
        return @bitCast(self.extraU());
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
    pub const toBoolNoCheck = OF.toBoolNoCheck;
    pub const isIndexable = OF.isIndexable;
    pub const isNil = OF.isNil;
    pub const isUnmoving = OF.isUnmoving;
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
