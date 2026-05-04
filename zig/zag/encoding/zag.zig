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
const Compact = ClassIndex.Compact;
const Process = zag.Process;
const SP = Process.SP;
const Context = zag.Context;
const HeapHeader = zag.heap.HeapHeader;
const HeapObject = zag.heap.HeapObject;
const HeapObjectConstPtr = zag.heap.HeapObjectConstPtr;
const InMemory = zag.InMemory;
const execute = zag.execute;
const Signature = execute.Signature;
const floatEncoding = @import("floatEncoding.zig").Fst2(4);
const encode = floatEncoding.encode;
const decode = floatEncoding.decode;

pub const Tag = enum(u3) {
    heap = 0,
    immediates,
    smallinteger,
    unused3,
    floatA,
    floatB,
    unused6,
    unused7,
    inline fn u(cg: Tag) u3 {
        return @intFromEnum(cg);
    }
};
pub const Object = packed struct(u64) {
    tag: Tag,
    hash: u45 = 0,
    extra: u11 = 0,
    class: Compact = @enumFromInt(0),
    const Self = @This();
    pub const maxInt = 0x3fff_ffff_ffff_ffff;
    pub const ZERO: Object = @bitCast(@as(u64, 0));
    pub inline fn False() Object {
        return oImm(.False, 0);
    }
    pub inline fn True() Object {
        return oImm(.True, 0);
    }
    pub inline fn Nil() Object {
        return Self{ .tag = .heap };
    }
    pub const LowTagType = Tag;
    pub const lowTagSmallInteger = Tag.smallinteger;
    pub const HighTagType = u5;
    pub const highTagSmallInteger = 0;
    pub const PackedTagType = Tag;
    pub const packedTagSmallInteger = Tag.smallinteger;
    pub const signatureTag = 0;
    pub const LowTag = u0;
    pub const HighTag = u8;
    inline fn tagbits(self: u64) u64 {
        return math.rotl(u64, @bitCast(self), 5) & 0xff;
    }
    inline fn tagMatch(self: Object, other: Object) bool {
        return tagbits(@as(u64, @bitCast(self)) ^ @as(u64, @bitCast(other))) == 0;
    }

    pub inline fn untaggedI(self: object.Object) ?i64 {
        if (self.taggedI()) |int| {
            @branchHint(.likely);
            return int - Tag.u(.smallinteger);
        }
        return null;
    }
    pub inline fn fromUntaggedI(i: i64, _: anytype, _: anytype) object.Object {
        return @bitCast(i + Tag.u(.smallinteger));
    }

    pub inline fn taggedI(self: object.Object) ?i64 {
        if (self.isInt()) {
            @branchHint(.likely);
            return @bitCast(self);
        }
        return null;
    }
    pub inline fn fromTaggedI(i: i64, _: anytype, _: anytype) object.Object {
        return @bitCast(i);
    }

    pub inline fn nativeI(self: object.Object) ?i64 {
        if (self.taggedI()) |int| {
            @branchHint(.likely);
            return int >> 2;
        }
        return null;
    }
    pub inline fn fromNativeI(i: i62, _: anytype, _: anytype) Object {
        return fromUntaggedI(asUntaggedI(i), null, null);
    }
    pub inline fn asUntaggedI(i: i62) i64 {
        return @as(i64, i) << 2;
    }
    inline fn isInt(self: object.Object) bool {
        return @as(u64, @bitCast(self)) & Tag.u(.smallinteger) != 0;
    }
    pub inline fn isNat(self: object.Object) bool {
        return self.isInt() and self.rawI() >= 0;
    }

    pub inline fn nativeF(self: object.Object) ?f64 {
        if (decode(@bitCast(self))) |flt| {
            @branchHint(.likely);
            return flt;
        }
        if (self.isMemoryDouble()) return self.toDoubleFromMemory();
        return null;
    }
    pub inline fn fromNativeF(t: f64, sp: SP, context: *Context) object.Object {
        return @bitCast(encode(t) catch {
            return InMemory.float(t, sp, context);
        });
    }
    inline fn isMemoryDouble(self: object.Object) bool {
        return if (self.ifHeapObject()) |ptr|
            ptr.getClass() == .Float
        else
            false;
    }
    inline fn toDoubleFromMemory(self: object.Object) f64 {
        return self.toUnchecked(*InMemory.MemoryFloat).*.value;
    }

    pub inline fn symbolHash(self: Object) ?u24 {
        if (self.isSymbol()) return self.hash24();
        return null;
    }
    pub inline fn numArgs(self: Object) u4 {
        return @truncate(self.hash);
    }
    pub fn makeSymbol(class: Compact, hash: u24, arity: u4) Object {
        return makeImmediate(class, (@as(u32, hash) << 5) | @as(u32, arity));
    }
    pub inline fn isSymbol(self: object.Object) bool {
        return self.tagMatch(comptime oImm(.Symbol, 0));
    }

    pub inline fn extraValue(self: object.Object) object.Object {
        return @bitCast(self.nativeI_noCheck() >> 8);
    }
    pub inline fn highPointer(self: object.Object, T: type) ?T {
        return @ptrFromInt(self.rawU() >> 16);
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
    pub inline fn isImmediateClass(self: object.Object, comptime class: Compact) bool {
        return self.tagMatch(comptime oImm(class, 0));
    }
    inline fn oImm(c: Compact, h: u45) Self {
        return Self{ .tag = .immediates, .class = c, .hash = h };
    }
    inline fn oImmAddr(c: Compact, ptr: anytype, e: u11) Self {
        return Self{ .tag = .immediates, .class = c, .hash = @truncate(@intFromPtr(ptr) >> 8), .extra = e };
    }
    inline fn oImmContextI(c: Compact, context: *Context, e: i11) Self {
        return oImmAddr(c, context, @bitCast(e));
    }
    inline fn oImmContextCE(c: Compact, context: *Context, c2: Compact, e: u6) Self {
        return oImmAddr(c, context, (@as(u11, @intFromEnum(c2)) << 6) | e);
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
    pub inline fn makeImmediate(cls: Compact, hash: u45) object.Object {
        return oImm(cls, hash);
    }
    pub inline fn hash24(self: object.Object) u24 {
        return @truncate(self.rawU() >> 8);
    }
    pub inline fn hash32(self: object.Object) u32 {
        return @truncate(self.rawU());
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
                if (self.nativeI()) |int| return int;
            },
            bool => {
                if (!check or self.isBool()) return self.toBoolNoCheck();
            },
            object.PackedObject => {
                if (self.taggedI()) |_| return @as(T, @bitCast(self));
            },

            //u8  => {return @intCast(u8, self.hash & 0xff);},
            else => {
                switch (@typeInfo(T)) {
                    .pointer => |ptrInfo| {
                        switch (@typeInfo(ptrInfo.child)) {
                            .@"fn" => {},
                            .@"struct" => {
                                if (!check or (self.hasHeapReference() and (!@hasDecl(ptrInfo.child, "ClassIndex") or self.toUnchecked(HeapObjectConstPtr).classIndex == ptrInfo.child.ClassIndex))) {
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
    pub inline //
    fn which_class(self: object.Object) ClassIndex {
        const u: u64 = @bitCast(self);
        if (true) {
            if (u & 2 != 0) {
                @branchHint(.likely);
                return .SmallInteger;
            } else if (u & 4 != 0) {
                @branchHint(.likely);
                return .Float;
            }
        } else {
            const shift: u4 = @intCast((u & 6) << 1); // depends on SmallInteger only using 2 (i.e. 3,6,7 unused)
            const offset = @min(ClassIndex.u(.Float), ClassIndex.u(.SmallInteger)) - 1;
            assert(@max(ClassIndex.u(.Float), ClassIndex.u(.SmallInteger)) - 15 <= offset);
            const key = ((ClassIndex.u(.SmallInteger) - offset) << 12) + ((ClassIndex.u(.Float) - offset) << 8) + ((ClassIndex.u(.SmallInteger) - offset) << 4);
            switch ((key >> shift) & 15) {
                else => |tag| {
                    @branchHint(.likely);
                    return @enumFromInt(tag + offset);
                },
                0 => {},
            }
        }
        const class = self.class;
        if (class == .none) {
            if (u == 0) {
                @branchHint(.unlikely);
                return .UndefinedObject;
            }
            return self.toUnchecked(*HeapObject).*.getClass();
        } else return self.class.classIndex();
    }

    pub inline fn hasHeapReference(self: Object) bool {
        return self.tag == .heap and self != Nil();
    }
    pub inline fn ifHeapObject(self: object.Object) ?*HeapObject {
        if (self.tag == .heap and self != Nil()) return @ptrFromInt(@as(u64, @bitCast(self)));
        return null;
    }

    pub fn returnObjectClosure(self: Object, context: *Context) ?Object {
        if (self.nativeI()) |i| {
            switch (i) {
                -1024...1023 => return oImmContextI(.ThunkReturnObject, context, @intCast(i)),
                else => {},
            }
        } else {
            switch (self.which_class()) {
                .False, .True => |c| return oImmContextCE(.ThunkReturnImmediate, context, c.compact(), 0),
                .UndefinedObject => return oImmContextCE(.ThunkReturnImmediate, context, .UndefinedObject, 0),
                else => {},
            }
        }
        return null;
    }
    pub fn returnLocalClosure(self: Object, context: *Context) ?Object {
        if (self.nativeI()) |i| {
            switch (i) {
                0...2047 => return oImmAddr(.ThunkReturnLocal, context, @intCast(i)),
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

    pub fn extraImmediateU(obj: Object) ?u11 {
        switch (obj.class) {
            .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnImmediate, .ThunkReturnCharacter, .ThunkReturnFloat => {
                return obj.extraU();
            },
            else => {},
        }
        return null;
    }

    pub fn extraImmediateI(obj: Object) ?i11 {
        switch (obj.class) {
            .ThunkReturnObject => {
                return obj.extraI();
            },
            else => {},
        }
        return null;
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
    pub inline fn makeThunk(class: Compact, obj: anytype, tag: u8) Object {
        return oImm(class, @intCast((@intFromPtr(obj) << 8) | tag));
    }
    pub inline fn makeThunkNoArg(class: Compact, value: u45) Object {
        return .oImm(class, value);
    }
    pub inline fn extraU(self: object.Object) u11 {
        return self.extra;
    }
    pub inline fn extraI(self: object.Object) i11 {
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
};
