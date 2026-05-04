//! This module implements Object encoding for Zag Mix encoding
const std = @import("std");
const builtin = @import("builtin");
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;
const assert = std.debug.assert;
const rotl = std.math.rotl;
const rotr = std.math.rotr;
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
const encoding = zag.config.objectEncoding;
const floatEncoding = switch (encoding) {
    .compact1 => @import("floatEncoding.zig").Fst1(1),
    .compactI1 => @import("floatEncoding.zig").Fst1(2),
    .compact2, .compactI2 => @import("floatEncoding.zig").Fst2(2),
    .compact4, .compactI4 => @import("floatEncoding.zig").Fst4,
    .compact6, .compactI6 => @import("floatEncoding.zig").Zag6,
    .compactZ => @import("floatEncoding.zig").Zag4,
    else => @compileError("No matching encoding"),
};
const encode = floatEncoding.encode;
const decode = floatEncoding.decode;
const IntType = switch (encoding) {
    .compactI1, .compactI2, .compactI4 => i62,
    .compactI6 => i61,
    .compactZ, .compact1 => i58,
    .compact2, .compact4 => i57,
    .compact6 => i56,
    else => @compileError("No matching encoding"),
};

pub const Object = packed struct(u64) {
    hash: u48 = 0,
    extra: ExtraType = 0,
    class: ClassIndex.Compact = @enumFromInt(0),
    const Self = @This();
    const ExtraType = zag.UInt(16 - @bitSizeOf(ClassIndex.Compact));
    pub const maxInt = 0x1ff_ffff_ffff_ffff;
    pub const ZERO: Object = @bitCast(@as(u64, 0));
    pub inline fn False() Object {
        return oImm(.False, 0);
    }
    pub inline fn True() Object {
        return oImm(.True, 1);
    }
    pub inline fn Nil() Object {
        return Self{ .class = .none };
    }
    pub const LowTagType = LowTag;
    pub const lowTagSmallInteger = 0;
    pub const HighTagType = ClassIndex.Compact;
    pub const highTagSmallInteger = ClassIndex.Compact.SmallInteger;
    pub const PackedTagType = ClassIndex.Compact;
    pub const packedTagSmallInteger = ClassIndex.Compact.SmallInteger;
    pub const signatureTag = 0;
    pub const LowTag = switch (encoding) {
        .compactZ => u0,
        else => u2,
    };
    pub const HighTag = u8;
    const heap: ClassIndex.Compact = if (encoding == .compactZ) .heap else .none;
    inline fn tagbits(self: Object) u64 {
        switch (encoding) {
            .compact1 => return rotl(u64, @bitCast(self), 5) & 0x1f,
            .compactZ => return @as(u64, @bitCast(self)) >> 58,
            else => return rotl(u64, @bitCast(self), 5) & 0x5f,
        }
    }

    pub inline fn taggedI(self: object.Object) ?i64 {
        if (self.isInt()) {
            @branchHint(.likely);
            switch (encoding) {
                .compactI1, .compactI2, .compactI4, .compactI6 => return @bitCast(self),
                .compactZ => return @as(i64, @bitCast(self)) << 6,
                else => return @as(i64, @bitCast(self)) << 5,
            }
        }
        return null;
    }
    pub inline fn fromTaggedI(i: i64, _: anytype, _: anytype) object.Object {
        switch (encoding) {
            .compactI1, .compactI2, .compactI4, .compactI6 => return @bitCast(i),
            .compactZ => return @bitCast(rotr(u64, @bitCast(i | @intFromEnum(ClassIndex.Compact.SmallInteger)), 6)),
            else => return @bitCast(rotr(u64, @bitCast(i | @intFromEnum(ClassIndex.Compact.SmallInteger)), 5)),
        }
    }
    pub inline fn untaggedI(self: object.Object) ?i64 {
        if (self.isInt()) {
            @branchHint(.likely);
            switch (encoding) {
                .compactI1, .compactI2, .compactI4, .compactI6 => return @as(i64, @bitCast(self)) - 1,
                .compactZ => return @as(i64, @bitCast(self)) << 6,
                else => return @as(i64, @bitCast(self)) << 5,
            }
        }
        return null;
    }
    pub inline fn fromUntaggedI(i: i64, _: anytype, _: anytype) object.Object {
        switch (encoding) {
            .compactI1, .compactI2, .compactI4, .compactI6 => return @bitCast(i + 1),
            .compactZ => return @bitCast(rotr(u64, @bitCast(i | @intFromEnum(ClassIndex.Compact.SmallInteger)), 6)),
            else => return @bitCast(rotr(u64, @bitCast(i | @intFromEnum(ClassIndex.Compact.SmallInteger)), 5)),
        }
    }

    pub inline fn nativeI(self: object.Object) ?i64 {
        if (self.untaggedI()) |int| {
            @branchHint(.likely);
            return int >> (64 - @bitSizeOf(IntType));
        }
        return null;
    }
    pub inline fn fromNativeI(i: IntType, _: anytype, _: anytype) Object {
        return fromUntaggedI(asUntaggedI(i), null, null);
    }
    pub inline fn asUntaggedI(i: IntType) i64 {
        return @as(i64, i) << (64 - @bitSizeOf(IntType));
    }
    inline fn isInt(self: object.Object) bool {
        switch (encoding) {
            .compactI1 => return self.rawU() & 1 != 0,
            .compactI2, .compactI4 => return self.rawI() & 3 == 1, // << 62 > 0,
            // return asm ( // on AARCH64
            //     "cmn xzr, %[val], lsl #62"
            //     : [ret] "=@ccgt" (-> bool)
            //     : [val] "r" (self.rawU()) // Pass the raw integer, not the struct
            // );
            .compactI6 => return self.rawU() & 7 == 1,
            else => return self.isImmediateClass(.SmallInteger),
        }
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
    inline fn isImmediateDouble(self: object.Object) bool {
        if (decode(@bitCast(self))) |_| return true;
        return false;
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
        return @truncate(self.hash >> 2);
    }
    pub fn makeSymbol(class: ClassIndex.Compact, hash: u24, arity: u4) Object {
        return makeImmediate(class, (@as(u32, hash) << 8) | @as(u32, arity) << 2);
    }
    pub inline fn isSymbol(self: object.Object) bool {
        return self.isImmediateClass(.Symbol);
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
    pub inline fn isImmediateClass(self: object.Object, comptime class: ClassIndex.Compact) bool {
        return self.tagbits() == @intFromEnum(class);
    }
    inline fn oImm(c: ClassIndex.Compact, h: u45) Self {
        return Self{ .class = c, .hash = h };
    }
    inline fn oImmAddr(c: ClassIndex.Compact, ptr: anytype, e: ExtraType) Self {
        return Self{ .class = c, .hash = @truncate(@intFromPtr(ptr)), .extra = e };
    }
    inline fn oImmContextI(c: ClassIndex.Compact, context: *Context, e: ExtraType) Self {
        return oImmAddr(c, context, @bitCast(e));
    }
    inline fn oImmContextCE(c: ClassIndex.Compact, context: *Context, c2: ClassIndex.Compact, e: u6) Self {
        return oImmAddr(c, context, (@as(ExtraType, @intFromEnum(c2)) << 6) | e);
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
    pub inline fn makeImmediate(cls: ClassIndex.Compact, hash: u45) object.Object {
        return oImm(cls, hash);
    }
    pub inline fn hash24(self: object.Object) u24 {
        return @truncate(self.rawU() >> 8);
    }
    pub inline fn hash32(self: object.Object) u32 {
        return @truncate(self.rawU());
    }

    pub fn fromAddress(value: anytype) Object {
        return oImmAddr(heap,value,0);
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
        switch (encoding) {
            .compactI2, .compactI4 => {
                if (true) {
                    const u: u64 = @bitCast(self);
                    if (decode(u)) |_| {
                        @branchHint(.likely);
                        return .Float;
                    } else if (u & 1 != 0) {
                        @branchHint(.likely);
                        return .SmallInteger;
                    }
                } else {
                    const f = @as(i64, @bitCast(self)) << 62;
                    if (f > 0) {
                        @branchHint(.likely);
                        return .SmallInteger;
                    } else if (f < 0) {
                        @branchHint(.likely);
                        return .Float;
                    }
                }
            },
            .compactI6 => {
                const u: u64 = @bitCast(self);
                if (decode(u)) |_| {
                    @branchHint(.likely);
                    return .Float;
                } else if (u & 1 != 0) {
                    @branchHint(.likely);
                    return .SmallInteger;
                }
            },
            .compactZ => {
                const class = self.class;
                if (@intFromEnum(class) < @intFromEnum(heap)) {
                    @branchHint(.likely);
                    if (@as(u64, @bitCast(self)) == 0) {
                        @branchHint(.unlikely);
                        return .UndefinedObject;
                    }
                    return class.classIndex();
                } else if (@intFromEnum(class) > @intFromEnum(heap)) {
                    return .Float;
                } else {
                    @branchHint(.unlikely);
                    return self.toUnchecked(*HeapObject).*.getClass();
                }
            },
            else => {
                const u: u64 = @bitCast(self);
                if (self.isInt()) {
                    @branchHint(.likely);
                    return .SmallInteger;
                } else if (decode(u)) |_| {
                    @branchHint(.likely);
                    return .Float;
                }
            },
        }
        const class = self.class;
        if (class != heap) {
            @branchHint(.likely);
            return self.class.classIndex();
        }
        if (@as(u64, @bitCast(self)) == 0) {
            @branchHint(.unlikely);
            return .UndefinedObject;
        }
        return self.toUnchecked(*HeapObject).*.getClass();
    }

    pub inline fn hasHeapReference(self: Object) bool {
        switch (encoding) {
            .compactZ => return self.class == heap,
            else => return self.rawU() & 3 == 0 and self.class == heap and self != Nil(),
        }
    }
    pub inline fn ifHeapObject(self: object.Object) ?*HeapObject {
        if (self.hasHeapReference()) return @ptrFromInt(@as(u64, @bitCast(self)));
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
        _ = context;
        return switch (class) {
            // FIX            .ThunkReturnObject, .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnImmediate, .ThunkReturnCharacter, .ThunkReturnFloat => oImm(class.compact(), @intCast(@intFromPtr(context) << 8 | sig.primitive())),
            else => @panic("fixme"), //null,
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
    pub inline fn makeThunk(class: ClassIndex.Compact, obj: anytype, tag: u8) Object {
        return oImm(class, @intCast((@intFromPtr(obj) << 8) | tag));
    }
    pub inline fn makeThunkNoArg(class: ClassIndex.Compact, value: u45) Object {
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
