//! taggedHigh encoding: class index in high 16 bits, pointer/integer in low 48 bits.
const std = @import("std");
const builtin = @import("builtin");
const math = std.math;
const rotl = std.math.rotl;
const rotr = std.math.rotr;
const assert = std.debug.assert;
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
const encode = @import("floatEncoding.zig").NuN.encode;
const decode = @import("floatEncoding.zig").NuN.decode;

const immFloatMask: u48 = 1;

pub const Object = packed struct(u64) {
    data: u32,
    immediateClass: ClassIndex,
    class: Compact,
    const AltObject = packed struct(u64) {
        intOrAddress: IntType,
        class: Compact,
        inline fn obj(self: AltObject) Object {
            return @bitCast(self);
        }
        inline fn from(self: Object) AltObject {
            return @bitCast(self);
        }
    };
    const alt = AltObject.from;
    inline fn u(self: Object) u64 {
        return @bitCast(self);
    }
    pub const Compact = enum(u16) {
        heap,
        SmallInteger,
        Signature,
        immediates,
        Float,
        _,
        pub inline fn classIndex(cp: Compact) ClassIndex {
            return @enumFromInt(@intFromEnum(cp));
        }
        pub inline fn from(ci: ClassIndex) Compact {
            return @enumFromInt(@intFromEnum(ci));
        }
        pub const immutableClasses = 0;
        pub const mutableClasses = 32;
    };

    const Self = @This();
    const intShift = 64 - @bitSizeOf(IntType);
    pub const IntType = i48;
    pub const maxInt: i64 = (1 << 47) - 1;

    pub const ZERO: Object = @bitCast(@as(u64, 0));

    inline fn oImm(comptime c: ClassIndex, h: u32) Self {
        switch (c) {
            .SmallInteger, .Signature => @compileError("use other methods"),
            else => return Self{ .class = .immediates, .immediateClass = c, .data = h },
        }
    }
    pub inline fn False() Object {
        return oImm(.False, 0);
    }
    pub inline fn True() Object {
        return oImm(.True, 0);
    }
    pub inline fn Nil() Object {
        return @bitCast(@as(u64, 0));
    }
    pub const LowTagType = u0;
    pub const lowTagSmallInteger = 0;
    pub const HighTagType = u16;
    pub const highTagSmallInteger: u16 = @intFromEnum(ClassIndex.SmallInteger);
    pub const PackedTagType = u8;
    pub const packedTagSmallInteger: u8 = 1;
    pub const signatureTag = 0;
    pub const LowTag = u0;
    pub const HighTag = u8;

    pub inline fn untaggedI(self: object.Object) ?i64 {
        if (self.isInt()) return untaggedI_noCheck(self);
        return null;
    }

    inline fn untaggedI_noCheck(self: object.Object) i64 {
        return @bitCast(@as(u64, @bitCast(self)) << 16);
    }

    pub inline fn taggedI(self: object.Object) ?i64 {
        if (self.isInt()) return taggedI_noCheck(self);
        return null;
    }

    inline fn taggedI_noCheck(self: object.Object) i64 {
        return @bitCast(rotl(u64, @bitCast(self), 16));
    }

    pub inline fn fromTaggedI(i: i64, _: anytype, _: anytype) object.Object {
        return @bitCast(rotr(u64, @bitCast(i), 16));
    }

    pub inline fn fromUntaggedI(i: i64, _: anytype, _: anytype) object.Object {
        return @bitCast(rotr(u64, @as(u64, @bitCast(i)) + @intFromEnum(Compact.SmallInteger), 16));
    }

    inline fn isInt(self: object.Object) bool {
        return self.class == .SmallInteger;
    }
    pub inline fn nativeI(self: object.Object) ?i64 {
        if (self.taggedI()) |int| return int >> 16;
        return null;
    }
    pub inline fn nativeF(self: object.Object) ?f64 {
        if (decode(@bitCast(self))) |int|
            return @bitCast(int);
        return null;
    }
    pub inline fn fromNativeI(i: IntType, _: anytype, _: anytype) Object {
        return (AltObject{ .intOrAddress = @bitCast(i), .class = .SmallInteger }).obj();
    }
    pub inline fn fromNativeF(t: f64, _: SP, _: *Context) object.Object {
        return @bitCast(encode(t) catch @panic("can't fail"));
    }

    pub inline fn symbolHash(self: object.Object) ?u24 {
        if (self.isSymbol()) return self.hash24();
        return null;
    }
    pub inline fn numArgs(self: Object) u4 {
        return @truncate(self.data);
    }
    pub fn makeSymbol(class: ClassIndex, hash: u24, arity: u4) Object {
        return makeImmediate(class, @as(u32, hash) << 8 | arity);
    }
    pub inline fn isSymbol(self: object.Object) bool {
        return self.isImmediateClass(.Symbol);
    }
    pub inline fn extraValue(_: object.Object) object.Object {
        @panic("extraValue not implemented for NuN");
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
        return null;
    }

    pub inline fn makeThunk(_: Compact, _: anytype, _: u8) Object {
        @panic("makeThunk not supported in taggedHigh encoding");
    }
    pub inline fn makeThunkNoArg(_: Compact, _: u56) Object {
        @panic("makeThunkNoArg not supported in taggedHigh encoding");
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
    pub fn returnObjectClosure(_: Object, _: anytype) ?Object {
        return null;
    }
    pub fn returnLocalClosure(_: Object, _: anytype) ?Object {
        return null;
    }
    pub fn immediateClosure(_: anytype, _: anytype, _: anytype) ?Object {
        return null;
    }

    pub fn fromAddress(value: anytype) Object {
        return @bitCast(@intFromPtr(value));
    }

    inline fn heapAddr(self: object.Object) usize {
        return self.u();
    }

    pub const StaticObject = struct {
        obj: InMemory.PointedObject,
        pub fn init(_: *StaticObject, comptime value: anytype) object.Object {
            return switch (@typeInfo(@TypeOf(value))) {
                .int, .comptime_int => fromNativeI(@intCast(value), {}, {}),
                .comptime_float => @bitCast(encode(value) catch @panic("can't fail")),
                .bool => if (value) object.Object.True() else object.Object.False(),
                else => @panic("Unsupported type for compile-time object creation"),
            };
        }
    };

    pub inline fn from(value: anytype, sp: SP, context: *Context) object.Object {
        const T = @TypeOf(value);
        if (T == object.Object) return value;
        switch (@typeInfo(T)) {
            .int, .comptime_int => return fromNativeI(@intCast(value), sp, context),
            .float, .comptime_float => return fromNativeF(@as(f64, value), sp, context),
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
                if (!check or self.isInt()) return @as(T, @bitCast(self));
            },
            else => {
                switch (@typeInfo(T)) {
                    .pointer => |ptrInfo| {
                        switch (@typeInfo(ptrInfo.child)) {
                            .@"fn" => {},
                            .@"struct" => {
                                if (!check or (self.hasHeapReference() and (!@hasDecl(ptrInfo.child, "ClassIndex") or self.toUnchecked(HeapObjectConstPtr).classIndex == ptrInfo.child.ClassIndex))) {
                                    if (@hasField(ptrInfo.child, "header") or (@hasDecl(ptrInfo.child, "includesHeader") and ptrInfo.child.includesHeader)) {
                                        return @as(T, @ptrFromInt(self.heapAddr()));
                                    } else {
                                        return @as(T, @ptrFromInt(@sizeOf(HeapHeader) + self.heapAddr()));
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

    pub inline fn which_class(self: object.Object) ClassIndex {
        switch (self.class) {
            .SmallInteger => {
                @branchHint(.likely);
                return .SmallInteger;
            },
            else => {
                @branchHint(.likely);
                return .Float;
            },
            .Signature => {
                @branchHint(.unlikely);
                return .SmallInteger;
            },
            .heap => {
                @branchHint(.none);
                if (self.rawU() == 0) {
                    @branchHint(.unlikely);
                    return .UndefinedObject;
                }
                return self.toUnchecked(*HeapObject).*.getClass();
            },
            .immediates => {
                @branchHint(.unlikely);
                return self.immediateClass;
            },
        }
    }

    inline fn isHeapObject(self: object.Object) bool {
        return self.class == .heap;
    }
    pub inline fn ifHeapObject(self: object.Object) ?*HeapObject {
        if (self.isHeapObject()) return @ptrFromInt(self.heapAddr());
        return null;
    }
    pub const hasHeapReference = isHeapObject;
    pub fn returnLiteralClosure(_: Object, _: *Context) ?Object {
        return null;
    }
    pub fn isImmediate(self: Object) bool {
        return self.class != .heap;
    }
    pub fn extraU(_: Object) u0 {
        @panic("not implemented");
    }
    pub inline fn isImmediateClass(self: object.Object, comptime class: ClassIndex) bool {
        return self.class == Compact.from(class) or
            self.class == .immediates and self.immediateClass == class;
    }

    pub inline fn makeImmediate(cls: ClassIndex, hash: u32) object.Object {
        if (@intFromEnum(cls) < @intFromEnum(Compact.Float))
            return Object{ .class = Compact.from(cls), .immediateClass = @enumFromInt(0), .data = @truncate(hash) };
        return Object{ .class = .immediates, .immediateClass = cls, .data = hash };
    }
    pub inline fn hash24(self: object.Object) u24 {
        if (self.isSymbol()) return @truncate(self.data >> 8);
        if (self.ifHeapObject()) |ho| return ho.header.hash;
        return 0;
    }
    pub inline fn hash32(self: object.Object) u32 {
        if (self.isSymbol()) return @truncate(self.data);
        if (self.ifHeapObject()) |ho| {
            const po: *InMemory.PointedObject = @ptrCast(@alignCast(ho));
            return @truncate(po.data.unsigned);
        }
        return 0;
    }
    pub fn encodedPointer(_: Object, T: type) ?T {
        @panic("Not implemented");
    }
    pub inline fn pointer(self: object.Object, T: type) ?T {
        switch (builtin.target.cpu.arch) {
            .x86_64 => {
                // Cast to a signed integer to trigger an Arithmetic Shift.
                // Shifting left by 16 discards the tag/aux metadata.
                // Shifting right copies bit 47 (the new sign bit) back into 63..48.
                const signed: isize = @bitCast(self);
                return @ptrFromInt(@as(usize, @bitCast((signed << 16) >> 16)));
            },
            else => {
                // On ARM, we use a Logical Shift (zero-filling).
                // The compiler will likely emit a single 'UBFX' instruction.
                const unsigned: usize = @bitCast(self);
                return @ptrFromInt((unsigned << 16) >> 16);
            },
        }
    }
    pub inline fn asUntaggedI(i: IntType) i64 {
        return @as(i64, i) << intShift;
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
    pub const tests = OF.tests;
};
