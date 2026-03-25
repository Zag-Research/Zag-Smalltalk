//! taggedHigh encoding: class index in high 16 bits, pointer/integer in low 48 bits.
const std = @import("std");
const math = std.math;
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

const immFloatMask: u48 = 1;

pub const Object = packed struct(u64) {
    intOrAddress: u48,
    class: ClassIndex,

    const Self = @This();

    pub const maxInt: i64 = (1 << 47) - 1;

    pub const ZERO: Object = @bitCast(@as(u64, 0));

    inline fn oImm(c: ClassIndex.Compact, h: u48) Self {
        return Self{ .class = c.classIndex(), .intOrAddress = h };
    }
    pub inline fn False() Object {
        return oImm(.False, 12345);
    }
    pub inline fn True() Object {
        return oImm(.True, 23451);
    }
    pub inline fn Nil() Object {
        return oImm(.UndefinedObject, 34512);
    }

    pub const tagged0: i64 = 0;
    pub const LowTagType = void;
    pub const lowTagSmallInteger = {};
    pub const HighTagType = u16;
    pub const highTagSmallInteger: u16 = @intFromEnum(ClassIndex.SmallInteger);
    pub const PackedTagType = u8;
    pub const packedTagSmallInteger: u8 = 1;
    pub const intTag: u16 = @intFromEnum(ClassIndex.SmallInteger);
    pub const immediatesTag: u8 = 1;

    pub inline fn tagbits(self: Self) u16 {
        return self.class;
    }

    pub inline fn untaggedI(self: object.Object) ?i64 {
        if (self.isInt()) return untaggedI_noCheck(self);
        return null;
    }

    pub inline fn untaggedI_noCheck(self: object.Object) i64 {
        return @bitCast(@as(u64, @bitCast(self)) << 16);
    }

    pub inline fn taggedI(self: object.Object) ?i64 {
        if (self.isInt()) return taggedI_noCheck(self);
        return null;
    }

    pub const taggedI_noCheck = untaggedI_noCheck;

    pub inline fn fromTaggedI(i: i64, _: anytype, _: anytype) object.Object {
        return @bitCast(@as(u64, @bitCast(i)) >> 16);
    }

    pub const fromUntaggedI = fromTaggedI;

    pub inline fn isInt(self: object.Object) bool {
        return self.class == .SmallInteger;
    }
    pub inline fn isNat(self: object.Object) bool {
        return self.isInt() and self.nativeI_noCheck() >= 0;
    }
    pub inline fn symbol40(self: object.Object) u40 {
        return @truncate(self.toUnchecked(*InMemory.PointedObject).data.unsigned);
    }
    pub inline fn nativeI(self: object.Object) ?i64 {
        if (self.isInt()) return self.nativeI_noCheck();
        return null;
    }
    inline fn nativeI_noCheck(self: object.Object) i64 {
        return @as(i48, @bitCast(self.intOrAddress));
    }
    pub inline fn nativeF(self: object.Object) ?f64 {
        if (self.immediateDouble()) |int|
            return @as(f32, @bitCast(int));
        if (self.class == .Float) return self.toDoubleFromMemory();
        return null;
    }
    pub inline fn isFloat(self: object.Object) bool {
        return self.class == .Float;
    }
    pub inline fn fromNativeI(i: i48, _: anytype, _: anytype) Object {
        return Object{ .intOrAddress = @bitCast(i), .class = .SmallInteger };
    }
    pub inline fn fromNativeF(t: f64, sp: SP, context: *Context) object.Object {
        if (encodeF32(t)) |encoded| return encoded;
        return InMemory.float(t, sp, context);
    }

    pub inline fn symbolHash(self: object.Object) ?u24 {
        if (self.isSymbol()) return @truncate(self.intOrAddress);
        return null;
    }
    pub inline fn extraValue(_: object.Object) object.Object {
        @panic("extraValue not implemented for taggedHigh");
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
        return null;
    }

    pub inline fn makeThunk(_: ClassIndex.Compact, _: anytype, _: u8) Object {
        @panic("makeThunk not supported in taggedHigh encoding");
    }
    pub inline fn makeThunkNoArg(_: ClassIndex.Compact, _: u56) Object {
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
        const addr = @intFromPtr(value);
        const Child = @typeInfo(@TypeOf(value)).pointer.child;
        const cls = if (@hasField(Child, "header"))
            @as(*const Child, @ptrCast(value)).header.classIndex
        else
            .none;
        return Object{ .intOrAddress = @truncate(addr), .class = cls };
    }

    inline fn heapAddr(self: object.Object) usize {
        return self.intOrAddress;
    }

    pub const StaticObject = struct {
        obj: InMemory.PointedObject,
        pub fn init(self: *StaticObject, comptime value: anytype) object.Object {
            const ptr: *InMemory.PointedObject = @ptrCast(self);
            switch (@typeInfo(@TypeOf(value))) {
                .int, .comptime_int => return fromNativeI(@intCast(value), {}, {}),
                .comptime_float => {
                    if (encodeF32(value)) |encoded| return encoded;
                    return fromAddress(ptr.set(.Float, value));
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
                if (!check or self.isInt()) return self.nativeI_noCheck();
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
                                if (!check or (self.hasMemoryReference() and (!@hasDecl(ptrInfo.child, "ClassIndex") or self.toUnchecked(HeapObjectConstPtr).classIndex == ptrInfo.child.ClassIndex))) {
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
        return self.class;
    }

    pub inline fn isHeapObject(self: object.Object) bool {
        switch (self.class) {
            .SmallInteger, .True, .False, .UndefinedObject, .Symbol => return false,
            .Float => {
                if (self.immediateDouble()) |int| {
                    return int != 0;
                } else return true;
            },
            else => return self.intOrAddress != 0,
        }
    }
    pub inline fn ifHeapObject(self: object.Object) ?*HeapObject {
        if (self.isHeapObject()) return @ptrFromInt(self.heapAddr());
        return null;
    }
    pub inline fn hasMemoryReference(self: object.Object) bool {
        return self.isHeapObject();
    }
    pub inline fn isImmediateClass(self: object.Object, comptime class: ClassIndex.Compact) bool {
        return self.class == class.classIndex();
    }
    inline fn immediateDouble(self: object.Object) ?u32 {
        const int: u64 = @bitCast(self);
        if (int & immFloatMask != 0) return @truncate(int >> 1);
        return null;
    }
    pub inline fn isMemoryDouble(self: object.Object) bool {
        return self.class == .Float and !self.isImmediateDouble() and self.intOrAddress != 0;
    }
    pub inline fn isSymbol(self: object.Object) bool {
        return self.class == .Symbol;
    }
    pub inline fn toBoolNoCheck(self: object.Object) bool {
        return self.rawU() == object.Object.True().rawU();
    }
    inline fn toDoubleFromMemory(self: object.Object) f64 {
        return self.toUnchecked(*InMemory.MemoryFloat).*.value;
    }

    inline fn encodeF32(t: f64) ?Object {
        const f: f32 = @floatCast(t);
        if (@as(f64, f) == t) {
            const payload = (@as(u64, @as(u32, @bitCast(f))) << 1) | immFloatMask;
            return Object{ .class = .Float, .intOrAddress = @truncate(payload) };
        }
        return null;
    }
    pub inline fn makeImmediate(cls: ClassIndex.Compact, hash: u56) object.Object {
        return Object{ .class = cls.classIndex(), .intOrAddress = @truncate(hash) };
    }
    pub inline fn hash24(self: object.Object) u24 {
        if (self.isSymbol()) return @truncate(self.intOrAddress);
        if (self.ifHeapObject()) |ho| return ho.header.hash;
        return 0;
    }
    pub inline fn hash32(self: object.Object) u32 {
        if (self.isSymbol()) return @truncate(self.intOrAddress);
        if (self.ifHeapObject()) |ho| {
            const po: *InMemory.PointedObject = @ptrCast(@alignCast(ho));
            return @truncate(po.data.unsigned);
        }
        return 0;
    }
    pub inline fn highPointer(self: object.Object, T: type) ?T {
        if (self.isHeapObject()) return @ptrFromInt(self.heapAddr());
        return null;
    }
    pub inline fn pointer(self: object.Object, T: type) ?T {
        if (self.isHeapObject()) return @ptrFromInt(self.heapAddr());
        return null;
    }
    pub inline fn hasPointer(self: object.Object) bool {
        return self.isHeapObject();
    }
    pub inline fn asUntaggedI(i: i48) i64 {
        return @as(i64, i) << 16;
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
    pub const signature = zag.execute.Signature.signature;
    pub const tests = OF.tests;
};
