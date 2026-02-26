//! taggedPtr encoding: class index in low 16 bits, pointer/integer in high 48 bits.
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

const classBits = 16;
const classMask: u64 = (1 << classBits) - 1;
const payloadBits = 64 - classBits;
const payloadMask: u64 = (1 << payloadBits) - 1;

pub const Object = packed struct(u64) {
    class: u16,
    data: u48,

    const Self = @This();

    pub const maxInt: i64 = (1 << 47) - 1;

    pub const ZERO: Object = @bitCast(@as(u64, 0));

    pub inline fn False() Object {
        if (@inComptime()) return @bitCast(@as(u64, 0));
        return fromAddress(&InMemory.False);
    }
    pub inline fn True() Object {
        if (@inComptime()) return @bitCast(@as(u64, 0));
        return fromAddress(&InMemory.True);
    }
    pub inline fn Nil() Object {
        if (@inComptime()) return @bitCast(@as(u64, 0));
        return fromAddress(&InMemory.Nil);
    }

    pub const tagged0: i64 = @as(i64, @intFromEnum(ClassIndex.SmallInteger));
    pub const LowTagType = u16;
    pub const lowTagSmallInteger: u16 = @intFromEnum(ClassIndex.SmallInteger);
    pub const HighTagType = void;
    pub const highTagSmallInteger = {};
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
        return @bitCast(self.rawU() & ~classMask);
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
        return @bitCast(@as(u64, @bitCast(i)) | @intFromEnum(ClassIndex.SmallInteger));
    }

    pub inline fn isInt(self: object.Object) bool {
        return self.class == @intFromEnum(ClassIndex.SmallInteger);
    }
    pub inline fn isNat(self: object.Object) bool {
        return self.isInt() and self.rawI() >= 0;
    }
    pub inline fn symbol40(self: object.Object) u40 {
        return @truncate(self.toUnchecked(*InMemory.PointedObject).data.unsigned);
    }
    pub inline fn nativeI(self: object.Object) ?i64 {
        if (self.isInt()) return self.nativeI_noCheck();
        return null;
    }
    inline fn nativeI_noCheck(self: object.Object) i64 {
        return self.rawI() >> classBits;
    }
    pub inline fn nativeF(self: object.Object) ?f64 {
        if (self.isMemoryDouble()) return self.toDoubleFromMemory();
        return null;
    }
    pub inline fn isFloat(self: object.Object) bool {
        return self.isMemoryDouble();
    }
    pub inline fn nativeF_noCheck(self: object.Object) f64 {
        return self.toDoubleFromMemory();
    }
    pub inline fn fromNativeI(i: i48, _: anytype, _: anytype) Object {
        const u: u64 = @as(u64, @bitCast(@as(i64, i))) << classBits;
        return @bitCast(u | @intFromEnum(ClassIndex.SmallInteger));
    }
    pub inline fn fromNativeF(t: f64, sp: SP, context: *Context) object.Object {
        return InMemory.float(t, sp, context);
    }

    pub inline fn symbolHash(self: object.Object) ?u24 {
        if (self.isSymbol()) return self.toUnchecked(*HeapObject).header.hash;
        return null;
    }
    pub inline fn heapObject(self: object.Object) ?*InMemory.PointedObject {
        if (self.isHeapObject() and !self.equals(Nil())) return @ptrFromInt(self.heapAddr());
        return null;
    }
    pub inline fn extraValue(_: object.Object) object.Object {
        @panic("extraValue not implemented for taggedPtr");
    }
    pub inline fn withPrimitive(self: Self, prim: u64) object.Object {
        return @bitCast(self.rawU() | prim << 40);
    }
    pub const testU = rawU;
    pub const testI = rawI;
    pub inline fn rawU(self: Self) u64 {
        return @bitCast(self);
    }
    inline fn rawI(self: object.Object) i64 {
        return @bitCast(self);
    }
    pub inline fn invalidObject(_: object.Object) ?u64 {
        return null;
    }

    pub inline fn makeThunk(_: ClassIndex.Compact, _: anytype, _: u8) Object {
        @panic("makeThunk not supported in taggedPtr encoding");
    }
    pub inline fn makeThunkNoArg(_: ClassIndex.Compact, _: u56) Object {
        @panic("makeThunkNoArg not supported in taggedPtr encoding");
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
    pub fn immediateClosure(_: anytype, _: anytype, _: anytype) ?Object {
        return null;
    }

    pub fn fromAddress(value: anytype) Object {
        if (@inComptime()) return @bitCast(@as(u64, 0));
        const addr = @intFromPtr(value);
        const Child = @typeInfo(@TypeOf(value)).pointer.child;
        const cls: u16 = if (@hasField(Child, "header"))
            @intFromEnum(@as(*const Child, @ptrCast(value)).header.classIndex)
        else
            @intFromEnum(ClassIndex.none);
        return @bitCast((addr << classBits) | cls);
    }

    inline fn heapAddr(self: object.Object) usize {
        return self.rawU() >> classBits;
    }

    pub const StaticObject = struct {
        obj: InMemory.PointedObject,
        pub fn init(self: *StaticObject, comptime value: anytype) object.Object {
            const ptr: *InMemory.PointedObject = @ptrCast(self);
            switch (@typeInfo(@TypeOf(value))) {
                .int, .comptime_int => return fromNativeI(@intCast(value), {}, {}),
                .comptime_float => return fromAddress(ptr.set(.Float, value)),
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
        return @enumFromInt(self.class);
    }

    pub inline fn isHeapObject(self: object.Object) bool {
        return self.heapAddr() != 0 and self.class != @intFromEnum(ClassIndex.SmallInteger);
    }
    pub inline fn ifHeapObject(self: object.Object) ?*HeapObject {
        if (self.isHeapObject()) return @ptrFromInt(self.heapAddr());
        return null;
    }
    pub inline fn hasMemoryReference(self: object.Object) bool {
        return self.isHeapObject();
    }
    pub inline fn isImmediateClass(self: object.Object, comptime class: ClassIndex.Compact) bool {
        return self.class == @intFromEnum(class.classIndex()) and
            self.rawU() >> classBits == 0;
    }
    pub inline fn isImmediateDouble(_: object.Object) bool {
        return false;
    }
    pub inline fn isMemoryDouble(self: object.Object) bool {
        return self.heapAddr() != 0 and self.class == @intFromEnum(ClassIndex.Float);
    }
    pub inline fn isSymbol(self: object.Object) bool {
        return self.heapAddr() != 0 and self.class == @intFromEnum(ClassIndex.Symbol);
    }
    pub inline fn toBoolNoCheck(self: object.Object) bool {
        return self.rawU() == object.Object.True().rawU();
    }
    pub inline fn toDoubleNoCheck(self: object.Object) f64 {
        return self.toDoubleFromMemory();
    }
    inline fn toDoubleFromMemory(self: object.Object) f64 {
        return self.toUnchecked(*InMemory.MemoryFloat).*.value;
    }
    pub inline fn makeImmediate(cls: ClassIndex.Compact, _: u56) object.Object {
        return @bitCast(@as(u64, @intFromEnum(cls.classIndex())));
    }
    pub inline fn hash24(self: object.Object) u24 {
        if (self.ifHeapObject()) |ho| return ho.header.hash;
        return 0;
    }
    pub inline fn hash32(self: object.Object) u32 {
        if (self.heapObject()) |po| return @truncate(po.data.unsigned);
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
    pub inline fn asUntaggedI(i: i64) i64 {
        return i << classBits;
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
