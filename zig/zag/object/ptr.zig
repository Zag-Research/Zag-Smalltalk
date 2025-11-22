//! This module implements Object encoding with everything a pointer to an in-memory object.
const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const math = std.math;
const zag = @import("../zag.zig");
const config = zag.config;
const assert = std.debug.assert;
const debugError = false;
const InMemory = zag.InMemory;
const object = zag.object;
const ClassIndex = object.ClassIndex;
const heap = zag.heap;
const HeapHeader = heap.HeapHeader;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
const Process = zag.Process;
const SP = Process.SP;
const Context = zag.Context;
pub const Object = packed struct(u64) {
    ref: *const InMemory.PointedObject,
    const Self = @This();
    pub const ZERO: Object = @bitCast(@as(u64, 0));
    pub inline fn False() Object {
        if (@inComptime()) {
            return Object{ .ref = undefined };
        }
        return Object.fromAddress(&InMemory.False);
    }

    pub inline fn True() Object {
        if (@inComptime()) {
            return Object{ .ref = undefined };
        }
        return Object.fromAddress(&InMemory.True);
    }

    pub inline fn Nil() Object {
        if (false and @inComptime()) {
            return Object{ .ref = undefined };
        }
        return Object.fromAddress(&InMemory.Nil);
    }
    pub const maxInt = 0x7fff_ffff_ffff_ffff;
    pub const tagged0: i64 = 0;
    pub const LowTagType = void;
    pub const lowTagSmallInteger = {};
    pub const HighTagType = void;
    pub const highTagSmallInteger = {};
    pub const PackedTagType = u3;
    pub const packedTagSmallInteger = 1;
    pub const intTag = @import("zag.zig").Object.intTag;
    pub const immediatesTag = 1;
    pub inline fn untaggedI(self: object.Object) ?i64 {
        if (self.isInt()) return self.untaggedI_noCheck();
        return null;
    }
    pub inline fn untaggedI_noCheck(self: object.Object) i64 {
        return self.ref.data.int;
    }
    pub const taggedI = untaggedI;
    pub const taggedI_noCheck = untaggedI_noCheck;
    pub inline fn fromTaggedI(i: i64, sp: SP, context: *Context) object.Object {
        return InMemory.int(i, sp, context);
    }
    pub const fromUntaggedI = fromTaggedI;
    pub inline fn symbol40(self: object.Object) u40 {
        return @truncate(self.ref.data.unsigned);
    }
    pub // inline
    fn nativeI(self: object.Object) ?i64 {
        if (self.isInt()) return self.rawI();
        return null;
    }
    pub inline fn nativeU(self: object.Object) ?u64 {
        if (self.isInt()) return self.rawU();
        return null;
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
    pub inline fn fromNativeF(t: f64, sp: SP, context: *Context) object.Object {
        return from(t, sp, context);
    }
    pub inline fn symbolHash(self: object.Object) ?u24 {
        if (self.isImmediateClass(.Symbol)) return self.ref.header.hash;
        return null;
    }
    pub inline fn heapObject(self: object.Object) ?*InMemory.PointedObject {
        return self.pointer(*InMemory.PointedObject);
    }
    pub inline fn extraValue(self: object.Object) object.Object {
        return @bitCast(self.rawU() >> 8);
    }
    pub inline fn isPICX(self: object.Object) bool {
        return self.isImmediateClass(.PICPointer);
    }
    pub inline fn extraI(self: object.Object) i8 {
        _ = .{ self, unreachable };
    }
    pub const testU = rawU;
    pub const testI = rawI;
    pub // inline
    fn rawU(self: object.Object) u64 {
        return self.ref.data.unsigned;
    }
    inline fn rawI(self: object.Object) i64 {
        return self.ref.data.int;
    }
    pub inline fn invalidObject(self: object.Object) ?u64 {
        const value: u64 = @bitCast(self);
        if (value == 0) return value;
        if (value & 7 != 0) return value;
        return null;
    }
    pub inline fn thunkImmediate(o: Object) ?Object {
        _ = .{ o, unreachable };
    }
    pub inline fn thunkImmediateValue(self: Self) Object {
        _ = .{ self, unreachable };
    }
    pub inline fn isImmediateClass(self: Object, comptime class: ClassIndex) bool {
        return self.ref.header.classIndex == class;
    }
    pub inline fn isHeap(_: Object) bool {
        return true;
    }
    pub inline fn isMemoryDouble(self: object.Object) bool {
        return self.isMemoryAllocated() and self.to(HeapObjectPtr).*.getClass() == .Float;
    }
    pub // inline
    fn isInt(self: Object) bool {
        return self.ref.header.classIndex == .SmallInteger;
    }
    pub inline fn isNat(self: Object) bool {
        return self.isInt() and self.rawI() >= 0;
    }
    pub inline fn isDouble(self: Object) bool {
        return self.ref.header.classIndex == .Float;
    }
    // pub inline fn oImm(c: ClassIndex.Compact, h: u56) Self {
    //     return Self{ .tag = .immediates, .class = c, .hash = h };
    // }
    pub inline fn hasPointer(self: Object) bool {
        return self.isMemoryAllocated();
    }
    pub inline fn highPointer(self: Object, T: type) ?T {
        return @ptrCast(self.ref.data.objects);
    }
    pub inline fn pointer(self: Object, T: type) ?T {
        if (self.isMemoryAllocated()) return @constCast(@ptrCast(self.ref));
        return null;
    }
    pub inline fn toBoolNoCheck(self: Object) bool {
        return self == Object.True();
    }
    pub inline fn toIntNoCheck(self: Object) i64 {
        return self.ref.data.int;
    }
    pub inline fn toNatNoCheck(self: Object) u64 {
        return self.ref.data.unsigned;
    }
    pub inline fn withPrimitive(self: object.Object, prim: u64) object.Object {
        return @bitCast(self.rawU() | prim << 40);
    }
    pub inline fn withClass(self: Object, class: ClassIndex) Object {
        if (!self.isSymbol()) std.debug.panic("not a Symbol: {f}", self);
        return @bitCast((self.rawU() & 0xffffffffff) | (@as(u64, @intFromEnum(class)) << 40));
    }
    pub inline fn rawWordAddress(self: Object) u64 {
        return self.rawU() & 0xffff_ffff_fff8;
    }
    inline fn toDoubleFromMemory(self: object.Object) f64 {
        return self.to(*InMemory.MemoryFloat).*.value;
    }
    pub inline fn toDoubleNoCheck(self: Object) f64 {
        return self.ref.data.float;
    }
    pub inline fn makeImmediate(cls: ClassIndex.Compact, hash: u64) Object {
        //@compileLog(cls, hash);
        _ = .{ cls, hash, unreachable };
    }
    pub inline fn makeThunk(cls: ClassIndex.Compact, ptr: anytype, extra: u8) Object {
        _ = .{ cls, ptr, extra, unreachable };
    }
    pub inline fn hash24(self: Object) u24 {
        return self.ref.header.hash;
    }
    pub inline fn hash32(self: Object) u32 {
        return @truncate(self.ref.data.unsigned);
    }
    pub // inline
    fn fromAddress(value: anytype) Object {
        return @bitCast(@intFromPtr(value));
    }
    pub const StaticObject = struct {
        obj: InMemory.PointedObject,
        pub fn init(self: *StaticObject, comptime value: anytype) object.Object {
            const ptr: *InMemory.PointedObject = @ptrCast(self);
            switch (@typeInfo(@TypeOf(value))) {
                .int, .comptime_int => return fromAddress(ptr.set(.SmallInteger, value)),
                .comptime_float => return fromAddress(ptr.set(.Float, value)),
                .bool => return if (value) object.Object.True() else object.Object.False(),
                else => @panic("Unsupported type for compile-time object creation"),
            }
        }
    };
    pub inline fn from(value: anytype, sp: SP, context: *Context) Object {
        const T = @TypeOf(value);
        if (T == Object) return value;
        switch (@typeInfo(T)) {
            .int, .comptime_int => return InMemory.int(value, sp, context),
            .float => return InMemory.float(value, sp, context),
            .comptime_float => return from(@as(f64, value), sp, context),
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
                if (!check or self.isDouble()) return self.toDoubleNoCheck();
            },
            i64 => {
                if (!check or self.isInt()) return self.toIntNoCheck();
            },
            u64 => {
                if (!check or self.isNat()) return self.toNatNoCheck();
            },
            bool => {
                if (!check or self.isBool()) return self.toBoolNoCheck();
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
    pub // inline
    fn which_class(self: Object) ClassIndex {
        return self.ref.header.classIndex;
    }
    pub inline fn isMemoryAllocated(self: Object) bool {
        return @intFromPtr(self.ref) & 0x7 == 0 and self != Object.Nil();
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
        return self.isImmediateClass(.Symbol);
    }
    pub inline fn isHeapObject(_: Object) bool {
        return true;
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
    pub const asVariable = zag.Context.asVariable;
    pub const PackedObject = object.PackedObject;
    pub const signature = zag.execute.Signature.signature;
};
