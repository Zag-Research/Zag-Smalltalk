const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const math = std.math;
const zag = @import("../zag.zig");
const config = zag.config;
const assert = std.debug.assert;
const debugError = false;
const object = zag.object;
const ClassIndex = object.ClassIndex;
const heap = zag.heap;
const HeapHeader = heap.HeapHeader;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
const Process = zag.Process;
const InMemory = @import("inMemory.zig");
pub const Object = packed struct(u64) {
    ref: *InMemory.PointedObject,
    const Self = @This();
    pub const inMemorySymbols = true;
    pub const ZERO = of(0);
    pub const False = Object.from(&InMemory.False, null);
    pub const True = Object.from(&InMemory.True, null);
    pub const Nil = Object.from(&InMemory.Nil, null);
    pub const tagged0: i64 = 0;
    pub const LowTagType = void;
    pub const LowTagSmallInteger = {};
    pub const HighTagType = void;
    pub const HighTagSmallInteger = {};
    pub const PackedTagType = u3;
    pub const PackedTagSmallInteger = 1;
    pub inline fn untaggedI(self: Object) i64 {
        _ = .{ self, unreachable };
    }
    pub inline fn untaggedI_noCheck(self: object.Object) i64 {
        return self.ref.data.int;
    }
    pub const taggedI = untaggedI;
    pub const taggedI_noCheck = untaggedI_noCheck;
    pub inline fn fromTaggedI(i: i64) object.Object {
        _ = .{ i, unreachable };
    }
    pub const fromUntaggedI = fromTaggedI;
    pub inline fn symbol40(self: object.Object) u40 {
        return self.hash32();
    }
    pub inline fn nativeI(self: object.Object) ?i64 {
        if (self.isInt()) return self.rawI();
        return null;
    }
    pub inline fn nativeU(self: object.Object) ?u64 {
        if (self.isInt()) return self.rawU();
        return null;
    }
    pub inline fn symbolHash(self: object.Object) ?u56 {
        if (self.isImmediateClass(.Symbol)) return self.ref.header.hash;
        return null;
    }
    pub inline fn extraValue(self: object.Object) object.Object {
        return @bitCast(self.rawU() >> 8);
    }
    pub inline fn isPIC(self: object.Object) bool {
        return self.isImmediateClass(.PICPointer);
    }
    pub inline fn tagMethod(o: object.Object) ?object.Object {
        return @bitCast(@as(u64, @bitCast(o)) | 1);
    }
    pub inline fn tagMethodValue(self: Self) object.Object {
        return @bitCast(@as(u64, @bitCast(self)) >> 1 << 1);
    }
    pub inline fn isTaggedMethod(self: object.Object) bool {
        return (@as(u64, @bitCast(self)) & 1) != 0;
    }
    pub inline fn extraI(self: object.Object) i8 {
        _ = .{ self, unreachable };
    }
    pub const testU = rawU;
    pub const testI = rawI;
    inline fn rawU(self: object.Object) u64 {
        return self.ref.data.unsigned;
    }
    inline fn rawI(self: object.Object) i64 {
        return self.ref.data.int;
    }
    inline fn of(comptime v: u64) object.Object {
        return @bitCast(v);
    }
    pub inline fn thunkImmediate(o: Object) ?Object {
        _ = .{ o, unreachable };
    }
    pub inline fn thunkImmediateValue(self: Self) Object {
        _ = .{ self, unreachable };
    }
    pub inline fn isImmediateClass(self: Object, class: ClassIndex) bool {
        return self.ref.header.classIndex == class;
    }
    pub inline fn isHeap(_: Object) bool {
        return true;
    }
    pub inline fn isNil(self: Object) bool {
        return self == Object.Nil;
    }
    pub inline fn isBool(self: Object) bool {
        return self == Object.False or self == Object.True;
    }
    pub inline fn isInt(self: Object) bool {
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
    pub inline fn hasPointer(_: Object) bool {
        return true;
    }
    pub inline fn highPointer(self: Object, T: type) ?T {
        return @ptrCast(self.ref.data.object);
    }
    pub inline fn pointer(self: Object, T: type) ?T {
        return @ptrCast(self.ref.data.object);
    }
    pub inline fn toBoolNoCheck(self: Object) bool {
        return self == Object.True;
    }
    pub inline fn toIntNoCheck(self: Object) i64 {
        return self.ref.data.int;
    }
    pub inline fn toNatNoCheck(self: Object) u64 {
        return self.ref.data.unsigned;
    }
    pub inline fn withClass(self: Object, class: ClassIndex) Object {
        if (!self.isSymbol()) @panic("not a Symbol");
        return @bitCast((self.rawU() & 0xffffffffff) | (@as(u64, @intFromEnum(class)) << 40));
    }
    pub inline fn rawWordAddress(self: Object) u64 {
        return self.rawU() & 0xffff_ffff_fff8;
    }
    pub inline fn toDoubleNoCheck(self: Object) f64 {
        return self.ref.data.float;
    }
    pub inline fn makeImmediate(cls: ClassIndex.Compact, hash: u56) Object {
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
        return self.ref.header.hash;
    }
    pub //inline
        fn from(value: anytype, maybeProcess: ?*Process) Object {
        const T = @TypeOf(value);
        if (T == Object) return value;
        switch (@typeInfo(T)) {
            .int, .comptime_int => return InMemory.int(value, maybeProcess),
            .float => return InMemory.float(value, maybeProcess),
            .comptime_float => return from(@as(f64, value), maybeProcess),
            .bool => return if (value) Object.True else Object.False,
            .null => return Object.Nil,
            .pointer => |ptr_info| {
                switch (ptr_info.size) {
                    .one, .many => {
                        if (! @isCompileTime()) std.debug.print("\nfrom: {*}",.{value});
                        return Object{ .ref = @alignCast(@constCast(@ptrCast(value))) };

                    },
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
    pub inline fn which_class(self: Object, _: bool) ClassIndex {
        return self.ref.header.classIndex;
    }
    pub inline fn isMemoryAllocated(self: Object) bool {
        return if (self.isHeap()) self != Object.Nil else @intFromEnum(self.class) <= @intFromEnum(ClassIndex.Compact.ThunkHeap);
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
    pub usingnamespace object.ObjectFunctions;
};
