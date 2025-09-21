const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const math = std.math;
const zag = @import("../zag.zig");
const config = zag.config;
const assert = std.debug.assert;
const debugError = false;
const object = zag.object;
const Object = object.Object;
const ClassIndex = object.ClassIndex;
const heap = zag.heap;
const HeapHeader = heap.HeapHeader;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
pub const TaggedPtrObject = packed struct(u64) {
    ref: HeapObjectPtr,
    const Self = @This();
    pub inline fn untaggedI(self: Object) i64 {
        _ = .{ self, unreachable };
    }
    inline fn of(comptime v: u64) Object {
        return @bitCast(v);
    }
    pub inline fn thunkImmediate(o: Object) ?Object {
        _ = .{ o, unreachable };
    }
    pub inline fn thunkImmediateValue(self: Self) Object {
        _ = .{ self, unreachable };
    }
    pub inline fn isImmediateClass(_: Object, comptime _: ClassIndex.Compact) bool {
        return false;
    }
    pub inline fn isHeap(_: Object) bool {
        return true;
    }
    pub inline fn isDouble(self: Object) bool {
        return (self.rawU() & 6) != 0;
    }
    pub inline fn oImm(c: ClassIndex.Compact, h: u56) Self {
        return Self{ .tag = .immediates, .class = c, .hash = h };
    }
    pub const ZERO = of(0);
    pub const False = oImm(.False, 0);
    pub const True = oImm(.True, 0);
    pub const Nil = Self{ .tag = .heap, .class = .none, .hash = 0 };
    pub inline fn isNat(self: Object) bool {
        return self.isInt() and self.rawI() >= 0;
    }
    pub inline fn hasPointer(_: Object) bool {
        return true;
    }
    pub inline fn highPointer(self: Object, T: type) ?T {
        return @bitCast(self);
    }
    pub inline fn pointer(self: Object, T: type) ?T {
        switch (self.tag) {
            .heap => return @ptrFromInt(self.rawU()),
            .immediates => switch (self.class) {
                .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnSmallInteger, .ThunkReturnImmediate, .ThunkReturnCharacter, .ThunkReturnFloat, .ThunkHeap, .ThunkLocal, .ThunkInstance, .BlockAssignLocal, .BlockAssignInstance, .PICPointer => return self.highPointer(T),
                else => {},
            },
            else => {},
        }
        return null;
    }
    pub inline fn toBoolNoCheck(self: Object) bool {
        return self.rawU() == Object.True.rawU();
    }
    pub inline fn toIntNoCheck(self: Object) i64 {
        return @as(i56, @bitCast(self.hash));
    }
    pub inline fn toNatNoCheck(self: Object) u64 {
        return self.hash;
    }
    pub inline fn withClass(self: Object, class: ClassIndex) Object {
        if (!self.isSymbol()) std.debug.panic("not a Symbol: {f}", self);
        return @bitCast((self.rawU() & 0xffffffffff) | (@as(u64, @intFromEnum(class)) << 40));
    }
    pub inline fn rawWordAddress(self: Object) u64 {
        return self.rawU() & 0xffff_ffff_fff8;
    }
    pub inline fn toDoubleNoCheck(self: Object) f64 {
        return decode(self);
    }
    pub inline fn makeImmediate(cls: ClassIndex.Compact, hash: u56) Object {
        return oImm(cls, hash);
    }
    pub inline fn makeThunk(cls: ClassIndex.Compact, ptr: anytype, extra: u8) Object {
        return oImm(cls, (@intFromPtr(ptr) << 8) + extra);
    }
    pub inline fn hash24(self: Object) u24 {
        return @truncate(self.hash);
    }
    pub inline fn hash48(self: Object) u48 {
        return @truncate(self.hash);
    }
    pub inline fn hash56(self: Object) u56 {
        return self.hash;
    }
    const nanMemObject = object.simpleFloat(math.nan(f64), .static);
    const pInfMemObject = object.simpleFloat(math.inf(f64), .static);
    const nInfMemObject = object.simpleFloat(-math.inf(f64), .static);
    inline fn encode(x: f64) Object {
        const u = math.rotl(u64, @bitCast(x), 4) + 2;
        if (u & 6 != 0)
            return @bitCast(u);
        if (math.isNan(x)) return Object.from(&nanMemObject);
        if (math.inf(f64) == x) return Object.from(&pInfMemObject);
        if (math.inf(f64) == -x) return Object.from(&nInfMemObject);
        return Object.Nil;
    }
    inline fn decode(self: Object) f64 {
        return @bitCast(math.rotr(u64, self.rawU() - 2, 4));
    }
    pub inline fn from(value: anytype) Object {
        const T = @TypeOf(value);
        if (T == Object) return value;
        switch (@typeInfo(T)) {
            .int, .comptime_int => return oImm(.SmallInteger, @as(u56, @bitCast(@as(i56, value)))),
            .float => return encode(value),
            .comptime_float => return encode(@as(f64, value)),
            .bool => return if (value) Object.True else Object.False,
            .null => return Object.Nil,
            .pointer => |ptr_info| {
                switch (ptr_info.size) {
                    .One, .Many => {
                        return @bitCast(@intFromPtr(value));
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
    pub inline fn which_class(self: Object, comptime full: bool) ClassIndex {
        return switch (self.tag) {
            .heap => if (self.rawU() == 0) .UndefinedObject else if (full) self.to(HeapObjectPtr).*.getClass() else .Object,
            .immediates => self.class.classIndex(),
            else => .Float,
        };
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
        return self.tagbits() == comptime Object.makeImmediate(.Symbol, 0).tagbits();
    }
    pub inline fn isImmediate(self: Object) bool {
        return self.tag == .immediates;
    }
    pub inline fn isHeapObject(self: Object) bool {
        return self.tag == .heap;
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
    pub const immediate_class = OF.immediate_class;
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
};
