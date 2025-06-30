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
const InMemory = zag.InMemory;
pub const Object = packed struct(u64) {
    tag: Group,
    class: ClassIndex.Compact,
    hash: u56,
    pub const Group = enum(u3) {
        heap = 0,
        immediates,
        float2,
        float3,
        float4,
        float5,
        float6,
        float7,
        inline fn u(cg: Group) u3 {
            return @intFromEnum(cg);
        }
    };
    const Self = @This();
    pub const inMemorySymbols = false;
    pub const maxInt = 0x7f_ffff_ffff_ffff;
    pub const ZERO = of(0);
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
    pub const packedTagSmallInteger = oImm(.SmallInteger, 0).tagbits();
    const TagAndClassType = u8;
    const tagAndClassBits = enumBits(Group) + enumBits(ClassIndex.Compact);
    comptime {
        assert(tagAndClassBits == @bitSizeOf(TagAndClassType));
    }
    const tagAndClass = (@as(u64, 1) << tagAndClassBits) - 1;
    const extraMask = 0xff;
    const ExtraType = u8;
    pub inline fn asObject(self: Self) Self {
        return self;
    }
    pub inline fn tagbits(self: Self) TagAndClassType {
        return @truncate(self.rawU());
    }
    fn enumBits(T: type) usize {
        return @typeInfo(@typeInfo(T).@"enum".tag_type).int.bits;
    }
    pub inline fn untaggedI(self: object.Object) ?i64 {
        if (self.isInt()) return untaggedI_noCheck(self);
        return null;
    }
    pub inline fn untaggedI_noCheck(self: object.Object) i64 {
        return @bitCast(self.rawU() & ~tagAndClass);
    }
    pub inline fn taggedI(self: object.Object) ?i64 {
        if (self.isInt()) return taggedI_noCheck(self);
        return null;
    }
    pub inline fn taggedI_noCheck(self: object.Object) i64 {
        return @bitCast(self);
    }
    pub inline fn fromTaggedI(i: i64, _: anytype) object.Object {
        return @bitCast(i);
    }
    pub inline fn fromUntaggedI(i: i64, _: anytype) object.Object {
        return @bitCast(i + oImm(.SmallInteger, 0).tagbits());
    }
    // pub inline fn cast(v: anytype) object.Object {
    //     // stored using little-endian order
    //     return @bitCast(v);
    // }
    pub inline fn symbol40(self: object.Object) u40 {
        return @truncate(self.rawU());
    }
    pub inline fn nativeI(self: object.Object) ?i64 {
        if (self.isInt()) return self.nativeI_noCheck();
        return null;
    }
    inline fn nativeI_noCheck(self: object.Object) i64 {
        return self.rawI() >> tagAndClassBits;
    }
    pub inline fn nativeU(self: object.Object) ?u64 {
        if (self.isInt()) return self.nativeU_noCheck();
        return null;
    }
    inline fn nativeU_noCheck(self: object.Object) u64 {
        return self.hash;
    }
    pub inline fn symbolHash(self: object.Object) ?u40 {
        if (self.isImmediateClass(.Symbol)) return @truncate(self.hash);
        return null;
    }
    pub inline fn extraValue(self: object.Object) object.Object {
        return @bitCast(self.nativeI_noCheck() >> 8);
    }
    pub inline fn isPIC(self: object.Object) bool {
        return self.isImmediateClass(.PICPointer);
    }
    pub const testU = rawU;
    pub const testI = rawI;
    inline fn rawU(self: object.Object) u64 {
        return @bitCast(self);
    }
    inline fn rawI(self: object.Object) i64 {
        return @bitCast(self);
    }
    inline fn of(comptime v: u64) object.Object {
        return @bitCast(v);
    }
    pub inline fn makeThunk(class: ClassIndex.Compact, obj: anytype, tag: u8) Object {
        return oImm(class, @truncate((@intFromPtr(obj) << 8) | tag));
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
        return @bitCast(@as(u8, @truncate(self.hash & extraMask)));
    }
    test "ThunkImmediate" {
        std.debug.print("Test: ThunkImmediate\n", .{});
        const ee = std.testing.expectEqual;
        if (thunkImmediate(object.Object.from(42, null))) |value|
            try ee(object.Object.from(42, null), value.thunkImmediateValue());
        if (thunkImmediate(object.Object.from(-42, null))) |value|
            try ee(object.Object.from(-42, null), value.thunkImmediateValue());
        try ee(null, thunkImmediate(object.Object.from(@as(u64, 1) << 47, null)));
    }
    pub inline fn isImmediateClass(self: object.Object, class: ClassIndex.Compact) bool {
        return self.tagbits() == oImm(class, 0).tagbits();
    }
    pub inline fn isHeap(self: object.Object) bool {
        return self.tag == .heap;
    }
    pub inline fn isImmediateDouble(self: object.Object) bool {
        return (self.rawU() & 6) != 0;
    }
    pub inline fn isDouble(self: object.Object) bool {
        return self.isImmediateDouble() or self.isMemoryDouble();
    }
    pub inline fn isMemoryDouble(self: object.Object) bool {
        return self.isMemoryAllocated() and self.to(HeapObjectPtr).*.getClass() == .Float;
    }
    inline fn oImm(c: ClassIndex.Compact, h: u56) Self {
        return Self{ .tag = .immediates, .class = c, .hash = h };
    }
    inline fn immX(c: ClassIndex.Compact, h: u56) u64 {
        return @bitCast(oImm(c, h));
    }
    inline fn g(grp: Group) u64 {
        return grp.base();
    }
    pub inline fn isInt(self: object.Object) bool {
        return self.isImmediateClass(.SmallInteger);
    }
    pub inline fn isNat(self: object.Object) bool {
        return self.isInt() and self.rawI() >= 0;
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
                .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnSmallInteger, .ThunkReturnImmediate, .ThunkReturnCharacter, .ThunkReturnFloat, .ThunkHeap, .ThunkLocal, .ThunkInstance, .BlockAssignLocal, .BlockAssignInstance, .PICPointer => return self.highPointer(T),
                else => {},
            },
            else => {},
        }
        return null;
    }
    pub inline fn toBoolNoCheck(self: object.Object) bool {
        return self.rawU() == object.Object.True().rawU();
    }
    pub inline fn withClass(self: object.Object, class: ClassIndex) object.Object {
        if (!self.isSymbol()) @panic("not a Symbol");
        return @bitCast((self.rawU() & 0xffffffffff) | (@as(u64, @intFromEnum(class)) << 40));
    }
    pub inline fn rawWordAddress(self: object.Object) u64 {
        return self.rawU() & 0xffff_ffff_fff8;
    }
    pub inline fn toDoubleNoCheck(self: object.Object) f64 {
        return decode(self);
    }
    pub inline fn toDoubleFromMemory(self: object.Object) f64 {
        return self.to(*InMemory.MemoryFloat).*.value;
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
    inline fn encode(x: f64) !object.Object {
        const u = math.rotl(u64, @bitCast(x), 4) + 2;
        if (u & 6 != 0)
            return @bitCast(u);
        if (math.isNan(x)) return object.Object.from(&InMemory.nanMemObject, null);
        if (math.inf(f64) == x) return object.Object.from(&InMemory.pInfMemObject);
        if (math.inf(f64) == -x) return object.Object.from(&InMemory.nInfMemObject);
        return error.Unencodable;
    }
    inline fn decode(self: object.Object) f64 {
        return @bitCast(math.rotr(u64, self.rawU() - 2, 4));
    }

    pub inline fn from(value: anytype, maybeProcess: ?*Process) object.Object {
        const T = @TypeOf(value);
        if (T == object.Object) return value;
        switch (@typeInfo(T)) {
            .int, .comptime_int => return oImm(.SmallInteger, @as(u56, @bitCast(@as(i56, value)))),
            .float => return encode(value) catch {
                unreachable;
            },
            .comptime_float => return from(@as(f64, value), maybeProcess),
            .bool => return if (value) object.Object.True() else object.Object.False(),
            .null => return object.Object.Nil(),
            .pointer => |ptr_info| {
                switch (ptr_info.size) {
                    .one, .many => {
                        return @bitCast(@intFromPtr(value));
                    },
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
                if (!check or self.isImmediateDouble()) return self.toDoubleNoCheck();
                if (!check or self.isMemoryDouble()) return self.toDoubleFromMemory();
            },
            i64 => {
                if (!check or self.isInt()) return self.nativeI_noCheck();
            },
            u64 => {
                if (!check or self.isNat()) return self.nativeU_noCheck();
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
    pub inline fn which_class(self: object.Object, comptime full: bool) ClassIndex {
        return switch (self.tag) {
            .heap => if (self.rawU() == 0) .UndefinedObject else if (full) self.to(HeapObjectPtr).*.getClass() else .Object,
            .immediates => self.class.classIndex(),
            else => .Float,
        };
    }
    pub inline fn isMemoryAllocated(self: object.Object) bool {
        return if (self.isHeap()) self != object.Object.Nil() else @intFromEnum(self.class) <= @intFromEnum(ClassIndex.Compact.ThunkHeap);
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
        return self.tagbits() == comptime object.Object.makeImmediate(.Symbol, 0).tagbits();
    }
    pub inline fn isImmediate(self: object.Object) bool {
        return self.tag == .immediates;
    }
    pub inline fn isHeapObject(self: object.Object) bool {
        return self.tag == .heap;
    }
    pub usingnamespace object.ObjectFunctions;
};
