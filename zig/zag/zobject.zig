const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const math = std.math;
const config = @import("config.zig");
const assert = std.debug.assert;
const debugError = false;
const symbol = if (debugError) struct {
    const inversePhi32 = @import("utilities.zig").inversePhi(u32);
    pub inline fn fromHash32(hash: u32) Object {
        return Object.makeImmediate(.Symbol, hash);
    }
    inline fn symbol_of(index: u24, arity: u8) Object {
        return fromHash32((index << 5 | @as(u32, arity) << 1 | 1) *% inversePhi32);
    }
    pub inline fn symbol0(index: u24) Object {
        return symbol_of(index, 0);
    }
    pub inline fn symbol1(index: u24) Object {
        return symbol_of(index, 1);
    }
    const symbols = struct {
        pub const yourself = symbol0(5);
    };
    fn asString(self: Object) Object {
        return self;
    }
} else @import("symbol.zig");
const heap = @import("heap.zig");
const Age = heap.Age;
const HeapObject = heap.HeapObject;
const HeapHeader = heap.HeapHeader;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
const largerPowerOf2 = @import("utilities.zig").largerPowerOf2;
pub usingnamespace if (!builtin.is_test) struct {} else struct {
    pub const SelfObject: Object = Object.oImm(.Symbol, 0xf0000ff);
};
// test "indexSymbol" {
//     const e = std.testing.expect;
//     const ee = std.testing.expectEqual;
//     try e(Object.indexSymbol0(42).isSymbol());
//     try e(Object.indexSymbol0(42).isIndexSymbol0());
//     try ee(Object.imm(.Symbol, 0x0002a0ff), 0x2a0ff71);
//     try ee(Object.indexSymbol0(42).rawU(), 0xf00002a71);
//     try ee(Object.indexSymbol0(42).indexNumber(), 42);
//     try e(Object.indexSymbol1(42).isSymbol());
//     try e(Object.indexSymbol1(42).isIndexSymbol1());
//     try ee(Object.indexSymbol1(42).rawU(), 0xf80002a71);
//     try ee(Object.indexSymbol1(42).indexNumber(), 42);
// }
pub const ZERO = Object.ZERO;
pub const False = Object.False;
pub const True = Object.True;
pub const Nil = Object.Nil;
pub const NotAnObject = Object.NotAnObject;
pub const MinSmallInteger = Object.MinSmallInteger;
pub const MaxSmallInteger = Object.MaxSmallInteger;
pub fn fromLE(comptime T: type, v: T) Object {
    const val = @as(*const [@sizeOf(T)]u8, @ptrCast(&v));
    return Object.of(mem.readIntLittle(T, val));
}
pub const compareObject = Object.compare;
pub const ClassIndex = enum(u16) {
    none = 0,
    ThunkReturnLocal,
    ThunkReturnInstance,
    ThunkReturnSmallInteger,
    ThunkReturnImmediate,
    ThunkReturnCharacter,
    ThunkReturnFloat,
    ThunkLocal,
    BlockAssignLocal,
    ThunkInstance,
    BlockAssignInstance,
    ThunkHeap,
    ThunkImmediate,
    ThunkFloat,
    SmallInteger,
    Symbol,
    False,
    True,
    Character,
    LLVM,
    reserved = 31,
    UndefinedObject,
    Float,
    ProtoObject,
    Object,
    BlockClosure,
    Context,
    Array,
    String,
    Utf8String,
    DoubleWordArray,
    Process,
    Class,
    CompiledMethod,
    Dispatch,
    Association,
    Exception,
    Error,
    SelectorException,
    PrimitiveFailed,
    testClass = config.max_classes - 1,
    max = 0xffff - 8,
    replace7,
    replace6,
    replace5,
    replace4,
    replace3,
    replace2,
    replace1,
    replace0,
    _,
    pub const LastSpecial = @intFromEnum(Self.Dispatch);
    const Self = @This();
    // inline fn base(ci: Self) u64 {
    //     return @as(u64, @intFromEnum(ci)) << 32;
    // }
    // inline fn immediate(cg: Self) u64 {
    //     return (@as(u64, @intFromEnum(Group.immediates)) << 48) | cg.base();
    // }
    pub const Compact = enum(u5) {
        none = 0,
        ThunkReturnLocal,
        ThunkReturnInstance,
        ThunkReturnSmallInteger,
        ThunkReturnImmediate,
        ThunkReturnCharacter,
        ThunkReturnFloat,
        ThunkLocal,
        BlockAssignLocal,
        ThunkInstance,
        BlockAssignInstance,
        ThunkHeap,
        ThunkImmediate,
        ThunkFloat,
        SmallInteger,
        Symbol,
        False,
        True,
        Character,
        LLVM,
        inline fn classIndex(cp: Compact) ClassIndex {
            return @enumFromInt(@intFromEnum(cp));
        }
        pub fn thunk16(self: Compact, addr: u64, tag: u8) Object {
            return Object.oImm(self, @truncate((addr << 8) | tag));
        }
        pub fn thunk8(self: Compact, value: u64) Object {
            return Object.oImm(self, @truncate(value));
        }
    };
    pub inline fn compact(ci: ClassIndex) Compact {
        return @enumFromInt(@intFromEnum(ci));
    }
};
comptime {
    std.debug.assert(@intFromEnum(ClassIndex.replace0) == 0xffff);
    std.testing.expectEqual(@intFromEnum(ClassIndex.ThunkReturnLocal), 1) catch unreachable;
    std.debug.assert(std.meta.hasUniqueRepresentation(Object));
    for (@typeInfo(ClassIndex.Compact).@"enum".fields, @typeInfo(ClassIndex).@"enum".fields[0..@typeInfo(ClassIndex.Compact).@"enum".fields.len]) |ci, cci| {
        std.testing.expectEqual(ci, cci) catch unreachable;
    }
}
const MemoryFloat = union {
    m: [@sizeOf(Internal)]u8,
    i: Internal,
    const Internal = extern struct {
        header: HeapHeader,
        value: f64,
    };
};
pub inline fn simpleFloat(v: f64, age: Age) MemoryFloat {
    const u: u64 = @bitCast(v);
    const hash: u24 = @truncate(u ^ (u >> 24) ^ (u >> 48));
    return .{ .i = .{
        .header = .{ .classIndex = .Float, .hash = hash, .format = .notIndexable, .age = age, .length = 1 },
        .value = v,
    } };
}
pub const Object = switch (config.objectEncoding) {
    .nan => unreachable, //@import("nanObject.zig"),
    .tag => TagObject,
};
const TagObject = packed struct(u64) {
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
    pub const TagAndClassType = u8;
    pub inline fn tagbits(self: Self) TagAndClassType {
        return @truncate(@as(u64, @bitCast(self)));
    }
    fn enumBits(T: type) usize {
        return @typeInfo(@typeInfo(T).@"enum".tag_type).int.bits;
    }
    const tagAndClassBits = enumBits(Group) + enumBits(ClassIndex.Compact);
    comptime {
        assert(tagAndClassBits == @bitSizeOf(TagAndClassType));
    }
    const tagAndClass = (@as(u64, 1) << tagAndClassBits) - 1;
    pub inline fn untaggedI(self: Object) i64 {
        return @bitCast(self.rawU() & ~tagAndClass);
    }
    // pub inline fn cast(v: anytype) Object {
    //     // stored using little-endian order
    //     return @bitCast(v);
    // }
    inline fn of(comptime v: u64) Object {
        return @bitCast(v);
    }
    pub inline fn thunkImmediate(o: Object) ?Object {
        const value: i64 = @bitCast(o);
        const shifted = value >> 55;
        if (shifted == 0 or shifted == -1)
            return oImm(.ThunkImmediate, @bitCast(@as(i56, @truncate(value))));
        return null;
    }
    pub inline fn isThunkImmediate(self: Self) bool {
        return self.isImmediateClass(.ThunkImmediate);
    }
    pub inline fn thunkImmediateValue(self: Self) Object {
        return @bitCast(@as(i64, @bitCast(self)) >> 8);
    }
    test "ThunkImmediate" {
        std.debug.print("Test: ThunkImmediate\n", .{});
        const ee = std.testing.expectEqual;
        if (thunkImmediate(Object.from(42))) |value|
            try ee(Object.from(42), value.thunkImmediateValue());
        if (thunkImmediate(Object.from(-42))) |value|
            try ee(Object.from(-42), value.thunkImmediateValue());
        try ee(null, thunkImmediate(Object.from(@as(u64, 1) << 47)));
    }
    pub inline fn isImmediateClass(self: Object, class: ClassIndex.Compact) bool {
        return self.tagbits() == oImm(class, 0).tagbits();
    }
    pub inline fn isHeap(self: Object) bool {
        return self.tag == .heap;
    }
    pub inline fn isImmediate(self: Object) bool {
        return self.tag == .immediates;
    }
    pub inline fn isDouble(self: Object) bool {
        return (self.rawU() & 6) != 0;
    }
    inline fn oImm(c: ClassIndex.Compact, h: u56) Self {
        return Self{ .tag = .immediates, .class = c, .hash = h };
    }
    inline fn imm(c: ClassIndex.Compact, h: u56) u64 {
        return @bitCast(oImm(c, h));
    }
    inline fn g(grp: Group) u64 {
        return grp.base();
    }
    pub const ZERO = of(0);
    pub const False = oImm(.False, 0);
    pub const True = oImm(.True, 0);
    pub const Nil = Self{ .tag = .heap, .class = .none, .hash = 0 };
    const NotAnObject = Self{ .tag = .heap, .class = .none, .hash = 0xf0000000000000 }; // never a valid object... should never be visible to managed language
    pub const u64_MINVAL = g(.smallInt);
    const u64_ZERO = g(.smallInt0);
    pub const u64_ZERO2 = u64_ZERO *% 2;
    const u64_MAXVAL = g(.numericThunk) - 1;
    pub const MinSmallInteger = of(u64_MINVAL).to(i64); // anything smaller than this will underflow
    pub const MaxSmallInteger = of(u64_MAXVAL).to(i64); // anything larger than this will overflow
    pub inline fn shiftI(n: i56) i64 {
        return @as(i64, n) << 8;
    }
    pub inline fn isInt(self: Object) bool {
        return self.isImmediateClass(.SmallInteger);
    }
    pub inline fn isNat(self: Object) bool {
        return self.isInt() and self.rawI() >= 0;
    }
    pub inline fn hasPointer(self: Object) bool {
        const bits = math.rotr(TagAndClassType, self.tagbits(), 3);
        return bits <= math.rotr(TagAndClassType, oImm(.ThunkHeap, 0).tagbits(), 3) and bits != 0;
    }
    pub inline fn pointer(self: Object, T: type) ?T {
        switch (self.tag) {
            .heap => return @ptrFromInt(self.rawU()),
            .immediates => switch (self.class) {
                .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnSmallInteger, .ThunkReturnImmediate, .ThunkReturnCharacter, .ThunkReturnFloat, .ThunkHeap, .ThunkLocal, .ThunkInstance, .BlockAssignLocal, .BlockAssignInstance => return @ptrFromInt(self.rawU() >> 16),
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
        if (!self.isSymbol()) @panic("not a Symbol");
        return @bitCast((self.rawU() & 0xffffffffff) | (@as(u64, @intFromEnum(class)) << 40));
    }
    pub inline fn classFromSymbolPlus(self: Object) ClassIndex {
        return @enumFromInt(self.rawU() >> 40);
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
    pub inline fn highAddress(self: Object) *u64 {
        return @ptrFromInt(self.rawU() >> 16);
    }
    pub inline fn numArgs(self: Object) u8 {
        return @truncate(self.hash);
    }
    const nanMemObject = simpleFloat(math.nan(f64), .static);
    const pInfMemObject = simpleFloat(math.inf(f64), .static);
    const nInfMemObject = simpleFloat(-math.inf(f64), .static);
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
                    .One => {
                        return @bitCast(@intFromPtr(value));
                    },
                    else => {},
                }
            },
            else => {},
        }
        @compileError("Can't convert \"" ++ @typeName(T) ++ "\"");
    }
    inline fn which_class(self: Object, comptime full: bool) ClassIndex {
        return switch (self.tag) {
            .heap => if (full) self.to(HeapObjectPtr).*.getClass() else if (self.rawU() == 0) .UndefinedObject else .Object,
            .immediates => self.class.classIndex(),
            else => .Float,
        };
    }
    pub inline fn isString(self: Object) bool {
        return self.which_class(true) == .String;
    }
    pub inline fn isMemoryAllocated(self: Object) bool {
        return if (self.isHeap()) self != Object.Nil else @intFromEnum(self.class) <= @intFromEnum(ClassIndex.Compact.ThunkHeap);
    }
    pub const Special = packed struct {
        imm: TagAndClassType,
        tag: u8,
        rest: u48,
        pub fn ptr(self: Special) *Object {
            return @ptrFromInt(self.rest >> 16);
        }
        pub fn objectFrom(tact: TagAndClassType, tag: u8, p: *opaque {}) Object {
            return @bitCast(Special{ .imm = tact, .tag = tag, .rest = @truncate(@intFromPtr(p)) });
        }
    };
    pub inline fn rawSpecial(self: Object) Special {
        return @bitCast(self);
    }
    pub usingnamespace ObjectFunctions;
};
const ObjectFunctions = struct {
    pub const empty = &[0]Object{};
    pub inline fn rawU(self: Object) u64 {
        return @bitCast(self);
    }
    pub inline fn rawI(self: Object) i64 {
        return @bitCast(self);
    }
    pub inline fn equals(self: Object, other: Object) bool {
        return self.rawU() == other.rawU();
    }
    pub inline fn asCharacter(int: u32) Object {
        return Object.makeImmediate(.Character, int);
    }
    pub inline fn setField(self: Object, field: usize, value: Object) void {
        if (self.asObjectArray()) |ptr| ptr[field] = value;
    }
    pub inline fn getField(self: Object, field: usize) Object {
        if (self.asObjectArray()) |ptr|
            return ptr[field];
        return Nil;
    }
    pub inline fn isSymbol(self: Object) bool {
        return self.tagbits() == comptime Object.makeImmediate(.Symbol, 0).tagbits();
    }
    pub inline fn isBool(self: Object) bool {
        return self == Object.False or self == Object.True;
    }
    pub inline fn isNil(self: Object) bool {
        return self == Object.Nil;
    }
    pub inline fn isImmediate(self: Object) bool {
        return self.tag == .immediates;
    }
    pub inline fn isHeapObject(self: Object) bool {
        return self.tag == .heap;
    }
    pub inline fn isUnmoving(self: Object) bool {
        return !self.isMemoryAllocated() or self.to(HeapObjectPtr).isUnmoving();
    }
    pub inline fn isLiteral(self: Object) bool {
        return !self.isMemoryAllocated();
    }
    pub inline fn hash(self: Object) Object {
        return self.from(self.hash32()) catch unreachable;
    }
    pub inline fn toBool(self: Object) !bool {
        if (self.isBool()) return self.toBoolNoCheck();
        return error.wrongType;
    }
    pub inline fn toInt(self: Object) !i64 {
        if (self.isInt()) return self.toIntNoCheck();
        return error.wrongType;
    }
    pub inline fn toNat(self: Object) !u64 {
        if (self.isNat()) return self.toNatNoCheck();
        return error.wrongType;
    }
    pub inline fn toDouble(self: Object) !u64 {
        if (self.isDouble()) return self.toDoubleNoCheck();
        return error.wrongType;
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
                                        return @as(T, @ptrFromInt(@as(usize, @bitCast(@as(i64, @bitCast(self)) << 16 >> 16))));
                                    } else {
                                        return @as(T, @ptrFromInt(@as(usize, @bitCast(@as(i64, @bitCast(self)) << 16 >> 16)) + @sizeOf(HeapHeader)));
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
    pub fn to(self: Object, comptime T: type) T {
        return self.toWithCheck(T, true);
    }
    pub fn toUnchecked(self: Object, comptime T: type) T {
        return self.toWithCheck(T, false);
    }
    pub inline fn asMemoryObject(self: Object) ?HeapObjectPtr {
        if (self.isMemoryAllocated()) return @ptrFromInt(@as(u48, @truncate(self.rawU())));
        return null;
    }
    pub inline fn asObjectArray(self: Object) ?[*]Object {
        if (self.isHeapObject()) return @ptrCast(self.to(HeapObjectPtr));
        return null;
    }
    pub fn header(self: Object) HeapObject {
        if (self.isHeapObject()) return self.to(HeapObjectPtr).*;
        return @as(HeapObject, @bitCast(@as(u64, 0)));
    }
    pub fn instVars(self: Object) []Object {
        if (self.isHeapObject()) return self.to(HeapObjectPtr).instVars() catch
            return &[0]Object{};
        return &[0]Object{};
    }
    pub fn asZeroTerminatedString(self: Object, target: []u8) ![*:0]u8 {
        const m = try self.arrayAsSlice(u8);
        if (m.len >= target.len) return error.NotEnoughSpace;
        @memcpy(target[0..m.len], m);
        target[m.len] = 0;
        return target[0..m.len :0];
    }
    pub fn arrayAsSlice(self: Object, comptime T: type) ![]T {
        if (self.isIndexable()) return self.to(HeapObjectPtr).arrayAsSlice(T);
        return error.ObjectNotIndexable;
    }
    pub fn size(self: Object) !usize {
        if (!self.isHeapObject()) return error.NotIndexable;
        return self.to(HeapObjectPtr).arraySize();
    }
    pub fn growSizeX(self: Object, stepSize: usize) !usize {
        if (!self.isHeapObject()) return error.NotIndexable;
        return self.to(HeapObjectPtr).growSize(stepSize);
    }
    pub fn isIndexable(self: Object) bool {
        if (self.isHeapObject()) return self.to(HeapObjectConstPtr).isIndexable();
        return false;
    }
    pub fn inHeapSize(self: Object) usize {
        if (self.isHeapObject()) return self.to(HeapObjectPtr).inHeapSize();
        return 0;
    }
    pub fn compare(self: Object, other: Object) std.math.Order {
        const ord = std.math.Order;
        if (self.equals(other)) return ord.eq;
        if (!self.isHeapObject() or !other.isHeapObject()) {
            const u64s = self.rawU();
            const u64o = other.rawU();
            return std.math.order(u64s, u64o);
        }
        const sla = self.arrayAsSlice(u8) catch &[0]u8{};
        const slb = other.arrayAsSlice(u8) catch &[0]u8{};
        for (sla[0..@min(sla.len, slb.len)], 0..) |va, index| {
            const vb = slb[index];
            if (va < vb) return ord.lt;
            if (va > vb) return ord.gt;
        }
        if (sla.len < slb.len) return ord.lt;
        if (sla.len > slb.len) return ord.gt;
        return ord.eq;
    }
    pub inline fn immediate_class(self: Object) ClassIndex {
        return self.which_class(false);
    }
    pub inline fn get_class(self: Object) ClassIndex {
        return self.which_class(true);
    }
    pub inline fn promoteTo(self: Object) !Object {
        if (self.isUnmoving()) return self;
        return error.PromoteUnimplemented;
        //        return arenas.GlobalArena.promote(self);
    }
    pub fn format(
        self: Object,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;
        try switch (self.immediate_class()) {
            .Object => writer.print("object:0x{x:0>16}=>{}", .{ self.rawU(), @as(*heap.HeapHeader, @ptrFromInt(self.rawU())).* }),
            .BlockClosure => writer.print("block:0x{x:>16}", .{self.rawU()}), //,as_pointer(x));
            .False => writer.print("false", .{}),
            .True => writer.print("true", .{}),
            .UndefinedObject => writer.print("nil", .{}),
            .Symbol => {
                try writer.print("#{s}", .{symbol.asString(self).arrayAsSlice(u8) catch "???"});
                if ((self.hash56() >> 32) > 0)
                    try writer.print("=>{}", .{self.classFromSymbolPlus()});
            },
            .Character => writer.print("${c}", .{self.to(u8)}),
            .SmallInteger => writer.print("{d}", .{self.toIntNoCheck()}),
            .Float => writer.print("{}", .{ self.to(f64) }),
            else => {
                try writer.print("{{?0x{x:0>16}}}", .{self.rawU()});
                //@panic("format for unknown class");
            },
        };
        if (fmt.len == 1 and fmt[0] == 'x') try writer.print("(0x{x:0>16})", .{self.rawU()});
    }
    pub const alignment = @alignOf(u64);
    // pub fn packedInt(f1: u14, f2: u14, f3: u14) Object {
    //     return @bitCast(PackedObject.from3(f1,f2,f3));
    // }
};
pub const PackedObject = packed struct {
    tag: Object.TagAndClassType,
    f1: u14,
    f2: u14,
    f3: u14,
    f4: u14,
    pub inline fn from3(f1: u14, f2: u14, f3: u14) PackedObject {
        return .{ .tag = Object.from(0).tagbits(), .f1 = f1, .f2 = f2, .f3 = f3, .f4 = 0 };
    }
    pub inline fn from(o: Object) PackedObject {
        return @bitCast(o);
    }
    fn combine(size: type, tup: anytype) comptime_int {
        comptime var n: u56 = 0;
        comptime var shift = 0;
        inline for (tup) |field| {
            switch (@TypeOf(field)) {
                comptime_int => n |= @as(u56, @as(size, field)) << shift,
                else => n |= @as(u56, @as(size, @intFromEnum(field))) << shift,
            }
            shift += @typeInfo(size).int.bits;
        }
        return n;
    }
    pub fn combine14(tup: anytype) comptime_int {
        return combine(u14, tup);
    }
    pub fn combine14asObject(tup: anytype) Object {
        return Object.from(combine(u14, tup));
    }
    pub fn classes14(tup: anytype) Object {
        return Object.from(combine(u14, tup));
    }
    pub fn combine24(tup: anytype) comptime_int {
        return combine(u24, tup);
    }
    test "combiners" {
        std.debug.print("Test: combiners\n", .{});
        const expectEqual = std.testing.expectEqual;
        try expectEqual(16384 + 2, combine14(.{ 2, 1 }));
        try expectEqual(245774, combine14([_]ClassIndex{ .SmallInteger, .Symbol }));
        try expectEqual(16777216 + 2, combine24(.{ 2, 1 }));
    }
};

test "testing doubles including NaN" {
    switch (config.objectEncoding) {
        .tag => {},
        .nan => {
            // test that all things that generate NaN generate positive ones
            // otherwise we'd need to check in any primitive that could create a NaN
            // because a negative one could look like one of our tags (in particular a large positive SmallInteger)
            const e = std.testing.expect;
            const inf = @as(f64, 1.0) / 0.0;
            const zero = @as(f64, 0);
            const one = @as(f64, 1);
            const fns = struct {
                fn cast(x: anytype) Object {
                    return Object.from(x);
                }
            };
            const cast = fns.cast;
            try e(cast(@sqrt(-one)).isDouble());
            try e(cast(@log(-one)).isDouble());
            try e(cast(zero / zero).isDouble());
            try e(cast((-inf) * 0.0).isDouble());
            try e(cast((-inf) * inf).isDouble());
            try e(cast((-inf) + inf).isDouble());
            try e(cast(inf - inf).isDouble());
            try e(cast(inf * 0.0).isDouble());
            try e(cast(std.math.nan(f64)).isDouble());
        },
    }
}
test "from conversion" {
    const ee = std.testing.expectEqual;
    //    try ee(@as(u64, @bitCast(Object.packedInt(1, 2, 3))), u64_ZERO + 0x000300020001);
    //    try ee(@as(f64, @bitCast((Object.from(3.14)))), 3.14);
    //    try ee((Object.from(42)).u(), u64_ZERO +% 42);
    try ee((Object.from(3.14)).immediate_class(), .Float);
    try std.testing.expect((Object.from(3.14)).isDouble());
    try ee((Object.from(3)).immediate_class(), .SmallInteger);
    try std.testing.expect((Object.from(3)).isInt());
    try std.testing.expect((Object.from(false)).isBool());
    try ee((Object.from(false)).immediate_class(), .False);
    try ee((Object.from(true)).immediate_class(), .True);
    try std.testing.expect((Object.from(true)).isBool());
    try ee((Object.from(null)).immediate_class(), .UndefinedObject);
    try std.testing.expect((Object.from(null)).isNil());
}
test "to conversion" {
    const ee = std.testing.expectEqual;
    try ee((Object.from(3.14)).to(f64), 3.14);
    try ee((Object.from(42)).toInt(), 42);
    try std.testing.expect((Object.from(42)).isInt());
    try ee((Object.from(true)).to(bool), true);
    try ee((Object.from(-0x400000)).toUnchecked(i64), -0x400000);
}
test "immediate_class" {
    const ee = std.testing.expectEqual;
    try ee((Object.from(3.14)).immediate_class(), .Float);
    try ee((Object.from(42)).immediate_class(), .SmallInteger);
    try ee((Object.from(true)).immediate_class(), .True);
    try ee((Object.from(false)).immediate_class(), .False);
    try ee(Nil.immediate_class(), .UndefinedObject);
    try ee(True.immediate_class(), .True);
    try ee(False.immediate_class(), .False);
    try ee(symbol.symbols.yourself.immediate_class(), .Symbol);
}
test "printing" {
    var buf: [255]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const stream = fbs.writer();
    try stream.print("{}\n", .{Object.from(42)});
    try stream.print("{}\n", .{symbol.symbols.yourself});
    try std.testing.expectEqualSlices(u8, "42\n#yourself\n", fbs.getWritten());
}
const Buf = blk: {
    @setRuntimeSafety(false);
    break :blk union {
        buf: [8]u8,
        obj: Object,
    };
};
const buf1: [1]Buf = .{Buf{ .obj = Object.from(42) }};
fn slice1() []const Buf {
    return &buf1;
}
test "order" {
    const ee = std.testing.expectEqual;
    const sl1 = slice1()[0].buf;
    try ee(42, sl1[1]);
    try ee(113, sl1[0]);
    try ee(0, sl1[2]);
    const buf2 = (Buf{
        .obj = Object.from(42.0),
    }).buf;
    try ee(buf2[0], 6);
    try ee(buf2[6], 80);
    try ee(buf2[7], 4);
}
