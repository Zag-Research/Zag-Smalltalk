const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const math = std.math;
const zag = @import("zag.zig");
const config = zag.config;
const assert = std.debug.assert;
const debugError = false;
const Process = zag.Process;
pub const inMemory = @import("object/inMemory.zig");
const symbol = if (debugError) struct {
    const inversePhi24 = @import("utilities.zig").inversePhi(u24);
    const undoPhi24 = @import("utilities.zig").undoPhi(u24);
    pub inline fn fromHash32(hash: u32) Object {
        return Object.makeImmediate(.Symbol, hash);
    }
    inline fn symbol_of(index: u24, arity: u4) Object {
        return fromHash32(@as(u24, index *% inversePhi24) | (@as(u32, arity) << 24));
    }
    pub inline fn symbolArity(obj: Object) u4 {
        return @truncate(obj.hash56() >> 24);
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
pub const False = Object.False;
pub const True = Object.True;
pub const Nil = Object.Nil;
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
    PICPointer,
    ThunkHeap,
    ThunkImmediate,
    ThunkFloat,
    SmallInteger,
    False,
    True,
    Symbol,
    Character,
    LLVM,
    reserved = 31,
    UndefinedObject,
    Context,
    Float,
    ProtoObject,
    Object,
    Array,
    String,
    Utf8String,
    DoubleWordArray,
    BlockClosure,
    Process,
    Class,
    CompiledMethod,
    Dispatch,
    Association,
    Exception,
    Error,
    ContextData,
    SelectorException,
    PrimitiveFailed,
    BlockClosureValue,
    testClass = config.max_classes - 1,
    replace7 = 0xffff - 7,
    replace6,
    replace5,
    replace4,
    replace3,
    replace2,
    replace1,
    replace0,
    _,
    pub const ReplacementIndices = Self.replace7;
    pub const LastSpecial = @intFromEnum(Self.Dispatch);
    const Self = @This();
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
        PICPointer,
        ThunkHeap,
        ThunkImmediate,
        ThunkFloat,
        SmallInteger,
        False,
        True,
        Symbol,
        Character,
        LLVM,
        pub inline fn classIndex(cp: Compact) ClassIndex {
            return @enumFromInt(@intFromEnum(cp));
        }
    };
    pub inline fn compact(ci: ClassIndex) Compact {
        return @enumFromInt(@intFromEnum(ci));
    }
};
comptime {
    std.debug.assert(@intFromEnum(ClassIndex.replace0) == 0xffff);
    std.testing.expectEqual(@intFromEnum(ClassIndex.ThunkReturnLocal), 1) catch unreachable;
    //    std.debug.assert(std.meta.hasUniqueRepresentation(Object));
    for (@typeInfo(ClassIndex.Compact).@"enum".fields, @typeInfo(ClassIndex).@"enum".fields[0..@typeInfo(ClassIndex.Compact).@"enum".fields.len]) |ci, cci| {
        std.testing.expectEqual(ci, cci) catch unreachable;
    }
}
pub const Object = switch (config.objectEncoding) {
    .zag => @import("object/zag.zig").Object,
    .nan => @import("object/nan.zig").Object,
    .spur => @import("object/spur.zig").Object,
    .taggedPtr => @import("object/taggedPtr.zig").Object,
    .cachedPtr, .ptr => @import("object/ptr.zig").Object,
};
pub const ObjectFunctions = struct {
    pub const empty = &[0]Object{};
    pub const Sentinel = Object.from(@as(*Object, @ptrFromInt(8)), null);
    pub inline fn equals(self: Object, other: Object) bool {
        return @as(u64, @bitCast(self)) == @as(u64, @bitCast(other));
    }
    pub inline fn asCharacter(int: u32) Object {
        return Object.makeImmediate(.Character, int);
    }
    pub inline fn numArgs(self: Object) u4 {
        return symbol.symbolArity(self);
    }
    pub inline fn setField(self: Object, field: usize, value: Object) void {
        if (self.asObjectArray()) |ptr| ptr[field] = value;
    }
    pub inline fn getField(self: Object, field: usize) Object {
        if (self.asObjectArray()) |ptr|
            return ptr[field];
        return Nil;
    }
    pub inline fn isNil(self: Object) bool {
        return self == Object.Nil;
    }
    pub inline fn isBool(self: Object) bool {
        return self == Object.False or self == Object.True;
    }
    pub inline fn isString(self: Object) bool {
        return self.which_class(true) == .String;
    }
    pub inline fn isUnmoving(self: Object) bool {
        return !self.isMemoryAllocated() or self.to(HeapObjectPtr).isUnmoving();
    }
    pub inline fn hash(self: Object) Object {
        return self.from(self.hash32()) catch unreachable;
    }
    pub inline fn toBool(self: Object) !bool {
        if (self.isBool()) return self.toBoolNoCheck();
        return error.wrongType;
    }
    pub inline fn toNat(self: Object) !u64 {
        if (self.isNat()) return self.toNatNoCheck();
        return error.wrongType;
    }
    pub inline fn toDouble(self: Object) !f64 {
        if (self.isDouble()) return self.toDoubleNoCheck();
        return error.wrongType;
    }

    pub fn to(self: Object, comptime T: type) T {
        return self.toWithCheck(T, true);
    }
    pub fn toUnchecked(self: Object, comptime T: type) T {
        return self.toWithCheck(T, false);
    }
    pub inline fn asMemoryObject(self: Object) ?HeapObjectPtr {
        if (self.isMemoryAllocated()) return self.pointer(HeapObjectPtr);
        return null;
    }
    pub inline fn asObjectArray(self: Object) ?[*]Object {
        if (self.isHeapObject()) return self.pointer([*]Object);
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
            unreachable;
            // const u64s = self.rawU();
            // const u64o = other.rawU();
            // return std.math.order(u64s, u64o);
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
    pub //inline
        fn immediate_class(self: Object) ClassIndex {
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
        if (self.nativeI()) |i| {
            try writer.print("{d}", .{i});
        } else if (self.equals(False)) {
            try writer.print("false", .{});
        } else if (self.equals(True)) {
            try writer.print("true", .{});
        } else if (self.symbolHash()) |_| {
            try writer.print("#{s}", .{symbol.asString(self).arrayAsSlice(u8) catch "???"});
            // if (self.classFromSymbolPlus()) |c|
            //     try writer.print("=>{}", .{c});
        } else if (self == Nil) {
            try writer.print("nil", .{});
        } else {
            try writer.print("{{?0x{x:0>16}}}", .{@as(u64, @bitCast(self))});
        }
        if (fmt.len == 1 and fmt[0] == 'x') try writer.print("(0x{x:0>16})", .{@as(u64, @bitCast(self))});
    }
    pub const alignment = @alignOf(u64);
    pub const intFromPackedObject = PackedObject.intFromPackedObject;
};
pub const PackedObject = packed struct {
    tag: Object.PackedTagType = Object.PackedTagSmallInteger,
    f1: u14,
    f2: u14 = 0,
    f3: u14 = 0,
    f4: std.meta.Int(.unsigned, 64 - 42 - @bitSizeOf(Object.PackedTagType)) = 0,
    pub inline fn from3(f1: u14, f2: u14, f3: u14) PackedObject {
        return .{ .tag = Object.from(0).tagbits(), .f1 = f1, .f2 = f2, .f3 = f3 };
    }
    fn intFromPackedObject(self: Object) u64 {
        return @as(u64, @bitCast(self)) >> @bitSizeOf(Object.PackedTagType);
    }
    fn combine(size: type, tup: anytype) comptime_int {
        comptime var n: u56 = 0;
        comptime var shift = 0;
        inline for (tup) |field| {
            n |= @as(u56, switch (@TypeOf(field)) {
                comptime_int => @as(size, field),
                else => @intFromEnum(field),
            }) << shift;
            shift += @typeInfo(size).int.bits;
        }
        return n;
    }
    pub fn combine14(tup: anytype) comptime_int {
        return combine(u14, tup);
    }
    pub fn object14(tup: anytype) Object {
        return @bitCast((@as(u64, combine(u14, tup)) << @bitSizeOf(Object.PackedTagType)) + Object.PackedTagSmallInteger);
    }
    test "combiners" {
        std.debug.print("Test: combiners\n", .{});
        const expectEqual = std.testing.expectEqual;
        try expectEqual(16384 + 2, combine14(.{ 2, 1 }));
        try expectEqual(294927, combine14([_]ClassIndex{ .SmallInteger, .Symbol }));
    }
};

test "from conversion" {
    const ee = std.testing.expectEqual;
    var process: Process align(Process.alignment) = Process.new();
    process.init(Nil);
    const p = &process;
    //    try ee(@as(f64, @bitCast((Object.from(3.14)))), 3.14);
    try ee((Object.from(3.14, p)).immediate_class(), .Float);
    try std.testing.expect((Object.from(3.14, p)).isDouble());
    try ee((Object.from(3, null)).immediate_class(), .SmallInteger);
    try std.testing.expect((Object.from(3, null)).isInt());
    try std.testing.expect((Object.from(false, null)).isBool());
    try ee((Object.from(false, null)).immediate_class(), .False);
    try ee((Object.from(true, null)).immediate_class(), .True);
    try std.testing.expect((Object.from(true, null)).isBool());
    try ee((Object.from(null, null)).immediate_class(), .UndefinedObject);
    try std.testing.expect((Object.from(null, null)).isNil());
}
test "to conversion" {
    var process: Process align(Process.alignment) = Process.new();
    process.init(Nil);
    const p = &process;
    const ee = std.testing.expectEqual;
    try ee((Object.from(3.14, p)).to(f64), 3.14);
    try ee((Object.from(42, p)).to(u64), 42);
    try std.testing.expect((Object.from(42, p)).isInt());
    try ee((Object.from(true, p)).to(bool), true);
    try ee((Object.from(-0x400000, p)).toUnchecked(i64), -0x400000);
}
test "immediate_class" {
    const ee = std.testing.expectEqual;
    try ee((Object.from(3.14, null)).immediate_class(), .Float);
    try ee((Object.from(42, null)).immediate_class(), .SmallInteger);
    try ee((Object.from(true, null)).immediate_class(), .True);
    try ee((Object.from(false, null)).immediate_class(), .False);
    try ee(Nil.immediate_class(), .UndefinedObject);
    try ee(True.immediate_class(), .True);
    try ee(False.immediate_class(), .False);
    try ee(symbol.symbols.yourself.immediate_class(), .Symbol);
}
test "printing" {
    var buf: [255]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const stream = fbs.writer();
    try stream.print("{}\n", .{Object.from(42, null)});
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
const buf1: [1]Buf = .{Buf{ .obj = Object.from(42, null) }};
fn slice1() []const Buf {
    return &buf1;
}
test "order" {
    switch (config.objectEncoding) {
        .zag => {
            const ee = std.testing.expectEqual;
            const sl1 = slice1()[0].buf;
            try ee(42, sl1[1]);
            try ee(121, sl1[0]);
            try ee(0, sl1[2]);
            @setRuntimeSafety(false);
            const buf2 = (Buf{
                .obj = Object.from(42.0, null),
            }).buf;
            try ee(buf2[0], 6);
            try ee(buf2[6], 80);
            try ee(buf2[7], 4);
        },
        else => {},
    }
}
