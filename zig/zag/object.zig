//! Object encoding and representation system.
//!
//! This module provides the compile-time-selected `Object` type and supporting
//! infrastructure shared across all encoding schemes.
//!
//! ## Object Encodings
//!
//! The active encoding is chosen at compile time via `config.objectEncoding`:
//! - `Object.zag`
//! - `Object.nan`
//! - `object.spur`
//! - `objectj.zagSpur`
//! - `object.taggedInt` / `taggedPtr` / `ptr`
//! - `object.onlyInt` / `object.onlyFloat`

const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const math = std.math;
const zag = @import("zag.zig");
const config = zag.config;
const trace = config.trace;
const assert = std.debug.assert;
const Process = zag.Process;
const hash64 = zag.utilities.ProspectorHash.hash64;
const symbol = @import("symbol.zig");
const heap = @import("heap.zig");
const Age = heap.Age;
const HeapObject = heap.HeapObject;
const HeapHeader = heap.HeapHeader;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
const largerPowerOf2 = @import("utilities.zig").largerPowerOf2;
//pub const SelfObject = if (!builtin.is_test) struct {} else Object.oImm(.Symbol, 0xf0000ff);
pub const False = Object.False;
pub const True = Object.True;
pub const Nil = Object.Nil;
pub fn fromLE(comptime T: type, v: T) Object {
    const val = @as(*const [@sizeOf(T)]u8, @ptrCast(&v));
    return @bitCast(mem.readIntLittle(T, val));
}
pub const compareObject = Object.compare;
const siIndex = 21;
const noneIndex = switch (config.objectEncoding) {
    .taggedPtr, .taggedHigh => siIndex,
    else => 0,
};
pub const ClassIndex = enum(u16) {
    none = noneIndex,
    SmallInteger = noneIndex ^ siIndex,
    ThunkReturnLocal = 1,
    ThunkReturnInstance,
    ThunkReturnObject,
    ThunkReturnImmediate,
    ThunkLocal,
    BlockAssignLocal,
    ThunkInstance,
    BlockAssignInstance,
    ThunkHeap,
    ThunkImmediate,
    Symbol,
    False,
    True,
    Character,
    Signature,
    ThunkReturnCharacter,
    ThunkReturnFloat,
    ThunkFloat,
    LLVM,
    UndefinedObject,
    reserved_22 = 22,
    reserved_23,
    reserved_24,
    reserved_25,
    reserved_26,
    o4,
    o3,
    o2,
    o1,
    o0,
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
    LLVMPrimitives,
    LLVMGenerator,
    leaveObjectOnStack = 0x3fff,
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
        none = noneIndex,
        SmallInteger = noneIndex ^ siIndex,
        ThunkReturnLocal = 1,
        ThunkReturnInstance,
        ThunkReturnObject,
        ThunkReturnImmediate,
        ThunkLocal,
        BlockAssignLocal,
        ThunkInstance,
        BlockAssignInstance,
        ThunkHeap,
        ThunkImmediate,
        Symbol,
        False,
        True,
        Character,
        Signature,
        ThunkReturnCharacter,
        ThunkReturnFloat,
        ThunkFloat,
        LLVM,
        UndefinedObject,
        reserved_22 = 22,
        reserved_23,
        reserved_24,
        reserved_25,
        reserved_26,
        o4,
        o3,
        o2,
        o1,
        o0,
        pub inline fn classIndex(cp: Compact) ClassIndex {
            return @enumFromInt(@intFromEnum(cp));
        }
        pub fn tag(comptime self: Compact) u8 {
            return @as(u8, @intFromEnum(self)) << 3 | 1;
        }
    };
    pub fn isImmediate(self: ClassIndex) bool {
        switch (self) {
            .Symbol, .False, .True, .Character => return true,
            else => return false,
        }
    }
    pub inline fn compact(ci: ClassIndex) Compact {
        return @enumFromInt(@intFromEnum(ci));
    }
    pub inline fn u(ci: ClassIndex) u16 {
        return @intFromEnum(ci);
    }
    pub inline fn classIndexFromInt(int: u5) ClassIndex {
        return @enumFromInt(int);
    }
    pub const lookupMethodForClass = zag.dispatch.lookupMethodForClass;
};
comptime {
    std.debug.assert(@intFromEnum(ClassIndex.Compact.UndefinedObject) == 20);
    std.debug.assert(@intFromEnum(ClassIndex.UndefinedObject) == 20);
    std.debug.assert(@intFromEnum(ClassIndex.replace0) == 0xffff);
    std.debug.assert(@intFromEnum(ClassIndex.Compact.o0) == 0x1f);
    std.debug.assert(@intFromEnum(ClassIndex.o0) == 0x1f);
    std.testing.expectEqual(@intFromEnum(ClassIndex.ThunkReturnLocal), 1) catch @panic("unreachable");
    //    std.debug.assert(std.meta.hasUniqueRepresentation(Object));
    for (@typeInfo(ClassIndex.Compact).@"enum".fields, @typeInfo(ClassIndex).@"enum".fields[0..@typeInfo(ClassIndex.Compact).@"enum".fields.len]) |ci, cci| {
        std.testing.expectEqual(ci, cci) catch @panic("unreachable");
    }
}
pub const Object = zag.encoding.module(config.objectEncoding).Object;
pub const testObjects = blk: {
    var testArray: [5]Object = undefined;
    for (&testArray, 0..) |*elem, i| {
        elem.* = @bitCast(7777777777777777 << 8 | 1 | (@intFromEnum(ClassIndex.o0) - i) << 3);
    }
    break :blk testArray;
};
// const ints = blk: {
//     var osArray: [4]Object.StaticObject = undefined;
//     var objs: [4]Object = undefined;
//     for (0..4) |i| {
//         objs[i] = Object.initStaticObject(i, &osArray[i]);
//     }
//     break :blk .{ objs, osArray };
//     };
// pub const zero = ints[0][0];
// pub const one = ints[0][1];
// pub const two = ints[0][2];
// pub const three = ints[0][3];

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
        return @intCast(self.hash32() >> 24);
    }
    pub inline //
    fn setField(self: Object, field: usize, value: Object) void {
        if (self.asObjectArray()) |ptr| ptr[field] = value;
    }
    pub inline //
    fn getField(self: Object, field: usize) Object {
        if (self.asObjectArray()) |ptr|
            return ptr[field];
        return Nil();
    }
    pub inline //
    fn isNil(self: Object) bool {
        return self.equals(Nil());
    }
    pub inline //
    fn isBool(self: Object) bool {
        return self.equals(Object.False()) or self.equals(Object.True());
    }
    pub inline //
    fn isString(self: Object) bool {
        return self.which_class() == .String;
    }
    pub inline //
    fn isUnmoving(self: Object) bool {
        return !self.hasMemoryReference() or self.to(*HeapObject).isUnmoving();
    }
    pub inline //
    fn hash(self: Object) Object {
        return self.from(self.hash32()) catch @panic("unreachable");
    }
    pub inline //
    fn toBool(self: Object) !bool {
        if (self.isBool()) return self.toBoolNoCheck();
        return error.wrongType;
    }
    pub inline fn toBoolNoCheck(self: Object) bool {
        return self.equals(Object.True());
    }
    pub inline //
    fn toNat(self: Object) !u64 {
        if (self.isNat()) return self.toNatNoCheck();
        return error.wrongType;
    }
    pub inline //
    fn toDouble(self: Object) !f64 {
        if (self.nativeF()) |flt| return flt;
        return error.wrongType;
    }
    pub fn rawFromU(u: u64) Object {
        return @bitCast(u);
    }
    pub fn to(self: Object, comptime T: type) T {
        return self.toWithCheck(T, true);
    }
    pub fn toUnchecked(self: Object, comptime T: type) T {
        return self.toWithCheck(T, false);
    }
    pub inline fn asObjectArray(self: Object) ?[*]Object {
        if (self.ifHeapObject()) |ptr| return @ptrCast(ptr);
        return null;
    }
    pub fn header(self: Object) HeapHeader {
        if (self.ifHeapObject()) |ptr| return ptr.header;
        return @as(HeapHeader, @bitCast(@as(u64, 0)));
    }
    pub fn instVars(self: Object) []Object {
        if (self.ifHeapObject()) |ptr| return ptr.instVars();
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
        if (self.isIndexable()) return self.to(*HeapObject).arrayAsSlice(T);
        return error.ObjectNotIndexable;
    }
    pub fn size(self: Object) !usize {
        if (self.ifHeapObject()) |ptr| return ptr.arraySize();
        return error.NotIndexable;
    }
    pub fn isIndexable(self: Object) bool {
        if (self.ifHeapObject()) |ptr| return ptr.isIndexable();
        return false;
    }
    pub fn inHeapSize(self: Object) usize {
        if (self.ifHeapObject()) |ptr| return ptr.inHeapSize();
        return 0;
    }
    pub fn compare(self: Object, other: Object) std.math.Order {
        const ord = std.math.Order;
        if (self.equals(other)) return ord.eq;
        if (self.ifHeapObject()) |_| {
            if (other.ifHeapObject()) |_| {
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
        }
        @panic("unreachable");
    }
    pub inline //
    fn get_class(self: Object) ClassIndex {
        return self.which_class();
    }
    pub inline fn promoteToUnmovable(self: Object) !Object {
        if (self.isUnmoving()) return self;
        return error.PromoteUnimplemented;
    }
    fn checkThreadedFn(self: u64) ?@import("threadedFn.zig").Enum {
        if (self == 0 or self & 7 != 0) return null;
        return @import("threadedFn.zig").find(@ptrFromInt(self));
    }
    pub fn format(
        self: Object,
        writer: anytype,
    ) !void {
        if (false) {
            try writer.print("({x})", .{@as(u64, @bitCast(self))});
            //return;
        }
        if (zag.config.is_test) {
            for (0..testObjects.len) |i| {
                if (testObjects[i].equals(self)) {
                    try writer.print("testObject[{}]", .{i});
                    return;
                }
            }
        }
        if (checkThreadedFn(@bitCast(self))) |name| {
            try writer.print("{}", .{name});
        } else if (self.invalidObject()) |invalid| {
            try writer.print("{{?0x{x:0>16}}}", .{invalid});
        } else if (self.signature()) |signature| {
            try writer.print("{f}", .{signature});
        } else if (self.nativeI()) |i| {
            try writer.print("{d}", .{i});
        } else if (self.symbolHash()) |_| {
            try writer.print("#{s}", .{symbol.asString(self).arrayAsSlice(u8) catch "???"});
        } else if (self.extraImmediateU()) |extra| {
            try writer.print("{}({}) -> {*}", .{ self.which_class(), extra, self.highPointer(*zag.Context) });
        } else if (self.extraImmediateI()) |extra| {
            try writer.print("{}({}) -> {*}", .{ self.which_class(), extra, self.highPointer(*zag.Context) });
        } else if (self.equals(False())) {
            try writer.print("false", .{});
        } else if (self.equals(True())) {
            try writer.print("true", .{});
        } else if (zag.config.show_trace and @as(u64, @bitCast(self)) == 0xaaaaaaaaaaaaaaaa) {
            try writer.print("undefined", .{});
        } else if (self.nativeF()) |float| {
            try writer.print("{}", .{float});
        } else if (self.equals(Nil())) {
            try writer.print("nil", .{});
        } else if (self.ifHeapObject()) |obj| {
            try writer.print("{f}@{x}", .{ obj, @as(u64, @bitCast(self)) });
        } else {
            try writer.print("{{?0x{x:0>16}}}", .{@as(u64, @bitCast(self))});
        }
    }
    pub const alignment = @alignOf(u64);
};
pub const PackedObject = packed struct {
    tag: Object.PackedTagType = Object.packedTagSmallInteger,
    f1: u14,
    f2: u14 = 0,
    f3: u14 = 0,
    f4: std.meta.Int(.unsigned, 64 - 42 - @bitSizeOf(Object.PackedTagType)) = 0,
    pub fn asU64(self: PackedObject) u64 {
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
    pub fn combine14(comptime tup: anytype) comptime_int {
        return combine(u14, tup);
    }
    pub fn classes(comptime tup: []const ClassIndex) PackedObject {
        var result: PackedObject = @bitCast((@as(u64, combine(u14, tup)) << @bitSizeOf(Object.PackedTagType)));
        result.tag = Object.packedTagSmallInteger;
        return result;
    }
    test "combiners" {
        const expectEqual = std.testing.expectEqual;
        try expectEqual(16384 + 2, combine14(.{ 2, 1 }));
        try expectEqual(0x2C015, combine14([_]ClassIndex{ .SmallInteger, .Symbol }));
    }
};
test {_ = tests;}
const tests = struct {
    var process: Process align(Process.alignment) = undefined;
    var sp: Process.SP = undefined;
    var context: *zag.Context = undefined;
    const ee = std.testing.expectEqual;
    const expect = std.testing.expect;
    const iTest = i40;
    fn init() void {
        process.init();
        sp = process.getSp();
        context = process.getContext();
    }
    fn isInt(obj: Object) bool {
        if (obj.nativeI()) |_| return true;
        return false;
    }
    fn nativeI(obj: Object) !i64 {
        if (obj.nativeI()) |int| return int;
        return error.NotInt;
    }
    fn nativeF(obj: Object) !f64 {
        if (obj.nativeF()) |flt| return flt;
        return error.NotFloat;
    }
    fn untaggedI(obj: Object) !iTest {
        if (obj.untaggedI()) |int| return @truncate(int);
        return error.NotInt;
    }
    fn taggedI(obj: Object) !iTest {
        if (obj.taggedI()) |int| return @truncate(int);
        return error.NotInt;
    }
    fn fromTaggedI(i: iTest) Object {
        return Object.fromTaggedI(i, sp, context);
    }
    fn fromUntaggedI(i: iTest) Object {
        return Object.fromUntaggedI(i, sp, context);
    }
    fn fromNativeI(i: iTest) Object {
        return Object.fromNativeI(i, sp, context);
    }
    inline fn from(x: anytype) Object {
        return Object.from(x, sp, context);
    }
    test "encoding: back and forth" {
        init();
        if (config.objectEncoding == .nan)
            try ee(3.14, @as(f64, @bitCast((from(3.14)))));
        try expect(!std.math.isNan(@as(f64, @bitCast(from(3.14)))));
        try expect(isInt(from(3)));
        try ee(3.14, (from(3.14)).to(f64));
        try ee(42, (from(42)).to(i64));
        try expect(isInt(from(42)));
        try ee(true, (from(true)).to(bool));
        try ee(-0x400000, (from(-0x400000)).toUnchecked(i64));
        for (0..200) |u| {
            const i:iTest = @truncate(@as(i64,@bitCast(u)) - 100);
            try ee(i, nativeI(fromNativeI(i)));
            try ee(i, nativeI(fromUntaggedI(try untaggedI(from(i)))));
        }
    }
    test "encoding: get_class" {
        //try config.skipNotZag();
        init();
        try expect((from(3.14)).isFloat());
        try expect((from(true)).isBool());
        try expect((from(false)).isBool());
        try expect((from(null)).isNil());
        try ee(.Float, (from(3.14)).get_class());
        try ee(.SmallInteger, (from(42)).get_class());
        try ee(.True, (from(true)).get_class());
        try ee(.False, (from(false)).get_class());
        try ee(.UndefinedObject, (from(null)).get_class());
        try ee(.Symbol, symbol.Symbols.yourself.get_class());
        try ee(.True, True().get_class());
        try ee(.False, False().get_class());
        try ee(.UndefinedObject, Nil().get_class());
    }
    test "printing" {
        init();
        var buf: [80]u8 = undefined;
        var fbs = std.io.fixedBufferStream(&buf);
        const stream = fbs.writer();
        try stream.print("{f}\n", .{from(42)});
        try stream.print("{f}\n", .{symbol.Symbols.yourself.asObject()});
        try std.testing.expectEqualSlices(u8, "42\n#yourself\n", fbs.getWritten());
    }
    test "order" {
        init();
        switch (config.objectEncoding) {
            .zag => {
                @setRuntimeSafety(false);
                const obj1 = from(42);
                const buf1 = @as([*]u8, @ptrFromInt(@intFromPtr(&obj1)))[0..8];
                try ee(@intFromEnum(ClassIndex.SmallInteger) << 3 | 1, buf1[0]);
                try ee(42, buf1[1]);
                try ee(0, buf1[2]);
                const obj2 = from(42.0);
                const buf2 = @as([*]u8, @ptrFromInt(@intFromPtr(&obj2)))[0..8];
                try ee(6, buf2[0]);
                try ee(80, buf2[6]);
                try ee(4, buf2[7]);
            },
            else => {},
        }
    }
};
