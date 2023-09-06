const std = @import("std");
const mem = std.mem;
const builtin = @import("builtin");
const native_endian = builtin.target.cpu.arch.endian();
const dispatchCache = @import("config.zig").dispatchCache;
const symbol = @import("symbol.zig");
const heap = @import("heap.zig");
const HeapObject = heap.HeapObject;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
const largerPowerOf2 = @import("utilities.zig").largerPowerOf2;
inline fn of(comptime v: u64) Object {
    return @as(Object, @bitCast(v));
}
pub fn oImm(c: ClassIndex, h: u32) u64 { // INLINED & NON-PUB
    return o(.immediates) | @as(u64, @intFromEnum(c)) << 32 | h;
}
inline fn o(g: Group) u64 {
    return g.base();
}
pub fn indexSymbol(uniqueNumber: u12) Object { // INLINED
    return @as(Object, @bitCast(oImm(.Symbol, 0xff000000 | @as(u32, uniqueNumber))));
}
pub const ZERO = of(0);
const Negative_Infinity: u64 = o(.immediates); //0xfff0000000000000;
const Start_of_Blocks: u64 = o(.numericThunk);
const Start_of_Pointer_Objects: u64 = o(.heapThunk); // things that have low 48 bits is an object pointer
const Start_of_Heap_Objects: u64 = o(.heap);
pub const False = of(oImm(.False, 0x0));
pub const True = of(oImm(.True, 0x1));
pub const Nil = of(oImm(.UndefinedObject, 0xffffffff));
pub const NotAnObject = of(oImm(.UndefinedObject, 0x3)); // never a valid object... should never be visible to managed language
const Symbol_Base = oImm(.Symbol, 0);
const Character_Base = oImm(.Character, 0);
pub const u64_MINVAL = o(.smallInt);
const u64_ZERO = o(.smallInt0);
pub const u64_ZERO2 = u64_ZERO *% 2;
const u64_MAXVAL = o(.unused1) - 1;
pub const MinSmallInteger = of(u64_MINVAL).to(i64); // anything smaller than this will underflow
pub const MaxSmallInteger = of(u64_MAXVAL).to(i64); // anything larger than this will overflow
pub const invalidHeapPointer = of(Start_of_Heap_Objects);

pub fn fromLE(comptime T: type, v: T) Object {
    const val = @as(*const [@sizeOf(T)]u8, @ptrCast(&v));
    return of(mem.readIntLittle(T, val));
}
pub const compareObject = Object.compare;
pub const ClassIndex = enum(u16) {
    none = 0,
    Object,
    SmallInteger,
    UndefinedObject,
    False,
    True,
    Float,
    Symbol,
    Character,
    BlockClosure,
    Array,
    String,
    SymbolTable,
    Method,
    CompiledMethod,
    Dispatch,
    ContextData,
    Context,
    BlockFailure,
    max = 0xffff,
    _,
    const Self = @This();
    inline fn base(ci: Self) u64 {
        return @as(u64, @intFromEnum(ci)) << 32;
    }
    inline fn immediate(cg: Self) u64 {
        return ((@as(u64, @intFromEnum(Group.immediates)) << 16) | @as(u64,@intFromEnum(cg))) << 32;
    }
};
pub const Group = enum(u16) {
    immediates = 0xfff0,
    smallInt,
    smallInt0 = 0xfff5,
    smallIntMax = 0xfff8,
    unused1,
    numericThunk,
    immediateThunk,
    heapThunk,
    nonLocalThunk,
    heapClosure,
    heap,
    _,
    const Self = @This();
    fn base(cg: Self) u64 { // INLINED
        return @as(u64, @intFromEnum(cg)) << 48;
    }
    inline fn u(cg: Self) u16 {
        return @intFromEnum(cg);
    }
};
pub const Object = packed struct(u64) {
    h0: u16, // align(8),
    h1: u16,
    classIndex: ClassIndex,
    tag: Group,
    pub const empty = &[0]Object{};
    pub inline fn makeImmediate(cls: ClassIndex, low32: u32) Object {
        return cast(cls.immediate() | low32);
    }
    pub inline fn withImmClass(self: Object, cls: ClassIndex) Object {
        return cast(self.hash32() | cls.immediate());
    }
    pub inline fn withClass(self: Object, cls: ClassIndex) Object {
        if (dispatchCache) {
            return cast(@as(u64,@intFromEnum(cls))<<32 | self.hash32());
        } else
            return self;
    }
    pub inline fn withOffsetx(self: Object, offset: u32) Object {
        return cast(@as(u64,offset)<<32 | self.hash32());
    }
    pub inline fn asSymbol(self: Object) Object {
        return makeImmediate(.Symbol, self.hash32());
    }
    pub inline fn makeGroup(grp: Group, low48: u48) Object {
        return cast(grp.base() | low48);
    }
    pub inline fn low16(self: Object) u16 {
        return self.h0;
    }
    pub inline fn mid16(self: Object) u16 {
        return self.h1;
    }
    pub inline fn high16(self: Object) u16 {
        return @intFromEnum(self.classIndex);
    }
    pub inline fn cast(v: anytype) Object {
        // stored using little-endian order
        return @as(Object, @bitCast(v));
    }
    pub inline fn hash(self: Object) Object {
        return cast(self.u() | u64_ZERO);
    }
    pub inline fn hash24(self: Object) u24 {
        return @as(u24, @truncate(self.u()));
    }
    pub inline fn hash32(self: Object) u32 {
        return @as(u32, @truncate(self.u()));
    }
    pub inline fn numArgs(self: Object) u4 {
        return @as(u4, @truncate(self.u() >> 24));
    }
    pub inline fn u(self: Object) u64 {
        return @as(u64, @bitCast(self));
    }
    pub inline fn i(self: Object) i64 {
        return @as(i64, @bitCast(self));
    }
    pub inline fn tagged(tag: Group, low: u3, addr: u64) Object {
        return cast((Object{ .tag = tag, .classIndex = .none, .h1 = 0, .h0 = low }).u() + addr);
    }
    pub inline fn tagbits(self: Object) u16 {
        return @intFromEnum(self.tag);
    }
    pub inline fn tagbitsL(self: Object) u32 {
        return @as(u32, @truncate(self.u() >> 32));
    }
    pub inline fn equals(self: Object, other: Object) bool {
        return self.u() == other.u();
    }
    pub inline fn hashEquals(self: Object, other: Object) bool {
        //@truncate(u24,self.u()^other.u())==0;
        return self.hash32() == other.hash32();
    }
    pub inline fn selectorEquals(self: Object, other: Object) bool { // may be false positive
//        return (self.u()^other.u())&0xffffffffffff == 0;
        return self.u()==other.u();
    }
    pub inline fn indexEquals(self: Object, other: Object) bool { // may be false positive
        return self.equals(other.withImmClass(.Symbol));
    }
    pub inline fn indexNumber(self: Object) u12 {
        return @truncate(self.u() >> 12);
    }
    pub inline fn isInt(self: Object) bool {
        return self.atLeastInt() and self.atMostInt();
    }
    pub fn atLeastInt(self: Object) bool { // INLINED
        var testing = Group.smallInt.u();
        return self.tag.u() >= testing;
    }
    pub inline fn atMostInt(self: Object) bool {
        return self.tag.u() <= Group.smallIntMax.u();
    }
    pub inline fn isNat(self: Object) bool {
        return self.atLeastZero() and self.atMostInt();
    }
    pub inline fn atLeastZero(self: Object) bool {
        return self.tag.u() >= Group.smallInt0.u();
    }
    pub inline fn isDouble(self: Object) bool {
        return self.u() <= Negative_Infinity;
    }
    pub inline fn isBool(self: Object) bool {
        const tag = self.tagbitsL();
        return tag == False.tagbitsL() or tag == True.tagbitsL();
    }
    pub inline fn isNil(self: Object) bool {
        return self.tagbitsL() == Nil.tagbitsL();
    }
    pub inline fn isImmediate(self: Object) bool {
        return self.tag == Group.immediates;
    }
    pub inline fn isHeapObject(self: Object) bool {
        return self.tag == Group.heap;
    }
    pub inline fn isHeapAllocated(self: Object) bool {
        const tag = self.tagbits();
        return tag >= Start_of_Pointer_Objects >> 48;
    }
    pub inline fn isUnmoving(self: Object) bool {
        return !self.isHeapAllocated() or self.to(HeapObjectPtr).isUnmoving();
    }
    pub inline fn isLiteral(self: Object) bool {
        return !self.isHeapAllocated();
    }
    pub inline fn isBlock(self: Object) bool {
        const tag = self.tagbits();
        return tag >= Start_of_Blocks >> 48 and !self.isHeapObject();
    }
    pub inline fn isIndexSymbol(self: Object) bool {
        return (self.u() >> 24) == (comptime indexSymbol(0).u() >> 24);
    }
    pub inline fn isSymbol(self: Object) bool {
        return self.tagbitsL() == comptime indexSymbol(0).tagbitsL();
    }
    pub inline fn toBool(self: Object) bool {
        if (self.isBool()) return @as(u1, @truncate(self.u())) == 1;
        @panic("Trying to convert Object to bool");
    }
    pub inline fn toInt(self: Object) i64 {
        if (self.isInt()) return @as(i64, @bitCast(self.u() -% u64_ZERO));
        return -42;//        @panic("Trying to convert Object to i64");
    }
    pub inline fn toNat(self: Object) u64 {
        if (self.isNat()) return self.u() -% u64_ZERO;
        @panic("Trying to convert Object to u64");
    }
    pub inline fn rawWordAddress(self: Object) u64 {
        return self.u() & 0xffff_ffff_fff8;
    }
    pub fn toWithCheck(self: Object, comptime T: type, comptime check: bool) T {
        switch (T) {
            f64 => {
                if (check and self.isDouble()) return @as(f64, @bitCast(self));
            },
            i64 => {
                if (check and self.isInt()) return self.toInt();
            },
            u64 => {
                if (check and self.isNat()) return self.toNat();
            },
            bool => {
                if (check and self.isBool()) return self.equals(True);
            },
            //u8  => {return @intCast(u8, self.hash & 0xff);},
            else => {
                switch (@typeInfo(T)) {
                    .Pointer => |ptrInfo| {
                        if (!check or (self.isHeapAllocated() and (!@hasDecl(ptrInfo.child, "ClassIndex") or self.to(HeapObjectConstPtr).classIndex == ptrInfo.child.ClassIndex))) {
                            if (@hasField(ptrInfo.child, "header") or (@hasDecl(ptrInfo.child, "includesHeader") and ptrInfo.child.includesHeader)) {
                                return @as(T, @ptrFromInt(@as(usize, @bitCast(@as(i64, @bitCast(self)) << 16 >> 16))));
                            } else {
                                unreachable; //return @ptrFromInt(T, @bitCast(usize, @bitCast(i64, self) << 16 >> 16)+@sizeOf(HeapObject));
                            }
                        }
                        @panic("Trying to convert Object pointer to " ++ @typeName(T));
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
        if (T == i64) return @as(i64, @bitCast(self.u() -% u64_ZERO));
        if (T == u64) return self.u() - u64_ZERO;
        return self.toWithCheck(T, false);
    }
    pub fn header(self: Object) HeapObject {
        if (self.isHeapObject()) return self.to(HeapObjectPtr).*;
        return @as(HeapObject, @bitCast(@as(u64, 0)));
    }
    pub fn instVars(self: Object) []Object {
        if (self.isHeapObject()) return self.to(HeapObjectPtr).instVars();
        return &[0]Object{};
    }
    pub fn arrayAsSlice(self: Object, comptime T: type) []T {
        if (self.isIndexable()) return self.to(HeapObjectPtr).arrayAsSlice(T) catch return &[0]T{};
        return &[0]T{};
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
        if (self.isHeapObject()) return self.to(HeapObjectPtr).isIndexable();
        return false;
    }
    pub fn inHeapSize(self: Object) usize {
        if (self.isHeapObject()) return self.to(HeapObjectPtr).inHeapSize();
        return 0;
    }
    pub fn from(value: anytype) Object { // INLINED
        const T = @TypeOf(value);
        if (T == Object) return value;
        switch (@typeInfo(@TypeOf(value))) {
            .Int, .ComptimeInt => return cast(@as(u64, @bitCast(@as(i64, value))) +% u64_ZERO),
            .Float, .ComptimeFloat => return cast(@as(f64, value)),
            .Bool => return if (value) True else False,
            .Null => return Nil,
            .Pointer => |ptr_info| {
                switch (ptr_info.size) {
                    .One => {
                        return cast(@as(u48, @truncate(@intFromPtr(value))) + Start_of_Heap_Objects);
                    },
                    else => {},
                }
            },
            else => {},
        }
        @compileError("Can't convert \"" ++ @typeName(@TypeOf(value)) ++ "\"");
    }
    pub fn compare(self: Object, other: Object) std.math.Order {
        if (!self.isHeapObject() or !other.isHeapObject()) {
            const u64s = self.u();
            const u64o = other.u();
            return std.math.order(u64s, u64o);
        }
        const ord = std.math.Order;
        if (self.equals(other)) return ord.eq;
        const sla = self.arrayAsSlice(u8);
        const slb = other.arrayAsSlice(u8);
        for (sla[0..@min(sla.len, slb.len)], 0..) |va, index| {
            const vb = slb[index];
            if (va < vb) return ord.lt;
            if (va > vb) return ord.gt;
        }
        if (sla.len < slb.len) return ord.lt;
        if (sla.len > slb.len) return ord.gt;
        return ord.eq;
    }
    inline fn which_class(self: Object, comptime full: bool) ClassIndex {
        return switch (self.tag) {
            .numericThunk, .immediateThunk, .heapThunk, .nonLocalThunk, .heapClosure => .BlockClosure,
            .immediates => self.classIndex,
            .heap => if (full) self.to(HeapObjectPtr).*.getClass() else .Object,
            .unused1 => unreachable,
            else => |tag| if (tag.u() <= Group.immediates.u()) .Float else .SmallInteger,
        };
    }
    pub inline fn immediate_class(self: Object) ClassIndex {
        return self.which_class(false);
    }
    pub inline fn get_class(self: Object) ClassIndex {
        return self.which_class(true);
    }
    pub fn full_get_class(self: Object) ClassIndex {
        return self.which_class(true);
    }
    pub inline fn promoteTo(self: Object) !Object {
        if (self.isUnmoving()) return self;
        unreachable;
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
            .Object => writer.print("object:0x{x:>16}", .{self.u()}), //,as_pointer(x));
            .BlockClosure => writer.print("block:0x{x:>16}", .{self.u()}), //,as_pointer(x));
            .False => writer.print("false", .{}),
            .True => writer.print("true", .{}),
            .UndefinedObject => writer.print("nil", .{}),
            .Symbol => writer.print("#{s}", .{symbol.asString(self).arrayAsSlice(u8)}),
            .Character => writer.print("${c}", .{self.to(u8)}),
            .SmallInteger => writer.print("{d}", .{self.toInt()}),
            .Float => writer.print("{}(0x{x})", .{ self.to(f64), self.u() }),
            else => {
                try writer.print("0x{x:>16}", .{self.u()});
                @panic("format for unknown class");
            },
        };
        if (fmt.len == 1 and fmt[0] == 'x') try writer.print("(0x{x:>16})", .{self.u()});
    }
    pub const alignment = @alignOf(u64);
    pub fn packedInt(f0: u16, f1: u16, f2: u16) Object {
        return Object{ .tag = .smallInt0, .h0 = f0, .h1 = f1, .classIndex = @enumFromInt(f2) };
    }
};

test "testing doubles including NaN" {
    // test that all things that generate NaN generate positive ones
    // otherwise we'd need to check in any primitive that could create a NaN
    // because a negative one could look like one of our tags (in particular a large positive SmallInteger)
    const e = std.testing.expect;
    const inf = @as(f64,1.0)/0.0;
    const zero = @as(f64,0);
    const one = @as(f64,1);
    const cast = Object.cast;
    try e(of(1).isDouble());
    try e(cast(@sqrt(-one)).isDouble());
    try e(cast(@log(-one)).isDouble());
    try e(cast(zero/zero).isDouble());
    try e(cast((-inf)*0.0).isDouble());
    try e(cast((-inf)*inf).isDouble());
    try e(cast((-inf)+inf).isDouble());
    try e(cast(inf-inf).isDouble());
    try e(cast(inf*0.0).isDouble());
    try e(cast(std.math.nan(f64)).isDouble());
}
test "from conversion" {
    const ee = std.testing.expectEqual;
    try ee(@as(u64, @bitCast(Object.packedInt(1, 2, 3))), u64_ZERO + 0x000300020001);
    try ee(@as(f64, @bitCast(Object.from(3.14))), 3.14);
    try ee(Object.from(42).u(), u64_ZERO +% 42);
    try ee(Object.from(3.14).immediate_class(), .Float);
    try std.testing.expect(Object.from(3.14).isDouble());
    try ee(Object.from(3).immediate_class(), .SmallInteger);
    try std.testing.expect(Object.from(3).isInt());
    try std.testing.expect(Object.from(false).isBool());
    try ee(Object.from(false).immediate_class(), .False);
    try ee(Object.from(true).immediate_class(), .True);
    try std.testing.expect(Object.from(true).isBool());
    try ee(Object.from(null).immediate_class(), .UndefinedObject);
    try std.testing.expect(Object.from(null).isNil());
}
test "to conversion" {
    const ee = std.testing.expectEqual;
    try ee(Object.from(3.14).to(f64), 3.14);
    try ee(Object.from(42).toInt(), 42);
    try std.testing.expect(Object.from(42).isInt());
    try ee(Object.from(true).to(bool), true);
    try ee(of(u64_MAXVAL).toUnchecked(i64), 0x3_ffffffffffff);
    try ee(Object.from(-0x400000).toUnchecked(i64), -0x400000);
}
test "immediate_class" {
    const ee = std.testing.expectEqual;
    try ee(Object.from(3.14).immediate_class(), .Float);
    try ee(Object.from(42).immediate_class(), .SmallInteger);
    try ee(Object.from(true).immediate_class(), .True);
    try ee(Object.from(false).immediate_class(), .False);
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
