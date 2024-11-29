const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const math = std.math;
const config = @import("config.zig");
// usingnamespace @import("config.zig");
// const std = @This().std;
const debugError = false;
const symbol = if (debugError) struct {
    const inversePhi24 = @import("utilities.zig").inversePhi(u24);
    pub inline fn fromHash32(hash: u32) Object {
        return Object.makeImmediate(.Symbol, hash);
    }
    inline fn symbol_of(index: u24, arity: u8) Object {
        return fromHash32(@as(u32, index *% inversePhi24) << 8 | arity);
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
test "indexSymbol" {
    const e = std.testing.expect;
    const ee = std.testing.expectEqual;
    try e(Object.indexSymbol0(42).isSymbol());
    try ee(Object.imm(.Symbol, 0x0002a0ff), 0xfff000070002a0ff);
    try ee(Object.indexSymbol0(0x2a).rawU(), 0xfff0000700002aff);
    try ee(Object.indexSymbol0(0x2a).indexNumber(), 42);
    try e(Object.indexSymbol1(42).isSymbol());
    try ee(Object.indexSymbol1(0x2a).rawU(), 0xfff0000701002aff);
    try ee(Object.indexSymbol1(0x2a).indexNumber(), 0x1002a);
}
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
    ThunkHeap,
    ThunkReturnLocal,
    ThunkReturnInstance,
    ThunkReturnSmallInteger,
    ThunkReturnImmediate,
    ThunkReturnCharacter,
    BlockAssignLocal,
    BlockAssignInstance,
    ThunkGetInstance,
    False,
    True,
    SmallInteger,
    Symbol,
    Character,
    ShortString,
    ThunkImmediate,
    ThunkFloat,
    UndefinedObject=23,
    Float,
    ProtoObject,
    Object,
    BlockClosure,
    BlockClosureValue,
    Context,
    Array,
    String,
    Utf8String,
    DoubleWordArray,
    Process,
    Class,
    CompiledMethod,
    Dispatch,
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
        ThunkHeap,
        ThunkReturnLocal,
        ThunkReturnInstance,
        ThunkReturnSmallInteger,
        ThunkReturnImmediate,
        ThunkReturnCharacter,
        BlockAssignLocal,
        BlockAssignInstance,
        ThunkGetInstance,
        False,
        True,
        SmallInteger,
        Symbol,
        Character,
        ShortString,
        ThunkImmediate,
        ThunkFloat,
         inline fn classIndex(cp: Compact) ClassIndex {
            return @enumFromInt(@intFromEnum(cp));
        }
    };
    pub inline fn compact(ci: ClassIndex) Compact {
        return @enumFromInt(@intFromEnum(ci));
    }
};
comptime {
    std.debug.assert(@intFromEnum(ClassIndex.replace0) == 0xffff);
    std.testing.expectEqual(@intFromEnum(ClassIndex.ThunkHeap),1) catch unreachable;
    std.testing.expectEqual(@intFromEnum(ClassIndex.ThunkFloat),17) catch unreachable;
}
const MemoryFloat = extern struct {
    header: HeapHeader,
    value: f64,
};
pub inline fn simpleFloat(v: f64, age: Age) MemoryFloat {
    const u: u64 = @bitCast(v);
    const hash: u24 = @truncate(u ^ (u>>24) ^ (u>>48));
    return .{
        .header = .{ .classIndex = .Float, .hash = hash, .format = .notIndexable, .age = age, .length = 1 },
        .value = v,
    };
}
pub const Object = switch (config.objectEncoding) {
    .nan => NanObject,
    .tag => TagObject,
};
const NanObject = packed struct(u64) {
    h0: u16, // align(8),
    h1: u16,
    classIndex: ClassIndex,
    tag: Group,
    pub const Group = enum(u16) {
        immediates = 0xfff0,
        heapThunk = 0xfff5,
        nonLocalThunk,
        heap,
        smallIntMin,
        smallIntNeg_2,
        smallIntNeg_3,
        smallIntNeg_4,
        smallInt0,
        smallIntPos_6,
        smallIntPos_7,
        smallIntMax,
        _,
        const Self = @This();
        inline fn base(cg: Self) u64 {
            return @as(u64, @intFromEnum(cg)) << 48;
        }
        inline fn u(cg: Self) u16 {
            return @intFromEnum(cg);
        }
    };
    const Negative_Infinity: u64 = g(.immediates); //0xfff0000000000000;
    const Start_of_Heap_Objects: u64 = g(.heap);
    inline fn of(comptime v: u64) Object {
        return @bitCast(v);
    }
    inline fn oImm(c: ClassIndex, h: u32) Object {
        return @bitCast(imm(c,h));
    }
    inline fn imm(c: ClassIndex, h: u32) u64 {
        return g(.immediates) | (@as(u64, @intFromEnum(c)) << 32) | h;
    }
    inline fn g(grp: Group) u64 {
        return grp.base();
    }
    const nonIndexSymbol = 0xffffffffff0000ff;
    pub inline fn indexSymbol0(uniqueNumber: u16) Object {
        return oImm(.Symbol, 0x00000ff | @as(u32, uniqueNumber) << 8);
    }
    pub inline fn indexSymbol1(uniqueNumber: u16) Object {
        return oImm(.Symbol, 0x10000ff | @as(u32, uniqueNumber) << 8);
    }
    pub inline fn isIndexSymbol0(self: Object) bool {
        return (self.rawU() & nonIndexSymbol) == (comptime indexSymbol0(0).rawU() & nonIndexSymbol);
    }
    pub inline fn isIndexSymbol1(self: Object) bool {
        return (self.rawU() & nonIndexSymbol) == (comptime indexSymbol1(0).rawU() & nonIndexSymbol);
    }
    pub const invalidHeapPointer = of(Start_of_Heap_Objects);
    pub const ZERO = of(0);
    pub const False = oImm(.False, 0);
    pub const True = oImm(.True, 0);
    pub const Nil = oImm(.UndefinedObject, 0);
    pub const NotAnObject = oImm(.UndefinedObject, 0x3); // never a valid object... should never be visible to managed language
    pub const u64_MINVAL = g(.smallInt);
    const u64_ZERO = g(.smallInt0);
    pub const u64_ZERO2 = u64_ZERO *% 2;
    const u64_MAXVAL = g(.numericThunk) - 1;
    pub const MinSmallInteger = of(u64_MINVAL).to(i64); // anything smaller than this will underflow
    pub const MaxSmallInteger = of(u64_MAXVAL).to(i64); // anything larger than this will overflow
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
    pub inline fn indexNumber(self: Object) u24 {
        return @truncate(self.rawU()>>8);
    }
    pub inline fn makeImmediate(cls: ClassIndex, low32: u32) Object {
        return @bitCast(Group.immediates.base() | (@as(u64,@intFromEnum(cls)) << 32) | low32);
    }
    pub inline fn hash24(self: Object) u24 {
        return @truncate(self.rawU()>>8);
    }
    pub inline fn hash32(self: Object) u32 {
        return @truncate(self.rawU());
    }
    pub inline fn numArgs(self: Object) u8 {
        return @truncate(self.rawU());
    }
    pub inline fn withOffsetx(self: Object, offset: u32) Object {
        return cast(@as(u64, offset) << 32 | self.hash32());
    }
    pub inline fn cast(v: anytype) Object {
        // stored using little-endian order
        return @bitCast(v);
    }
    pub inline fn untaggedInt(self: Object) u64 {
        return self.toNatNoCheck();
    }
    pub inline fn tagged(tag: Group, low: u3, addr: u64) Object {
        return cast((Object{ .tag = tag, .classIndex = .none, .h1 = 0, .h0 = low }).rawU() + addr);
    }
    pub inline fn tagbitsL(self: Object) u32 {
        return @truncate(self.rawU() >> 32);
    }
    pub inline fn hashEquals(self: Object, other: Object) bool {
        //@truncate(u24,self.rawU()^other.rawU())==0;
        return self.hash32() == other.hash32();
    }
    pub inline fn selectorEquals(self: Object, other: Object) bool {
        //        return (self.rawU()^other.rawU())&0xffffffffffff == 0; // may be false positive
        return self.rawU() == other.rawU();
    }
    pub inline fn isInt(self: Object) bool {
        return switch (self.tag) {
            .smallIntMin, .smallIntNeg_2, .smallIntNeg_3, .smallIntNeg_4, .smallInt0, .smallIntPos_6, .smallIntPos_7, .smallIntMax => true,
            else => false
        };
    }
    pub inline fn atLeastInt(self: Object) bool { // only useful if you have a Smallinteger-u51
        return @intFromEnum(self.tag) >= @intFromEnum(Group.smallIntMin);
    }
    pub inline fn atMostInt(self: Object) bool { // only useful if you have a Smallinteger+u51
        return if (@intFromEnum(Group.smallIntMax)==0xffff)
            self.atLeastInt()
            else
            @intFromEnum(self.tag) <= @intFromEnum(Group.smallIntMax);
    }
    pub inline fn isNat(self: Object) bool {
        return switch (self.tag) {
            .smallInt0, .smallIntPos_6, .smallIntPos_7, .smallIntMax => true,
            else => false
        };
    }
    pub inline fn isDouble(self: Object) bool {
        return self.rawU() <= Negative_Infinity;
    }
    pub inline fn isNonLocalThunk(self: Object) bool {
        return switch (self.tag) {
            .nonLocalThunk => true,
            else => false};
    }
    pub inline fn isMemoryAllocated(self: Object) bool {
        return switch (self.tag) {
            .heapThunk,
            .nonLocalThunk,
            .heap => true,
            else => false};
    }
    pub inline fn isBlock(self: Object) bool {
        return switch (self.tag){
            .heapThunk,
            .nonLocalThunk => true,
            .immediates => switch (self.classIndex) {
                .ThunkSmallInteger,
                .ThunkFloat,
                .ThunkImmediate,
                .ThunkHeap,
                .ThunkReturnSelf,
                .ThunkReturnTrue,
                .ThunkReturnFalse,
                .ThunkReturnNil,
                .ThunkReturn_1,
                .ThunkReturn0,
                .ThunkReturn1,
                .ThunkReturn2 => true,
                else => false},
            else => false};
    }
    pub inline fn toBoolNoCheck(self: Object) bool {
        return @as(u1, @truncate(self.rawU())) == 1;
    }
    pub inline fn toIntNoCheck(self: Object) i64 {
        return @as(i64, @bitCast(self.rawU() -% u64_ZERO));
    }
    pub inline fn toNatNoCheck(self: Object) u64 {
        return self.rawU() -% u64_ZERO;
    }
    pub inline fn rawWordAddress(self: Object) u64 {
        return self.rawU() & 0xffff_ffff_fff8;
    }
    pub inline fn toDoubleNoCheck(self: Object) f64 {
        return @bitCast(self);
    }
    pub inline fn from(value: anytype) Object {
        return fromWithError(value) catch unreachable;
    }
    pub inline fn fromWithError(value: anytype) !Object {
        const T = @TypeOf(value);
        if (T == Object) return value;
        switch (@typeInfo(T)) {
            .Int,.ComptimeInt => return cast(@as(u64, @bitCast(@as(i64, value))) +% u64_ZERO),
            .Float => return cast(value),
            .ComptimeFloat => return cast(@as(f64, value)),
            .Bool => return if (value) Object.True else Object.False,
            .Null => return Object.Nil,
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
        @compileError("Can't convert \"" ++ @typeName(T) ++ "\"");
    }
    inline fn which_class(self: Object, comptime full: bool) ClassIndex {
        return switch (self.tag) {
            .heapThunk => .BlockClosure,
            .nonLocalThunk => @panic("nonLocalThunk"),
            .immediates => self.classIndex,
            .heap => if (full) self.to(HeapObjectPtr).*.getClass() else .Object,
            .smallIntMin, .smallIntNeg_2, .smallIntNeg_3, .smallIntNeg_4, .smallInt0, .smallIntPos_6, .smallIntPos_7, .smallIntMax => .SmallInteger,
            else => .Float
        };
    }
    pub usingnamespace ObjectFunctions;
};
const TagObject = packed struct(u64) {
    tag: Group,
    classIndex: ClassIndex,
    hash: u45,
    pub const Group = enum(u3) { heap=0, smallInteger, immediates, float3, float4, float5, float6, float7};
    pub inline fn cast(v: anytype) Object {
        // stored using little-endian order
        return @bitCast(v);
    }
    const Negative_Infinity: u64 = g(.immediates); //0xfff0000000000000;
    const Start_of_Heap_Objects: u64 = g(.heap);
    inline fn of(comptime v: u64) Object {
        return @bitCast(v);
    }
    inline fn oImm(c: ClassIndex, h: u32) Object {
        return @bitCast(imm(c,h));
    }
    inline fn imm(c: ClassIndex, h: u32) u64 {
        return g(.immediates) | (@as(u64, @intFromEnum(c)) << 32) | h;
    }
    inline fn g(grp: Group) u64 {
        return grp.base();
    }
    const nonIndexSymbol = 0xffffffffff0000ff;
    pub inline fn indexSymbol0(uniqueNumber: u16) Object {
        return oImm(.Symbol, 0x00000ff | @as(u32, uniqueNumber) << 8);
    }
    pub inline fn indexSymbol1(uniqueNumber: u16) Object {
        return oImm(.Symbol, 0x10000ff | @as(u32, uniqueNumber) << 8);
    }
    pub inline fn isIndexSymbol0(self: Object) bool {
        return (self.rawU() & nonIndexSymbol) == (comptime indexSymbol0(0).rawU() & nonIndexSymbol);
    }
    pub inline fn isIndexSymbol1(self: Object) bool {
        return (self.rawU() & nonIndexSymbol) == (comptime indexSymbol1(0).rawU() & nonIndexSymbol);
    }
    pub const invalidHeapPointer = of(Start_of_Heap_Objects);
    pub const ZERO = of(0);
    pub const False = oImm(.False, 0x0);
    pub const True = oImm(.True, 0x1);
    pub const Nil = oImm(.UndefinedObject, 0xffffffff);
    pub const NotAnObject = oImm(.UndefinedObject, 0x3); // never a valid object... should never be visible to managed language
    pub const u64_MINVAL = g(.smallInt);
    const u64_ZERO = g(.smallInt0);
    pub const u64_ZERO2 = u64_ZERO *% 2;
    const u64_MAXVAL = g(.numericThunk) - 1;
    pub const MinSmallInteger = of(u64_MINVAL).to(i64); // anything smaller than this will underflow
    pub const MaxSmallInteger = of(u64_MAXVAL).to(i64); // anything larger than this will overflow
    pub inline fn indexNumber(self: Object) u24 {
        return @truncate(self.hash>>8);
    }
    pub inline fn makeImmediate(cls: ClassIndex, low32: u32) Object {
        return .{.tag=.immediates,.classIndex=cls,.hash=low32};
    }
    pub inline fn hash24(self: Object) u24 {
        return @truncate(self.hash);
    }
    pub inline fn hash32(self: Object) u32 {
        return @truncate(self.hash);
    }
    pub inline fn numArgs(self: Object) u8 {
        return @truncate(self.hash);
    }
    const nanMemObject = simpleFloat(math.nan(f64),.static);
    const pInfMemObject = simpleFloat(math.inf(f64),.static);
    const nInfMemObject = simpleFloat(-math.inf(f64),.static);
    fn encode(x: f64) Object {
        const u = math.rotl(u64,@bitCast(x),4);
        if (u&7>=5) {
            if (math.isNan(x)) return try Object.from(&nanMemObject);
            if (math.inf(f64)==x) return try Object.from(&pInfMemObject);
            if (math.inf(f64)==-x) return try Object.from(&nInfMemObject);
            unreachable;
        }
        return u+%3;
    }
    fn decode(self: Object) f64 {
        return @bitCast(math.rotr(u64,self.rawU()-3,4));
    }
    pub inline fn from(value: anytype) !Object {
        const T = @TypeOf(value);
        if (T == Object) return value;
        switch (@typeInfo(T)) {
            .Int,.ComptimeInt => return cast(@as(u64, @bitCast(@as(i64, value)))<<3 +% @intFromEnum(Group.smallInteger)),
            .Float => return try encode(value),
            .ComptimeFloat => return try encode(@as(f64, value)),
            .Bool => return if (value) Object.True else Object.False,
            .Null => return Object.Nil,
            .Pointer => |ptr_info| {
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
            .heap => if (full) self.to(HeapObjectPtr).*.getClass() else .Object,
            .smallInteger => .SmallInteger,
            .immediates => self.classIndex,
            else => .Float
        };
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
    pub inline fn asSymbol(self: Object) Object {
        return Object.makeImmediate(.Symbol, self.hash32());
    }
    pub inline fn withImmClass(self: Object, cls: ClassIndex) Object {
        return Object.makeImmediate(cls,self.hash32());
    }
    pub inline fn asCharacter(int: u32) Object {
        return Object.makeImmediate(.Character, int);
    }
    pub inline fn setField(self: Object,field: usize, value: Object) void {
        if (self.asObjectArray()) |ptr| ptr[field] = value;
    }
    pub inline fn getField(self: Object,field: usize) Object {
        if (self.asObjectArray()) |ptr|
            return ptr[field];
        return Nil;
    }
    pub inline fn tagbits(self: Object) u16 {
        return @intFromEnum(self.tag);
    }
    pub inline fn indexEquals(self: Object, other: Object) bool {
        return self.equals(other.withImmClass(.Symbol)); // may be false positive
    }
    pub inline fn isSymbol(self: Object) bool {
        return self.tagbitsL() == comptime Object.indexSymbol0(0).tagbitsL();
    }
    pub inline fn isBool(self: Object) bool {
        const tag = self.tagbitsL();
        return tag == Object.False.tagbitsL() or tag == Object.True.tagbitsL();
    }
    pub inline fn isNil(self: Object) bool {
        return self.tagbitsL() == Object.Nil.tagbitsL();
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
    
    pub inline fn toWithCheck(self: Object, comptime T: type, comptime check: bool) T {
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
                    .Pointer => |ptrInfo| {
                        if (!check or (self.isMemoryAllocated() and (!@hasDecl(ptrInfo.child, "ClassIndex") or self.to(HeapObjectConstPtr).classIndex == ptrInfo.child.ClassIndex))) {
                            if (@hasField(ptrInfo.child, "header") or (@hasDecl(ptrInfo.child, "includesHeader") and ptrInfo.child.includesHeader)) {
                                return @as(T, @ptrFromInt(@as(usize, @bitCast(@as(i64, @bitCast(self)) << 16 >> 16))));
                            } else {
                                return @as(T, @ptrFromInt(@as(usize, @bitCast(@as(i64, @bitCast(self)) << 16 >> 16))+@sizeOf(HeapHeader)));
                            }
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
            .Object => writer.print("object:0x{x:0>16}", .{self.rawU()}), //,as_pointer(x));
            .BlockClosure => writer.print("block:0x{x:>16}", .{self.rawU()}), //,as_pointer(x));
            .False => writer.print("false", .{}),
            .True => writer.print("true", .{}),
            .UndefinedObject => writer.print("nil", .{}),
            .Symbol => if (self.isIndexSymbol0()) {
                try writer.print("#symbols.i_{}", .{self.indexNumber()});
            } else if (self.isIndexSymbol1()) {
                try writer.print("#symbols.m_{}", .{@as(u16, @truncate(self.indexNumber()))});
            } else writer.print("#{s}", .{symbol.asString(self).arrayAsSlice(u8)}),
            .Character => writer.print("${c}", .{self.to(u8)}),
            .SmallInteger => writer.print("{d}", .{self.toIntNoCheck()}),
            .Float => writer.print("{}(0x{x:0>16})", .{ self.to(f64), self.rawU() }),
            else => {
                try writer.print("0x{x:0>16}", .{self.rawU()});
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
    const inf = @as(f64, 1.0) / 0.0;
    const zero = @as(f64, 0);
    const one = @as(f64, 1);
    const fns = struct {
        fn cast(x: anytype) Object {return Object.from(x);}};
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
//    try ee(of(u64_MAXVAL).toUnchecked(i64), 0x3_ffffffffffff);
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
test "order" {
    const ee = std.testing.expectEqual;
    const Buf = extern union { buf: [8]u8, obj: Object,};
    const buf1 = (Buf{.obj = Object.from(42)}).buf;
    try ee(buf1[0],42);
    try ee(buf1[6],252);
    try ee(buf1[7],255);
    const buf2 = (Buf{.obj = Object.from(42.0),}).buf;
    try ee(buf2[0],0);
    try ee(buf2[6],69);
    try ee(buf2[7],64);
}
