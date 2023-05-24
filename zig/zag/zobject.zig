const std = @import("std");
const mem = std.mem;
const builtin = @import("builtin");
const native_endian = builtin.target.cpu.arch.endian();
const symbol = @import("symbol.zig");
const heap = @import("heap.zig");
const HeapObject = heap.HeapObject;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
pub const ClassIndex = u16;
const largerPowerOf2 = @import("utilities.zig").largerPowerOf2;
inline fn of(comptime v: u64) Object {
    return @bitCast(Object,v);
}
inline fn oImm(c: Level2, comptime h: comptime_int) u64 {
    return o(.immediates)|@as(u64,@enumToInt(c))<<32|h;
}
inline fn o(g:Group) u64 {
    return g.base();
}
pub inline fn indexSymbol(uniqueNumber: u64) Object {
    return @bitCast(Object,oImm(.Symbol,0xff000000)|uniqueNumber);
}
pub const ZERO              = of(0);
const Negative_Infinity: u64     =    o(.immediates); //0xfff0000000000000;
// unused NaN fff00-fff4f
const Start_of_Blocks: u64 =          o(.immediateThunk);
const Start_of_Pointer_Objects: u64 = o(.closureFreeBlock);
const Start_of_Heap_Objects: u64 =    o(.heap);
pub const False             = of(oImm(.False,0x0));
pub const True              = of(oImm(.True,0x1));
pub const Nil               = of(oImm(.UndefinedObject,0x2));
pub const NotAnObject       = of(oImm(.UndefinedObject,0x3)); // never a valid object... should never be visible to managed language
const Symbol_Base           =    oImm(.Symbol,0);
const Character_Base        =    oImm(.Character,0);
pub const u64_MINVAL            =    o(.smallInt);
const u64_ZERO              =    o(.smallInt0);
pub const u64_ZERO2              =    u64_ZERO*%2;
const u64_MAXVAL            =    o(.unused1)-1;
pub const MinSmallInteger = of(u64_MINVAL).to(i64); // anything smaller than this will underflow
pub const MaxSmallInteger = of(u64_MAXVAL).to(i64); // anything larger than this will overflow
pub const invalidHeapPointer = of(Start_of_Heap_Objects);

pub fn fromLE(comptime T: type, v: T) Object {
    const val = @ptrCast(*const [@sizeOf(T)]u8,&v);
    return of(mem.readIntLittle(T,val));
}
pub const compareObject = Object.compare;
pub const Level2 = enum(ClassIndex) {
    Object = 1, SmallInteger, Float, False, True, UndefinedObject, Symbol, Character, BlockClosure, Array, String, SymbolTable, Method, CompiledMethod, _ };
pub const Object_I = @enumToInt(Level2.Object);
pub const SmallInteger_I = @enumToInt(Level2.SmallInteger);
pub const Float_I = @enumToInt(Level2.Float);
pub const False_I = @enumToInt(Level2.False);
pub const True_I = @enumToInt(Level2.True);
pub const UndefinedObject_I = @enumToInt(Level2.UndefinedObject);
pub const Symbol_I = @enumToInt(Level2.Symbol);
pub const Character_I = @enumToInt(Level2.Character);
pub const BlockClosure_I = @enumToInt(Level2.BlockClosure);
pub const Array_I = @enumToInt(Level2.Array);
pub const String_I = @enumToInt(Level2.String);
pub const SymbolTable_I = @enumToInt(Level2.SymbolTable);
pub const Method_I = @enumToInt(Level2.Method);
pub const CompiledMethod_I = @enumToInt(Level2.CompiledMethod);
pub const Group = enum(u16) {
    immediates = 0xfff0, smallInt, smallInt0 = 0xfff5, smallIntMax = 0xfff8, unused1, unused2, immediateThunk, closureFreeBlock, nonLocalThunk, heapClosure, heap,  _,
    const Self = @This();
    inline fn base(cg: Self) u64 {
        return @as(u64,@enumToInt(cg))<<48;
    }
    inline fn u(cg: Self) u16 {
        return @enumToInt(cg);
    }
};
pub const Object = packed struct(u64) {
    h0: u16, // align(8),
    h1: u16,
    l2: ClassIndex,
    tag: Group,
    pub inline fn makeImmediate(cls: ClassIndex, low32: u32) Object {
        return cast(low32|Group.immediates.base()|@as(u64,cls)<<32);
    }
    pub inline fn cast(v: anytype) Object {
        return @bitCast(Object,v);
    }
    pub inline fn hash(self: Object) Object {
        return cast(self.u()|u64_ZERO);
    }
    pub inline fn hash24(self: Object) u24 {
        return @truncate(u24,self.u());
    }
    pub inline fn hash32(self: Object) u32 {
        return @truncate(u32,self.u());
    }
    pub inline fn numArgs(self: Object) u8 {
        return @truncate(u8,self.u()>>24);
    }
    pub inline fn u(self: Object) u64 {
        return @bitCast(u64,self);
    }
    pub inline fn i(self: Object) i64 {
        return @bitCast(i64,self);
    }
    pub inline fn tagbits(self: Object) u16 {
        return @enumToInt(self.tag);
    }
    pub inline fn tagbitsL(self: Object) u32 {
        return @truncate(u32,self.u()>>32);
    }
    pub inline fn equals(self: Object,other: Object) bool {
        return self.u() == other.u();
    }
    pub inline fn isInt(self: Object) bool {
        return self.tag.u() >= Group.smallInt.u() and self.tag.u() <= Group.smallIntMax.u();
    }
    pub inline fn isNat(self: Object) bool {
        return self.tag.u() >= Group.smallInt0.u() and self.tag.u() <= Group.smallIntMax.u();
    }
    pub inline fn isDouble(self: Object) bool {
        return self.u() <= Negative_Infinity;
    }
    pub inline fn isBool(self: Object) bool {
        const tag = self.tagbitsL();
        return tag==False.tagbitsL() or tag==True.tagbitsL();
    }
    pub inline fn isNil(self: Object) bool {
        return self.tagbitsL() == Nil.tagbitsL();
    }
    pub inline fn isHeapObject(self: Object) bool {
        return self.tag == Group.heap;
    }
    pub inline fn isHeapAllocated(self: Object) bool {
        const tag = self.tagbits();
        return tag >= Start_of_Pointer_Objects>>48;
    }
    pub inline fn isUnmoving(self: Object) bool {
        return !self.isHeapAllocated() or self.to(HeapObjectPtr).isUnmoving();
    }
    pub inline fn isLiteral(self: Object) bool {
        return !self.isHeapAllocated();
    }
    pub inline fn isBlock(self: Object) bool {
        const tag = self.tagbits();
        return tag >= Start_of_Blocks>>48 and !self.isHeapObject();
    }
    pub inline fn isIndexSymbol(self: Object) bool {
        return (self.u()>>24)==(comptime indexSymbol(0).u()>>24);
    }
    pub inline fn isSymbol(self: Object) bool {
        return self.tagbitsL()==comptime indexSymbol(0).tagbitsL();
    }
    pub inline fn toBool(self: Object) bool {
        if (self.isBool()) return @truncate(u1,self.u())==1;
        @panic("Trying to convert Object to bool");
    }
    pub inline fn toInt(self: Object) i64 {
        if (self.isInt()) return @bitCast(i64, self.u() -% u64_ZERO);
        @panic("Trying to convert Object to i64");
    }
    pub inline fn toNat(self: Object) u64 {
        if (self.isNat()) return self.u() -% u64_ZERO;
        @panic("Trying to convert Object to u64");
    }
    pub fn toWithCheck(self: Object, comptime T:type, comptime check: bool) T {
        switch (T) {
            f64 => {if (check and self.isDouble()) return @bitCast(f64, self);},
            i64 => {if (check and self.isInt()) return self.toInt();},
            u64 => {if (check and self.isNat()) return self.toNat();},
            bool=> {if (check and self.isBool()) return self.equals(True);},
            //u8  => {return @intCast(u8, self.hash & 0xff);},
            else => {
                switch (@typeInfo(T)) {
                    .Pointer => |ptrInfo| {
                        if (!check or (self.isHeapAllocated() and (!@hasDecl(ptrInfo.child,"ClassIndex") or self.to(HeapObjectConstPtr).classIndex==ptrInfo.child.ClassIndex))) {
                            if (@hasDecl(ptrInfo.child,"includesHeader") and ptrInfo.child.includesHeader) {
                                return @intToPtr(T, @bitCast(usize, @bitCast(i64, self) << 16 >> 16));
                            } else {
                                return @intToPtr(T, @bitCast(usize, @bitCast(i64, self) << 16 >> 16)+@sizeOf(HeapObject));
                            }
                        }
                        @panic("Trying to convert Object pointer to "++@typeName(T));
                    },
                    else => {},
                }
            },
        }
        @panic("Trying to convert Object to "++@typeName(T));
    }
    pub  fn to(self: Object, comptime T:type) T {
        return self.toWithCheck(T,true);
    }
    pub  fn toUnchecked(self: Object, comptime T:type) T {
        if (T == i64) return @bitCast(i64, self.u() -% u64_ZERO);
        if (T == u64) return self.u() - u64_ZERO;
        return self.toWithCheck(T,false);
    }
    pub  fn header(self: Object) HeapObject {
        if (self.isHeapObject()) return self.to(HeapObjectPtr).*;
        return @bitCast(HeapObject,@as(u64,0));
    }
    pub  fn instVars(self: Object) []Object {
        if (self.isHeapObject()) return self.to(HeapObjectPtr).instVars();
        return &[0]Object{};
    }
    pub  fn arrayAsSlice(self: Object, comptime T:type) []T {
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
    pub  fn isIndexable(self: Object) bool {
        if (self.isHeapObject()) return self.to(HeapObjectPtr).isIndexable();
        return false;
    }
    pub  fn inHeapSize(self: Object) usize {
        if (self.isHeapObject()) return self.to(HeapObjectPtr).inHeapSize();
        return 0;
    }
    pub inline fn from(value: anytype) Object {
        const T = @TypeOf(value);
        if (T==Object) return value;
        if (T==HeapObjectConstPtr) return cast(@truncate(u48,@ptrToInt(value)) + Start_of_Heap_Objects);
        switch (@typeInfo(@TypeOf(value))) {
            .Int, .ComptimeInt => return cast(@bitCast(u64, @as(i64, value)) +% u64_ZERO),
            .Float, .ComptimeFloat => return cast(@as(f64, value)),
            .Bool => return if (value) True else False,
            .Null => return Nil,
            .Pointer => |ptr_info| {
                switch (ptr_info.size) {
                    .One => {
                        return cast(@truncate(u48,@ptrToInt(value)) + Start_of_Heap_Objects);
                    },
                    else => {},
                }
            },
            else => {},
        }
        @compileError("Can't convert \""++@typeName(@TypeOf(value))++"\"");
    }
    pub fn new(classIndex: ClassIndex, comptime tup: anytype) Object {
        // @setEvalBranchQuota(20000);
        // const obj = arenas.globalArena.allocObject(classIndex,tup.len);
        // inline for (tup,obj.instVars()) |field,*iVar| {
        //     iVar.* = from(field);
        // }
        // return obj;
        _ = classIndex; _ = tup; unreachable;
    }
    pub fn compare(self: Object, other: Object) std.math.Order {
        if (!self.isHeapObject() or !other.isHeapObject()) {
            const u64s = self.u();
            const u64o = other.u();
            return std.math.order(u64s,u64o);
        }
        const ord = std.math.Order;
        if (self.equals(other)) return ord.eq;
        const sla = self.arrayAsSlice(u8);
        const slb = other.arrayAsSlice(u8);
        for (sla[0..@min(sla.len,slb.len)],0..) |va,index| {
            const vb=slb[index];
            if (va<vb) return ord.lt;
            if (va>vb) return ord.gt;
        }
        if (sla.len<slb.len) return ord.lt;
        if (sla.len>slb.len) return ord.gt;
        return ord.eq;
    }
    inline fn which_class(self: Object, comptime full: bool) ClassIndex {
        return switch (self.tag) {
            .immediateThunk, .closureFreeBlock, .nonLocalThunk, .heapClosure => BlockClosure_I,
            .immediates => self.l2,
            .heap => if (full) self.to(HeapObjectPtr).*.getClass() else Object_I,
            .unused1, .unused2 => unreachable,
            else => |tag| if (tag.u() <= Group.immediates.u()) Float_I else SmallInteger_I,
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
        _ = fmt;
        _ = options;
        
        try switch (self.immediate_class()) {
            Object_I => writer.print("object:0x{x:>16}", .{self.u()}), //,as_pointer(x));
            False_I => writer.print("false", .{}),
            True_I => writer.print("true", .{}),
            UndefinedObject_I => writer.print("nil", .{}),
            Symbol_I => writer.print("#{s}", .{symbol.asString(self).arrayAsSlice(u8)}),
            Character_I => writer.print("${c}", .{self.to(u8)}),
            SmallInteger_I => writer.print("{d}", .{self.toInt()}),
            Float_I => writer.print("{}", .{self.to(f64)}),
            else => { try writer.print("0x{x:>16}", .{self.u()});@panic("format for unknown class");},
        };
    }
    pub const alignment = @alignOf(u64);
    pub fn packedInt(f0: u16, f1: u16, f2: u16) Object {
        return Object{.tag =.smallInt0, .h0 = f0, .h1 = f1, .l2 = f2};
    }

};

test "new" {
//    const o1 = Object.new(1,.{3,True,Nil});
//    std.debug.print("\nOBJ = {} {}\n",.{o1,o1.full_get_class()});
}
test "slicing" {
//    const testing = std.testing;
//    try testing.expectEqual(Nil.arrayAsSlice(u8).len,0);
}
test "from conversion" {
    const ee = std.testing.expectEqual;
    try ee(@bitCast(u64, Object.packedInt(1,2,3)), u64_ZERO+0x000300020001);
    try ee(@bitCast(f64, Object.from(3.14)), 3.14);
    try ee(Object.from(42).u(), u64_ZERO +% 42);
    try ee(Object.from(3.14).immediate_class(),Float_I);
    try std.testing.expect(Object.from(3.14).isDouble());
    try ee(Object.from(3).immediate_class(),SmallInteger_I);
    try std.testing.expect(Object.from(3).isInt());
    try std.testing.expect(Object.from(false).isBool());
    try ee(Object.from(false).immediate_class(),False_I);
    try ee(Object.from(true).immediate_class(),True_I);
    try std.testing.expect(Object.from(true).isBool());
    try ee(Object.from(null).immediate_class(),UndefinedObject_I);
    try std.testing.expect(Object.from(null).isNil());
}
test "to conversion" {
    const ee = std.testing.expectEqual;
    try ee(Object.from(3.14).to(f64), 3.14);
    try ee(Object.from(42).toInt(), 42);
    try std.testing.expect(Object.from(42).isInt());
    try ee(Object.from(true).to(bool), true);
    try ee(of(u64_MAXVAL).toUnchecked(i64),0x3_ffffffffffff);
    try ee(Object.from(-0x400000).toUnchecked(i64),-0x400000);
}
test "immediate_class" {
    const ee = std.testing.expectEqual;
    try ee(Object.from(3.14).immediate_class(), Float_I);
    try ee(Object.from(42).immediate_class(), SmallInteger_I);
    try ee(Object.from(true).immediate_class(), True_I);
    try ee(Object.from(false).immediate_class(), False_I);
    try ee(Nil.immediate_class(),UndefinedObject_I);
    try ee(True.immediate_class(),True_I);
    try ee(False.immediate_class(),False_I);
    try ee(symbol.symbols.yourself.immediate_class(),Symbol_I);
    
}
test "printing" {
    var buf: [255]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const stream = fbs.writer();
    try stream.print("{}\n",.{Object.from(42)});
    try stream.print("{}\n",.{symbol.symbols.yourself});
    try std.testing.expectEqualSlices(u8, "42\n#yourself\n", fbs.getWritten());
}
