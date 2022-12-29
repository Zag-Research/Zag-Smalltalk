const std = @import("std");
const mem = std.mem;
const builtin = @import("builtin");
const native_endian = builtin.target.cpu.arch.endian();
const symbol = @import("symbol.zig");
const arenas = @import("arenas.zig");
const heap = @import("heap.zig");
const Header = heap.Header;
const HeapPtr = heap.HeapPtr;
const HeapConstPtr = heap.HeapConstPtr;
const Thread = @import("thread.zig");
//const Dispatch = @import("dispatch.zig");
//const Context = Dispatch.Context;
const Code = @import("execute.zig").Code;
const class = @import("class.zig");
const ClassIndex = class.ClassIndex;
const largerPowerOf2 = @import("utilities.zig").largerPowerOf2;
inline fn of(comptime v: u64) Object {
    return @bitCast(Object,v);
}
inline fn o2(c: ClassIndex, comptime h: comptime_int) u64 {
    return (((@as(u64,ClassGrouping.Immediates)<<16)+c)<<32)+h;
}
pub const ZERO              = of(0);
const Negative_Infinity: u64     =    0xfff0000000000000;
// unused NaN fff00-fff4f
const Start_of_Blocks: u64 =          0xfff3_000000000000;
const Start_of_Pointer_Objects: u64 = 0xfff4_000000000000;
const End_of_Blocks: u64 =            0xfff6_ffffffffffff;
const Start_of_Heap_Objects: u64 =    0xfff7_000000000000;
const End_of_Heap_Objects: u64   =    0xfff7_ffffffffffff;
pub const False             = of(o2(class.False_I,0x0));
pub const True              = of(o2(class.True_I,0x1));
pub const Nil               = of(o2(class.UndefinedObject_I,0x2));
pub const NotAnObject       = of(o2(class.UndefinedObject_I,0x3)); // never a valid object... should never be visible to managed language
const Symbol_Base           =    o2(class.Symbol_I,0);
const Character_Base        =    o2(class.Character_I,0);
pub const u64_MINVAL            =    0xfff8_000000000000;
const u64_ZERO              =    0xfffc_000000000000;
const u64_MAXVAL            =    0xffff_ffffffffffff;
pub const MinSmallInteger = of(u64_MINVAL).to(i64); // anything smaller than this will underflow
pub const MaxSmallInteger = of(u64_MAXVAL).to(i64); // anything larger than this will overflow
pub const invalidHeapPointer = of(Start_of_Heap_Objects);

pub fn fromLE(comptime T: type, v: T) Object {
    const val = @ptrCast(*const [@sizeOf(T)]u8,&v);
    return of(mem.readIntLittle(T,val));
}
pub const compareObject = Object.compare;
pub const Level2 = enum(u16) { Object = 1, SmallInteger, Float, False, True, UndefinedObject, Symbol, Character, _ };
pub const ClassGrouping = enum(u16) {
    immediates = Immediates, immediateThunk, closureFreeBlock, selfThunk, heapClosure, heap, smallIntMin, smallInt0 = 0xfffc, _,
    const Immediates = 0xfff2;
};
pub const Object = packed struct(u64) {
    h0: u16, // align(8),
    h1: u16,
    l2: Level2,
    signMantissa: ClassGrouping,
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
        return @truncate(u16,self.u()>>48);
    }
    pub inline fn tagbitsL(self: Object) u32 {
        return @truncate(u32,self.u()>>32);
    }
    pub inline fn equals(self: Object,other: Object) bool {
        return self.u() == other.u();
    }
    pub inline fn isInt(self: Object) bool {
        return self.tagbits() >= u64_MINVAL>>48;
    }
    pub inline fn isNat(self: Object) bool {
        return self.tagbits() >= u64_ZERO>>48;
    }
    pub inline fn isDouble(self: Object) bool {
        return self.tagbits() <= Negative_Infinity>>48;
    }
    pub inline fn isBool(self: Object) bool {
        const tag = self.tagbitsL();
        return tag==False.tagbitsL() or tag==True.tagbitsL();
    }
    pub inline fn isNil(self: Object) bool {
        return self.tagbitsL() == Nil.tagbitsL();
    }
    pub inline fn isHeapObject(self: Object) bool {
        return self.tagbits() == Start_of_Heap_Objects>>48;
    }
    pub inline fn isHeapAllocated(self: Object) bool {
        const tag = self.tagbits();
        return tag >= Start_of_Pointer_Objects>>48 and  tag <= End_of_Heap_Objects>>48;
    }
    pub inline fn isLiteral(self: Object) bool {
        return !self.isHeapAllocated();
    }
    pub inline fn isBlock(self: Object) bool {
        const tag = self.tagbits();
        return tag >= Start_of_Blocks>>48 and  tag <= End_of_Blocks>>48;
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
                        if (check and (self.isHeapObject() and (!@hasDecl(ptrInfo.child,"ClassIndex") or self.to(HeapConstPtr).classIndex==ptrInfo.child.ClassIndex))) {
                            if (@hasDecl(ptrInfo.child,"includesHeader") and ptrInfo.child.includesHeader) {
                                return @intToPtr(T, @bitCast(usize, @bitCast(i64, self) << 16 >> 16));
                            } else {
                                return @intToPtr(T, @bitCast(usize, @bitCast(i64, self) << 16 >> 16)+@sizeOf(heap.Header));
                            }
                        }
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
        //const pr = std.io.getStdOut().writer().print;
        //pr("0x{X:0>16}\n",.{self.u()}) catch unreachable;
        if (T == i64) return @bitCast(i64, self.u() -% u64_ZERO);
        if (T == u64) return self.u() - u64_ZERO;
        return self.toWithCheck(T,false);
    }
    pub  fn header(self: Object) heap.Header {
        if (self.isHeapObject()) return self.to(HeapPtr).*;
        return @bitCast(heap.Header,@as(u64,0));
    }
    pub  fn instVars(self: Object) []Object {
        if (self.isHeapObject()) return self.to(HeapPtr).instVars();
        return &[0]Object{};
    }
    pub  fn arrayAsSlice(self: Object, comptime T:type) []T {
        if (self.isIndexable()) return self.to(HeapPtr).arrayAsSlice(T) catch return &[0]T{};
        return &[0]T{};
    }
    pub fn size(self: Object) !usize {
        if (!self.isHeapObject()) return error.NotIndexable;
        return self.to(HeapPtr).arraySize();
    }
    pub fn growSize(self: Object, stepSize: usize) !usize {
        if (!self.isHeapObject()) return error.NotIndexable;
        return self.to(HeapPtr).growSize(stepSize);
    }
    pub  fn isIndexable(self: Object) bool {
        if (self.isHeapObject()) return self.to(HeapPtr).isIndexable();
        return false;
    }
    pub  fn inHeapSize(self: Object) usize {
        if (self.isHeapObject()) return self.to(HeapPtr).inHeapSize();
        return 0;
    }
    pub inline fn from(value: anytype) Object {
        const T = @TypeOf(value);
        if (T==HeapConstPtr) return cast(@truncate(u48,@ptrToInt(value)) + Start_of_Heap_Objects);
        switch (@typeInfo(@TypeOf(value))) {
            .Int,
            .ComptimeInt => {
                return cast(@bitCast(u64, @as(i64, value)) +% u64_ZERO);
            },
            .Float,
            .ComptimeFloat => {
                return cast(@as(f64, value));
            },
            .Bool => {
                return if (value) True else False;
            },
            .Null => {
                return Nil;
            },
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
        for (sla[0..@min(sla.len,slb.len)]) |va,index| {
            const vb=slb[index];
            if (va<vb) return ord.lt;
            if (va>vb) return ord.gt;
        }
        if (sla.len<slb.len) return ord.lt;
        if (sla.len>slb.len) return ord.gt;
        return ord.eq;
    }
    pub inline fn immediate_class(self: Object) ClassIndex {
        const tag = self.tagbits();
        if (tag >= u64_MINVAL>>48) return class.SmallInteger_I;
        if (tag == Start_of_Heap_Objects>>48) return class.Object_I;
        if (tag >= Start_of_Blocks>>48) return class.BlockClosure_I;
        if (tag <= Negative_Infinity>>48) return class.Float_I;
        if (tag == ClassGrouping.Immediates) return @truncate(ClassIndex,self.u() >> 32);
        @panic("unknown encoding");
    }
    pub inline fn get_class(self: Object) ClassIndex {
        const immediate = self.immediate_class();
        if (immediate > 1) return immediate;
        return self.to(HeapPtr).*.getClass();
    }
    pub inline fn promoteTo(self: Object) !Object {
        return arenas.GlobalArena.promote(self);
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
            class.Object_I => writer.print("object:0x{x:>16}", .{self.u()}), //,as_pointer(x));
            class.False_I => writer.print("false", .{}),
            class.True_I => writer.print("true", .{}),
            class.UndefinedObject_I => writer.print("nil", .{}),
            class.Symbol_I => writer.print("#{s}", .{symbol.asString(self).arrayAsSlice(u8)}),
            class.Character_I => writer.print("${c}", .{self.to(u8)}),
            class.SmallInteger_I => writer.print("{d}", .{self.toInt()}),
            class.Float_I => writer.print("{}", .{self.to(f64)}),
            else => { try writer.print("0x{x:>16}", .{self.u()});@panic("format for unknown class");},
        };
    }
    pub const alignment = @alignOf(u64);
    pub fn packedInt(f0: u16, f1: u16, f2: u16) Object {
        return Object{.signMantissa =.smallInt0, .h0 = f0, .h1 = f1, .l2 = @intToEnum(Level2,f2)};
    }

};

test "slicing" {
//    const testing = std.testing;
//    try testing.expectEqual(Nil.arrayAsSlice(u8).len,0);
}
test "from conversion" {
    const ee = std.testing.expectEqual;
    try ee(@bitCast(u64, Object.packedInt(1,2,3)), 0xfffc000300020001);
    try ee(@bitCast(f64, Object.from(3.14)), 3.14);
    try ee(Object.from(42).u(), u64_ZERO +% 42);
    try ee(Object.from(3.14).immediate_class(),class.Float_I);
    try std.testing.expect(Object.from(3.14).isDouble());
    try ee(Object.from(3).immediate_class(),class.SmallInteger_I);
    try std.testing.expect(Object.from(3).isInt());
    try std.testing.expect(Object.from(false).isBool());
    try ee(Object.from(false).immediate_class(),class.False_I);
    try ee(Object.from(true).immediate_class(),class.True_I);
    try std.testing.expect(Object.from(true).isBool());
    try ee(Object.from(null).immediate_class(),class.UndefinedObject_I);
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
    try ee(Object.from(3.14).immediate_class(), class.Float_I);
    try ee(Object.from(42).immediate_class(), class.SmallInteger_I);
    try ee(Object.from(true).immediate_class(), class.True_I);
    try ee(Object.from(false).immediate_class(), class.False_I);
    try ee(Nil.immediate_class(),class.UndefinedObject_I);
    try ee(True.immediate_class(),class.True_I);
    try ee(False.immediate_class(),class.False_I);
    try ee(symbol.symbols.yourself.immediate_class(),class.Symbol_I);
    
}
test "printing" {
    var buf: [255]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const stream = fbs.writer();
    try stream.print("{}\n",.{Object.from(42)});
    try stream.print("{}\n",.{symbol.symbols.yourself});
    try std.testing.expectEqualSlices(u8, "42\n#yourself\n", fbs.getWritten());
}
