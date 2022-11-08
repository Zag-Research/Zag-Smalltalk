const std = @import("std");
const mem = std.mem;
const builtin = @import("builtin");
const native_endian = builtin.target.cpu.arch.endian();
const symbol = @import("symbol.zig");
const heap = @import("heap.zig");
const HeapPtr = heap.HeapPtr;
const HeapConstPtr = heap.HeapConstPtr;
const Thread = @import("thread.zig");
//const Dispatch = @import("dispatch.zig");
//const Context = Dispatch.Context;
const Code = @import("execute.zig").Code;
const class = @import("class.zig");
const ClassIndex = class.ClassIndex;
inline fn of(comptime v: u64) Object {
    return @bitCast(Object,v);
}
inline fn o2(c: ClassIndex, comptime h: comptime_int) u64 {
    return ((@as(u64,0xfff7)<<16)+c<<32)+h;
}
pub const ZERO              = of(0);
const Negative_Infinity: u64     =    0xfff0000000000000;
// unused NaN fff00-fff4f
const Start_of_Code_References: u64 = 0xfff4_000000000000;
const End_of_Code_References: u64   = 0xfff4_ffffffffffff;
const Start_of_Pointer_Objects: u64 = 0xfff5_000000000000;
const Start_of_Heap_Objects: u64 =    0xfff6_000000000000;
const End_of_Heap_Objects: u64   =    0xfff6_ffffffffffff;
const c2Base = o2(0,0);
pub const False             = of(o2(class.False_I,0x00010000));
pub const True              = of(o2(class.True_I,0x00100001));
pub const Nil               = of(o2(class.UndefinedObject_I,0x01000002));
pub const NotAnObject       = of(o2(class.UndefinedObject_I,0x01000003)); // never a valid object... should never be visible to managed language
const Symbol_Base           =    o2(class.Symbol_I,0);
const Character_Base        =    o2(class.Character_I,0);
pub const u64_MINVAL            =    0xfff8_000000000000;
const u64_ZERO              =    0xfffc_000000000000;
const u64_MAXVAL            =    0xffff_ffffffffffff;
pub const MinSmallInteger = of(u64_MINVAL).to(i64); // anything smaller than this will underflow
pub const MaxSmallInteger = of(u64_MAXVAL).to(i64); // anything larger than this will overflow

pub fn fromLE(comptime T: type, v: T) Object {
    const val = @ptrCast(*const [@sizeOf(T)]u8,&v);
    return @bitCast(Object,mem.readIntLittle(T,val));
}
pub const compareObject = objectMethods.compare;
const objectMethods = struct {
    pub inline fn hash(self: Object) Object {
        return @bitCast(Object,self.u()|u64_ZERO);
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
    pub inline fn tagbits(self: Object) u16 {
        return @bitCast(u16,@truncate(i16,@bitCast(i64,self)>>48));
    }
    pub inline fn tagbitsL(self: Object) u32 {
        return @bitCast(u32,@truncate(i32,@bitCast(i64,self)>>32));
    }
    pub inline fn equals(self: Object,other: Object) bool {
        return self.u() == other.u();
    }
    pub inline fn isIntA(self: Object) bool {
        return self.u() >= u64_MINVAL;
    }
    pub inline fn isInt(self: Object) bool {
        return self.tagbits() >= u64_MINVAL>>48;
    }
    pub inline fn isDouble(self: Object) bool {
        return self.u() <= Negative_Infinity;
    }
    pub inline fn isBool(self: Object) bool {
        return self.equals(False) or self.equals(True);
    }
    pub inline fn isNilA(self: Object) bool {
        return self.equals(Nil);
    }
    pub inline fn isNil(self: Object) bool {
        return self.tagbitsL() == Nil.tagbitsL();
    }
    pub inline fn isHeapA(self: Object) bool {
        if (self.u() <= Start_of_Heap_Objects) return false;
        return self.u() <= End_of_Heap_Objects;
    }
    pub inline fn isHeap(self: Object) bool {
        return self.tagbits() == Start_of_Heap_Objects>>48;
    }
    pub inline fn is_memory(self: Object) bool {
        if (self.u() <= Start_of_Code_References) return false;
        return self.u() <= End_of_Heap_Objects;
    }
    pub  fn toInt(self: Object) i64 {
        if (self.isInt()) return @bitCast(i64, self.u() -% u64_ZERO);
        @panic("Trying to convert Object to i64");
    }
    pub  fn toWithCheck(self: Object, comptime T:type, comptime check: bool) T {
        switch (T) {
            f64 => {if (check and self.isDouble()) return @bitCast(f64, self);},
            bool=> {if (check and self.isBool()) return self.equals(True);},
            //u8  => {return @intCast(u8, self.hash & 0xff);},
            else => {
                switch (@typeInfo(T)) {
                    .Pointer => |ptrInfo| {
                        if (check and (self.isHeap() and (!@hasDecl(ptrInfo.child,"ClassIndex") or self.to(HeapConstPtr).classIndex==ptrInfo.child.ClassIndex))) {
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
    pub  fn as_string(self: Object) []const u8 {
        return symbol.asString(self).arrayAsSlice(u8);
    }
    pub  fn arrayAsSlice(self: Object, comptime T:type) []T {
        if (self.isHeap()) return self.to(HeapPtr).arrayAsSlice(T);
        return &[0]T{};
    }
    pub inline fn from(value: anytype) Object {
        const T = @TypeOf(value);
        if (T==HeapConstPtr) return @bitCast(Object, @truncate(u48,@ptrToInt(value)) + Start_of_Heap_Objects);
        if (T==[*]Code) return @bitCast(Object, @truncate(u48,@ptrToInt(value)) + Start_of_Code_References);
        switch (@typeInfo(@TypeOf(value))) {
            .Int,
            .ComptimeInt => {
                return @bitCast(Object, @bitCast(u64, @as(i64, value)) +% u64_ZERO);
            },
            .Float,
            .ComptimeFloat => {
                return @bitCast(Object, @as(f64, value));
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
                        return @bitCast(Object, @truncate(u48,@ptrToInt(value)) + Start_of_Heap_Objects);
                    },
                    else => {},
                }
            },
            else => {},
        }
        @compileError("Can't convert \""++@typeName(@TypeOf(value))++"\"");
    }
    pub fn compare(self: Object, other: Object) std.math.Order {
        if (!self.isHeap() or !other.isHeap()) {
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
        if (self.isInt()) return class.SmallInteger_I;
        if (self.isDouble()) return class.Float_I;
        if (self.u() >= c2Base) return @truncate(ClassIndex,self.u() >> 32);
        if (self.u() >= Start_of_Code_References) {
            if (self.u() > End_of_Code_References) return class.Object_I;
            return class.CodeReference_I;
        }
            @panic("unknown immediate");
    }
    pub inline fn get_class(self: Object) ClassIndex {
        const immediate = self.immediate_class();
        if (immediate > 1) return immediate;
        return self.to(HeapPtr).*.getClass();
    }
    pub inline fn promoteTo(self: Object) !Object {
        return heap.GlobalArena.promote(self);
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
            class.Symbol_I => writer.print("#{s}", .{self.as_string()}),
            class.Character_I => writer.print("${c}", .{self.to(u8)}),
            class.SmallInteger_I => writer.print("{d}", .{self.toInt()}),
            class.Float_I => writer.print("{}", .{self.to(f64)}),
            else => { try writer.print("0x{x:>16}", .{self.u()});@panic("format for unknown class");},
        };
    }
    pub const alignment = @alignOf(u64);
    pub fn packedInt(f0: u16, f1: u16, f2: u16) Object {
        return Object{.signMantissa =.SmallInt0, .h0 = f0, .h1 = f1, .l2 = @intToEnum(Level2,f2)};
    }

};
pub const Level2 = enum(u16) { Object = 1, SmallInteger, Float, False, True, UndefinedObject, Symbol, Character, Context, _ };
pub const ClassGrouping = enum(u16) {CodeReference = 0xfff5, Local2, Heap, SmallIntMin, SmallInt0 = 0xfffc, _ };
pub const Object = switch (native_endian) {
    .Big => packed struct {
        signMantissa: ClassGrouping, // align(8),
        l2: Level2,
        h1: u16,
        h0: u16,
        usingnamespace objectMethods;
    },
    .Little => packed struct {
        h0: u16, // align(8),
        h1: u16,
        l2: Level2,
        signMantissa: ClassGrouping,
        usingnamespace objectMethods;
    },
};

test "slicing" {
    const testing = std.testing;
    try testing.expectEqual(Nil.arrayAsSlice(u8).len,0);
}
test "from conversion" {
    const testing = std.testing;
    try testing.expectEqual(@bitCast(u64, Object.packedInt(1,2,3)), 0xfffc000300020001);
    try testing.expectEqual(@bitCast(f64, Object.from(3.14)), 3.14);
    try testing.expectEqual(Object.from(42).u(), u64_ZERO +% 42);
    try testing.expectEqual(Object.from(3.14).immediate_class(),class.Float_I);
    try testing.expect(Object.from(3.14).isDouble());
    try testing.expectEqual(Object.from(3).immediate_class(),class.SmallInteger_I);
    try testing.expect(Object.from(3).isInt());
    try testing.expect(Object.from(false).isBool());
    try testing.expectEqual(Object.from(false).immediate_class(),class.False_I);
    try testing.expectEqual(Object.from(true).immediate_class(),class.True_I);
    try testing.expect(Object.from(true).isBool());
    try testing.expectEqual(Object.from(null).immediate_class(),class.UndefinedObject_I);
    try testing.expect(Object.from(null).isNil());
}
test "to conversion" {
    const testing = std.testing;
    try testing.expectEqual(Object.from(3.14).to(f64), 3.14);
    try testing.expectEqual(Object.from(42).toInt(), 42);
    try testing.expect(Object.from(42).isInt());
    try testing.expectEqual(Object.from(true).to(bool), true);
    try testing.expectEqual(of(u64_MAXVAL).toUnchecked(i64),0x3_ffffffffffff);
    try testing.expectEqual(Object.from(-0x400000).toUnchecked(i64),-0x400000);
}
test "printing" {
    _ = try symbol.init(250,symbol.noStrings);
    var buf: [255]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const stream = fbs.writer();
    try stream.print("{}\n",.{Object.from(42)});
    try stream.print("{}\n",.{symbol.symbols.yourself});
    try std.testing.expectEqualSlices(u8, "42\n#yourself\n", fbs.getWritten());
}
