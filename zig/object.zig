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
const class = @import("class.zig");
const ClassIndex = class.ClassIndex;
pub const u32_phi_inverse=2654435769;
inline fn of(comptime v: u64) Object {
    return @bitCast(Object,v);
}
inline fn o2(c: ClassIndex, comptime h: comptime_int) u64 {
    return ((@as(u64,0xfff7)<<16)+c<<32)+h;
}
pub const ZERO              = of(0);
const Negative_Infinity: u64     =    0xfff0000000000000;
// unused NaN fff00-fff4f
const Start_of_Pointer_Objects: u64 = 0xfff5_000000000000;
const Start_of_Heap_Objects: u64 =    0xfff6_000000000000;
const End_of_Heap_Objects: u64   =    0xfff6_ffffffffffff;
const c2Base = o2(0,0);
pub const False             = of(o2(class.False_I,0x00010000));
pub const True              = of(o2(class.True_I,0x00100001));
pub const Nil               = of(o2(class.UndefinedObject_I,0x01000002));
pub const NilFlag           = of(o2(class.UndefinedObject_I,0x01000003));
const Symbol_Base           =    o2(class.Symbol_I,0);
const Character_Base        =    o2(class.Character_I,0);
const u64_MINVAL            =    0xfff8_000000000000;
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
    pub inline fn u(self: Object) u64 {
        return @bitCast(u64,self);
    }
    pub inline fn equals(self: Object,other: Object) bool {
        return self.u() == other.u();
    }
    pub inline fn is_int(self: Object) bool {
        return self.u() >= u64_MINVAL;
    }
    pub inline fn is_double(self: Object) bool {
        return self.u() <= Negative_Infinity;
    }
    pub inline fn is_bool(self: Object) bool {
        return self.equals(False) or self.equals(True);
    }
    pub inline fn is_nil(self: Object) bool {
        return self.equals(Nil);
    }
    pub inline fn is_heap(self: Object) bool {
        if (self.u() <= Start_of_Heap_Objects) return false;
        return self.u() <= End_of_Heap_Objects;
    }
    pub inline fn is_memory(self: Object) bool {
        if (self.u() <= Start_of_Pointer_Objects) return false;
        return self.u() <= End_of_Heap_Objects;
    }
    pub  fn to(self: Object, comptime T:type) T {
        switch (T) {
            i64 => {if (self.is_int()) return @bitCast(i64, self.u() - u64_ZERO);},
            f64 => {if (self.is_double()) return @bitCast(f64, self);},
            bool=> {if (self.is_bool()) return self.equals(True);},
            //u8  => {return @intCast(u8, self.hash & 0xff);},
            else => {
                switch (@typeInfo(T)) {
                    .Pointer => |ptrInfo| {
                        if (self.is_memory() and (!@hasDecl(ptrInfo.child,"ClassIndex") or self.to(HeapConstPtr).classIndex==ptrInfo.child.ClassIndex)) {
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
    pub  fn toUnchecked(self: Object, comptime T:type) T {
        switch (T) {
            i64 => {return @bitCast(i64, self.u() - u64_ZERO);},
            f64 => {return @bitCast(f64, self);},
            bool=> {return self.equals(True);},
            //u8  => {return @intCast(u8, self.hash & 0xff);},
            else => {
                @panic("Trying to convert Object to "++@typeName(T));
            },
        }
    }
    pub  fn as_string(self: Object) []const u8 {
        return symbol.asString(self).arrayAsSlice(u8);
    }
    pub  fn arrayAsSlice(self: Object, comptime T:type) []T {
        if (self.is_memory()) return self.to(HeapPtr).arrayAsSlice(T);
        return &[0]T{};
    }
    pub inline fn from(value: anytype) Object {
        const T = @TypeOf(value);
        if (T==HeapConstPtr) return @bitCast(Object, @truncate(u48,@ptrToInt(value)) + Start_of_Heap_Objects);
//        if (T==*Context or T==*Thread) return @bitCast(Object, Context.headerPtr(value) + Start_of_Pointer_Objects);
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
                        return @bitCast(Object, @ptrToInt(value) + Start_of_Heap_Objects);
                    },
                    else => {},
                }
            },
            else => {},
        }
        @compileError("Can't convert");
    }
    pub fn compare(self: Object, other: Object) std.math.Order {
        if (!self.is_memory() or !other.is_memory()) {
            const u64s = self.u();
            const u64o = other.u();
            return std.math.order(u64s,u64o);
        }
        const ord = std.math.Order;
        if (self.equals(other)) return ord.eq;
        const sla = self.arrayAsSlice(u8);
        const slb = other.arrayAsSlice(u8);
        for (sla[0..@minimum(sla.len,slb.len)]) |va,index| {
            const vb=slb[index];
            if (va<vb) return ord.lt;
            if (va>vb) return ord.gt;
        }
        if (sla.len<slb.len) return ord.lt;
        if (sla.len>slb.len) return ord.gt;
        return ord.eq;
    }
    pub inline fn immediate_class(self: Object) ClassIndex {
        if (self.is_int()) return class.SmallInteger_I;
        if (self.u() >= c2Base) return @truncate(ClassIndex,self.u() >> 32);
        if (self.u() >= Start_of_Pointer_Objects) return class.Object_I;
        if (self.is_double()) return class.Float_I;
        @panic("unknown immediate");
    }
    pub inline fn get_class(self: Object) ClassIndex {
        const immediate = self.immediate_class();
        if (immediate > 1) return immediate;
        return self.to(HeapPtr).*.getClass();
    }
    pub inline fn promoteTo(self: Object, arena: *heap.Arena) !Object {
        return arena.promote(self);
    }
//    pub inline fn send(self: Object, selector: Object, other: Object, cp: *Dispatch.Context) Dispatch.MethodReturns {
//        return Dispatch.call(selector, self, other, cp);
//    }
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
            class.SmallInteger_I => writer.print("{d}", .{self.to(i64)}),
            class.Float_I => writer.print("{}", .{self.to(f64)}),
            else => @panic("format for unknown class"),
        };
    }
    pub const alignment = @alignOf(u64);
};
pub const Tag = enum(u16) { Object = 1, False, True, UndefinedObject, Symbol, Character, Context };
pub const Object = switch (native_endian) {
    .Big => packed struct {
        signMantissa: u16, // align(8),
        tag: Tag,
        hash: u32,
        usingnamespace objectMethods;
    },
    .Little => packed struct {
        hash: u32, // align(8),
        tag: Tag,
        signMantissa: u16,
        usingnamespace objectMethods;
    },
};

test "slicing" {
    const testing = std.testing;
    try testing.expectEqual(Nil.arrayAsSlice(u8).len,0);
}
test "from conversion" {
    const testing = std.testing;
    try testing.expectEqual(@bitCast(f64, Object.from(3.14)), 3.14);
    try testing.expectEqual(Object.from(42).u(), u64_ZERO +% 42);
    try testing.expectEqual(Object.from(3.14).immediate_class(),class.Float_I);
    try testing.expect(Object.from(3.14).is_double());
    try testing.expectEqual(Object.from(3).immediate_class(),class.SmallInteger_I);
    try testing.expect(Object.from(3).is_int());
    try testing.expect(Object.from(false).is_bool());
    try testing.expectEqual(Object.from(false).immediate_class(),class.False_I);
    try testing.expectEqual(Object.from(true).immediate_class(),class.True_I);
    try testing.expect(Object.from(true).is_bool());
    try testing.expectEqual(Object.from(null).immediate_class(),class.UndefinedObject_I);
    try testing.expect(Object.from(null).is_nil());
}
test "to conversion" {
    const testing = std.testing;
    try testing.expectEqual(Object.from(3.14).to(f64), 3.14);
    try testing.expectEqual(Object.from(42).to(i64), 42);
    try testing.expect(Object.from(42).is_int());
    try testing.expectEqual(Object.from(true).to(bool), true);
}
test "printing" {
    var thr = try Thread.Thread.initForTest();
    _ = try symbol.init(&thr,250,"");
    var buf: [255]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const stream = fbs.writer();
    try stream.print("{}\n",.{Object.from(42)});
    try stream.print("{}\n",.{symbol.symbols.yourself});
    try std.testing.expectEqualSlices(u8, "42\n#yourself\n", fbs.getWritten());
}
