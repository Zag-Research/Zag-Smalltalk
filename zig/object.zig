const std = @import("std");
const mem = std.mem;
const builtin = @import("builtin");
const Start_of_Heap_Objects = 0xfff2000000000000;
const End_of_Heap_Objects = 0xfff3ffffffffffff;
pub const False = @bitCast(Object, @as(u64, 0xfff4000000000000));
pub const True = @bitCast(Object, @as(u64, 0xfff6000000000001));
pub const Nil = @bitCast(Object, @as(u64, 0xfff8000000000000));
const u64_MINVAL = 0xfffe000000000000;
const u64_ZERO = 0xffff000000000000;
pub const ZERO = @bitCast(Object, @as(u64, 0));
const native_endian = builtin.target.cpu.arch.endian();
const heap = @import("heap.zig");
const HeapPtr = heap.HeapPtr;
const HeapConstPtr = heap.HeapConstPtr;
const Thread = @import("thread.zig");
const dispatch = @import("dispatch.zig");

pub fn fromLE(v: u64) Object {
    const val = @ptrCast(*const [8]u8,&v);
    return @bitCast(Object,mem.readIntLittle(u64,val));
}
pub const compareObject = objectMethods.compare;
const objectMethods = struct {
    pub inline fn equals(self: Object,other: Object) bool {
        return @bitCast(u64, self) == @bitCast(u64,other);
    }
    pub inline fn is_int(self: Object) bool {
        return @bitCast(u64, self) >= u64_MINVAL;
    }
    pub inline fn is_double(self: Object) bool {
        return @bitCast(u64, self) < Start_of_Heap_Objects;
    }
    pub inline fn is_bool(self: Object) bool {
        return @bitCast(u64,self) == @bitCast(u64,False) or @bitCast(u64,self) == @bitCast(u64,True);
    }
    pub inline fn is_nil(self: Object) bool {
        return @bitCast(u64,self) == @bitCast(u64,Nil);
    }
    pub inline fn is_heap(self: Object) bool {
        if (@bitCast(u64, self) < Start_of_Heap_Objects) return false;
        return @bitCast(u64, self) <= End_of_Heap_Objects;
    }
    pub inline fn to(self: Object, comptime T:type) T {
        switch (T) {
            i64 => {if (self.is_int()) return @bitCast(i64, @bitCast(u64, self) - u64_ZERO);},
            f64 => {if (self.is_double()) return @bitCast(f64, self);},
            bool=> {if (self.is_bool()) return @bitCast(u64, self) == @bitCast(u64, True);},
            //u8  => {return @intCast(u8, self.hash & 0xff);},
            HeapPtr,HeapConstPtr => {if (self.is_heap()) return @intToPtr(T, @bitCast(usize, @bitCast(i64, self) << 16 >> 16));},
            else => {},
        }
        unreachable;
    }
    pub inline fn as_string(self: Object) []const u8 {
        //
        // symbol handling broken
        //
        _ = self;
        return "dummy string";
    }
    pub inline fn arrayAsSlice(self: Object, comptime T:type) []T {
        if (self.is_heap()) return self.to(HeapPtr).arrayAsSlice(T);
        return &[0]T{};
    }
    pub inline fn from(value: anytype) Object {
        const T = @TypeOf(value);
        if (T==HeapConstPtr) return @bitCast(Object, @ptrToInt(value) + Start_of_Heap_Objects);
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
            else => unreachable,
        }
    }
    pub inline fn fullHash(self: Object) u64 {
        return @bitCast(u64, self) % 16777213; // largest 24 bit prime
    }
    pub fn compare(self: Object, other: Object) std.math.Order {
        const ord = std.math.Order;
        if (@bitCast(u64,self)==@bitCast(u64,other)) return ord.eq;
        if (self.immediate_class() != other.immediate_class()) unreachable;
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
    pub inline fn immediate_class(self: Object) u64 {
        if (@bitCast(u64, self) <= Start_of_Heap_Objects) return 8;
        return (@bitCast(u64, self) >> 49) & 7;
    }
    pub inline fn get_class(self: Object) u64 {
        const immediate = self.immediate_class();
        if (immediate > 1) return immediate;
        return self.to(HeapPtr).*.get_class();
    }
    pub inline fn promoteTo(self: Object, arena: *heap.Arena) !Object {
        return arena.promote(self);
    }
    pub inline fn call(self: Object, thread: Thread) dispatch.MethodRetruns {
        return dispatch.call(self,thread);
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
            1 => writer.print("object:0x{x:>16}", .{@bitCast(u64,self)}), //,as_pointer(x));
            2 => writer.print("false", .{}),
            3 => writer.print("true", .{}),
            4 => writer.print("nil", .{}),
            5 => writer.print("#{s}", .{self.as_string()}),
            6 => writer.print("${c}", .{self.to(u8)}),
            7 => writer.print("{d}", .{self.to(i64)}),
            8 => writer.print("{}", .{self.to(f64)}),
            else => unreachable,
        };
    }
    pub const alignment = @alignOf(u64);
};
test "printing" {
    var buf: [255]u8 = undefined;
    var fbs = std.io.fixedBufferStream(&buf);
    const stream = fbs.writer();
    const symbol = @import("symbol.zig");
    try stream.print("{}\n",.{Object.from(42)});
    try stream.print("{}\n",.{symbol.yourself});
    try std.testing.expectEqualSlices(u8, "42\n#dummy string\n", fbs.getWritten());
}
pub const Tag = enum(u3) { Object = 1, False, True, UndefinedObject, Symbol, Character, SmallInteger };
pub const Object = switch (native_endian) {
    .Big => packed struct {
        signMantissa: u12, // align(8),
        tag: Tag,
        highHash: u17,
        nArgs : u8,
        hash: i24,
        usingnamespace objectMethods;
    },
    .Little => packed struct {
        hash: i24, // align(8),
        nArgs : u8,
        highHash: u17,
        tag: Tag,
        signMantissa: u12,
        usingnamespace objectMethods;
    },
};

test "slicing" {
    const testing = std.testing;
    try testing.expectEqual(Nil.arrayAsSlice(u8).len,0);
}
test "from conversion" {
    const expect = std.testing.expect;
    try expect(@bitCast(f64, Object.from(3.14)) == 3.14);
    try expect(@bitCast(u64, Object.from(42)) == u64_ZERO +% 42);
}
test "to conversion" {
    const testing = std.testing;
    try testing.expectEqual(Object.from(3.14).to(f64), 3.14);
    try testing.expectEqual(Object.from(42).to(i64), 42);
    try testing.expect(Object.from(42).is_int());
    try testing.expectEqual(Object.from(true).to(bool), true);
}
