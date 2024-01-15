const std = @import("std");
const rotr = std.math.rotr;
const rotl = std.math.rotl;
const pow = std.math.pow;
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
    _,
};
pub const Object = packed struct {
    u: u64,
    pub fn immediate_class(self: Object) ClassIndex {
        switch (self.u&7) {
            0 => return .Object,
            1 => return .SmallInteger,
            2 => return @enumFromInt((self.u>>4)&0xffff),
            else => return .Float,
        }
    }
    const Nil: Object = @bitCast(@as(u64,0x000032));
    const False: Object = @bitCast(@as(u64,0x000042));
    const True: Object = @bitCast(@as(u64,0x100052));
    pub inline fn from(value: anytype) Object {
        const T = @TypeOf(value);
        if (T == Object) return value;
        switch (@typeInfo(@TypeOf(value))) {
            .Int, .ComptimeInt => return @bitCast(@as(u64, value)*8+1),
            .Float, .ComptimeFloat => {
                const u = rotl(u64,@as(u64,@bitCast(@as(f64,value))),4)+%3;
                return @bitCast(if (u&7>=3) u else 8);
            },
            .Bool => return if (value) True else False,
            .Null => return Nil,
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
        @compileError("Can't convert \"" ++ @typeName(@TypeOf(value)) ++ "\"");
    }
    pub fn format(
        self: Object,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = options;

        try switch (self.immediate_class()) {
            .Object => if (self.u==8) writer.print("float object", .{}) else writer.print("object:0x{x:0>16}", .{self.u}),
            .False => writer.print("false", .{}),
            .True => writer.print("true", .{}),
            .UndefinedObject => writer.print("nil", .{}),
            .Symbol => writer.print("#symbols.i_{}", .{self.u>>20}),
            .Character => writer.print("${c}", .{@as(u8,@truncate(self.u>>20))}),
            .SmallInteger => writer.print("{d}", .{@as(i64,@bitCast(self.u>>1))}),
            .Float => writer.print("{}", .{ decode(self.u) }),
            else => {
                try writer.print("0x{x:0>16}", .{self.u});
                @panic("format for unknown class");
            },
        };
        if (fmt.len == 1 and fmt[0] == 'x') try writer.print("(0x{x:>16})", .{self.u});
    }
};
fn decode(x: u64) f64 {
    if (x&7>=3) return @bitCast(rotr(u64,x-3,4));
    return 0;
}
fn cvtU64(value: anytype) u64 {
    return switch (@typeInfo(@TypeOf(value))) {
        .ComptimeInt => @as(u64,value),
        .ComptimeFloat => cvtU64(@as(f64,value)),
        .Bool => @intFromBool(value),
        .Null =>  0,
        .Pointer => @intFromPtr(value),
        else => @bitCast(value),
    };
}
pub fn main() !void {
    const xBig: f64 = @bitCast(@as(u64,0x4fff_ffff_ffff_ffff));
    const xMax: f64 = @bitCast(@as(u64,0x7fef_ffff_ffff_ffff));
    const xInf: f64 = @bitCast(@as(u64,0x7ff0_0000_0000_0000));
    const xNaN: f64 = @bitCast(@as(u64,0x7fff_ffff_ffff_ffff));
    const data = .{
        &xBig, 42, null, false, true,
        0.0,-0.0,
        pow(f64,2.0,-1022),pow(f64,2.0,-600),pow(f64,2.0,-400),pow(f64,2.0,-40),
        0.5, 0.75, -1.0, 1.0, 2.0, 16.0, pow(f64,2.0,40),pow(f64,2.0,159),pow(f64,2.0,256),
        xBig, -xBig,
        pow(f64,2.0,257),pow(f64,2.0,600),pow(f64,2.0,800),
        xMax, -xMax,
        xInf, -xInf,
        xNaN, -xNaN,
    };
    inline for (data) |x| {
        std.debug.print("{x:0>16} {x:0>16} {} {} {}\n",.{cvtU64(x),Object.from(x).u,x,Object.from(x),Object.from(x).immediate_class()});
    }
}
