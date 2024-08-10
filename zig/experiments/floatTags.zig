// Modified Spur encoding

const std = @import("std");
const math = std.math;
const rotr = math.rotr;
const rotl = math.rotl;
const pow = math.pow;
pub const ClassIndex = enum(u5) {
    none = 0,
    ThunkHeap,
    ThunkReturnLocal,
    ThunkReturnSmallInteger,
    ThunkReturnImmediate,
    ThunkReturnCharacter,
    UndefinedObject,
    True,
    False,
    SmallInteger,
    Symbol,
    Character,
    ThunkImmediate,
    ThunkFloat,
    Float,
    Object,
    _,
};
pub const Object = packed struct {
    tag: Group,
    classIndex: ClassIndex,
    hash: u56,
    pub const Group = enum(u3) { heap=0, immediates, float2, float3, float4, float5, float6, float7};
    pub fn immediate_class(self: Object) ClassIndex {
        const selfU = self.u();
        if (selfU&6>0) return .Float;
        if (selfU&1>0) return self.classIndex;
        if (selfU==0) return .UndefinedObject;
        return .Object;
    }
    pub inline fn u(self: Object) u64 {
        return @bitCast(self);
    }
    const Nil: Object = @bitCast(@as(u64,0));
    const False: Object = @bitCast(@as(u64,0x41));
    const True: Object = @bitCast(@as(u64,0x39));
    pub inline fn from(value: anytype) Object {
        const T = @TypeOf(value);
        if (T == Object) return value;
        switch (@typeInfo(@TypeOf(value))) {
            .Int, .ComptimeInt => return @bitCast(@as(u64, value)*256+(if (value<255) 0x59 else 0x49)),
            .Float, .ComptimeFloat => return @bitCast(encode(value)),
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
        const selfU = self.u();
        try switch (self.immediate_class()) {
            .Object => if (selfU==8) writer.print("float object", .{}) else writer.print("object:0x{x:0>16}", .{selfU}),
            .False => writer.print("false", .{}),
            .True => writer.print("true", .{}),
            .UndefinedObject => writer.print("nil", .{}),
            .Symbol => writer.print("#symbols.i_{}", .{self.hash}),
            .Character => writer.print("${c}", .{@as(u8,@truncate(self.hash))}),
            .SmallInteger => writer.print("{d}", .{@as(i64,@bitCast(selfU>>8))}),
            .Float => writer.print("{}", .{ decode(selfU) }),
            else => {
                try writer.print("0x{x:0>16}", .{selfU});
                try writer.print(" - format for unknown class",.{});
            },
        };
        if (fmt.len == 1 and fmt[0] == 'x') try writer.print("(0x{x:>16})", .{selfU});
    }
};
fn encode(x: f64) u64 {
    const u = rotl(u64,@bitCast(x),4)+%2;
    if (u&6>0) return u;
    if (math.isNan(x)) return 16;
    if (math.inf(f64)==x) return 24;
    if (math.inf(f64)==-x) return 32;
    return 8;
}
fn decode(x: u64) f64 {
    if (x&7<2) return 0;
    return @bitCast(rotr(u64,x-2,4));
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
    const xMin: f64 = @bitCast(@as(u64,0x0000_0000_0000_0001));
    const xSmall: f64 = @bitCast(@as(u64,0x2fff_ffff_ffff_ffff));
    const xSmall2: f64 = @bitCast(@as(u64,0x3000_0000_0000_0000));
    const xBig: f64 = @bitCast(@as(u64,0x5fff_ffff_ffff_ffff));
    const xMax: f64 = @bitCast(@as(u64,0x7fef_ffff_ffff_ffff));
    const xInf: f64 = @bitCast(@as(u64,0x7ff0_0000_0000_0000));
    const xNaN: f64 = @bitCast(@as(u64,0x7fff_ffff_ffff_ffff));
    const data = .{
        &xBig, 300, null, false, true, 42, 0x7_ffff_ffff_ffff, 0x7f_ffff_ffff_ffff, 'a',
        0.0,-0.0,
        xMin,
        pow(f64,2.0,-767),pow(f64,2.0,-511),
        xSmall,xSmall2,
        0.5, 0.75, 1.0, -1.0, 2.0,
        xBig,
        pow(f64,2.0,513),pow(f64,2.0,769),
        xMax, -xMax,
        xInf, -xInf,
        xNaN, -xNaN,
    };
    inline for (data) |x| {
        const u = Object.from(x);
        // std.debug.print("{} {} {}\n\t",.{u.tag,u.classIndex,u.hash});
        if (u.immediate_class()==.Object) {
            std.debug.print("{x:0>16} {x:0>16} {} coded as Object {s}\n",.{cvtU64(x),u.u(),x, switch (u.u()>>3) {
                else => "",
                2 => "NaN",
                3 => "+inf",
                4 => "-inf",
            }});
        } else
            std.debug.print("{x:0>16} {x:0>16} {} {} {}\n",.{cvtU64(x),u.u(),x,u,u.immediate_class()});
    }
}
