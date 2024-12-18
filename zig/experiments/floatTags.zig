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
    UndefinedObject = 23,
    Float,
    Object,
    BlockClosure,
    BlockClosureValue,
    Context,
    Array,
    String,
    Utf8String,
    _,
};
pub const Character = extern union {
    c: u8,
    _ignored: Object,
    fn encoded(self : @This()) Object {
        return .{.tag = .immediates, .classIndex = .Character, .hash = self.c};
    }
};
pub const Object = packed struct {
    tag: Group,
    classIndex: ClassIndex,
    hash: u56,
    pub const Group = enum(u3) { heap=0, immediates, float2, float3, float4, float5, float6, float7};
    pub fn immediate_class(self: Object) ClassIndex {
        const selfU = self.u();
        if (selfU&6>0) return .Float;
        if (selfU&7>0) return self.classIndex;
        if (selfU==0) return .UndefinedObject;
        return .Object;
    }
    pub inline fn u(self: Object) u64 {
        return @bitCast(self);
    }
    const Nil: Object = @bitCast(@as(u64,0));
    const False: Object = @bitCast(@as(u64,0x51));
    const True: Object = @bitCast(@as(u64,0x59));
    pub inline fn from(value: anytype) Object {
        const T = @TypeOf(value);
        if (T == Object) return value;
        if (T == Character) return value.encoded();
        if (T == bool) return if (value) True else False;
        switch (@typeInfo(T)) {
            .int, .comptime_int => return @bitCast(@as(i64, value)*256+0x61),
            .float, .comptime_float => return @bitCast(encode(value)),
            .null => return Nil,
            .pointer => |ptr_info| {
                switch (ptr_info.size) {
                    .One => {
                        switch (@typeInfo(ptr_info.child)) {
                            .array => |array_info| {
                                if (array_info.child == u8)
                                    return shortStringEncode(value[0..]);
                            },
                            else =>
                                return @bitCast(@intFromPtr(value)),
                        }
                    },
                    else => {},
                }
            },
            else => {},
        }
        @compileError("Can't convert \"" ++ @typeName(T) ++ "\"");
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
            .ShortString => shortStringPrint(self,writer),
            .SmallInteger => writer.print("{d}", .{@as(i64,@bitCast(selfU))>>8}),
            .Float => writer.print("{}", .{ decode(selfU) }),
            else => {
                try writer.print("0x{x:0>16}", .{selfU});
                try writer.print(" - format for unknown class",.{});
            },
        };
        if (fmt.len == 1 and fmt[0] == 'x') try writer.print("(0x{x:>16})", .{selfU});
    }
    fn shortStringPrint(self: Object, writer: anytype) !void {
        try writer.print("'",.{});
        var chars: u64 = self.hash;
        while (chars>0) {
            try writer.print("{c}", .{@as(u8,@truncate(chars))});
            chars = chars>>8;
        }
        try writer.print("'",.{});
    }
    fn shortStringEncode(str: [] const u8) Object {
        var chars: u64 = 0;
      //  if (str.len>7) @compileError("ShortString limited to 7 chars");
        for (str,0..) |char,index|
            chars = chars + (@as(u64,char) << @truncate(index*8));
        return .{.tag = .immediates, .classIndex = .ShortString, .hash = @truncate(chars)};
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
        .comptime_int => @bitCast(@as(i64,value)),
        .comptime_float => cvtU64(@as(f64,value)),
        .bool => @intFromBool(value),
        .null =>  0,
        .pointer => @intFromPtr(value),
        else => @bitCast(value),
    };
}
pub fn main() !void {
    const xMin: f64 = @bitCast(@as(u64,0x0000_0000_0000_0001));
    const xSmall: f64 = @bitCast(@as(u64,0x2fff_ffff_ffff_ffff));
    const xSmall2: f64 = @bitCast(@as(u64,0x3000_0000_0000_0000));
    const xSpurMax: f64 = @bitCast(@as(u64,0x4fff_ffff_ffff_ffff));
    const xBig: f64 = @bitCast(@as(u64,0x5fff_ffff_ffff_ffff));
    const xMax: f64 = @bitCast(@as(u64,0x7fef_ffff_ffff_ffff));
    const xInf: f64 = @bitCast(@as(u64,0x7ff0_0000_0000_0000));
    const xNaN: f64 = @bitCast(@as(u64,0x7fff_ffff_ffff_ffff));
    const data = .{
//        &xBig, &main,
        null, false, true, 42, -0x40_0000_0000_0000, 0x3f_ffff_ffff_ffff,
        Character{.c='a'},
        "abc",
        0.0,-0.0,
        xMin,
        pow(f64,2.0,-767),pow(f64,2.0,-511),
        xSmall,xSmall2,
        0.5, 0.75, 1.0, -1.0, 2.0,
        xSpurMax,xBig,
        pow(f64,2.0,513),pow(f64,2.0,769),
        xMax, -xMax,
        xInf, -xInf,
        xNaN, -xNaN,
    };
    const print_x = false;
    inline for (data) |x| {
        const u = Object.from(x);
        if (u.immediate_class()==.Object) {
            // if (@TypeOf(x)=
            // std.debug.print("{x:0>16} {x:0>16} {} coded as Object {s}\n",.{cvtU64(x),u.u(),x, switch (u.u()>>3) {
            //     else => "",
            //     2 => "NaN",
            //     3 => "+inf",
            //     4 => "-inf",
            // }});
        } else if (print_x) {
            std.debug.print("{x:0>16} {x:0>16} {} {} {}\n",.{cvtU64(x),u.u(),x,u,u.immediate_class()});
        } else  {
            std.debug.print("{x:0>16} {x:0>16} {} {}\n",.{cvtU64(x),u.u(),u,u.immediate_class()});
        }
    }
    for (0..255) |bits| {
        const sign_exponent = @as(u64,bits & 0xc0) << 56;
        const exponent_mantissa = @as(u64,@bitCast(@as(i64,@bitCast(@as(u64,bits)<<58)) >> 6))>>2;
        const u = sign_exponent | exponent_mantissa;
        const x:f64 = @bitCast(u);
        std.debug.print("{:>3} {x:0>16} {x:0>16} {x:0>16} ",.{bits,  sign_exponent, exponent_mantissa, cvtU64(x)});
        if (@abs(x)<0.0001 or @abs(x)>1000.0) {
            std.debug.print("{e:10.5}\n",.{x});
        } else std.debug.print("{d:11.7}\n",.{x});
    }
}
