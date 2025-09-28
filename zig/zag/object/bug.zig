const std = @import("std");
const math = std.math;
// zig run -Doptimize=ReleaseFast bug.zig
const z = struct {
    pub inline fn decode(self: u64) f64 {
        return @bitCast(math.rotr(u64, self - 2, 4));
    }
    const decode_values = [_]u64{
        0x0000000000000002, // encoded +0.0
        0x000000000000000a, // encoded -0.0
        0xff00000000000005, // encoded +1.0
        0xff0000000000000d, // encoded -1.0
        0x00921fb54442d186, // encoded +pi
        0x0450000000000006, // encoded +42.0
        0x00921f9f01b866ee, // encoded -3.14159
        0x0590000000000006, // encoded +100.0
        0x059000000000000e, // encoded -100.0
        0x0000000000000012, // encoded +smallest
        0xfffffffffffffff7, // encoded -largest
        0x00921fb54442d186, // repetition
        0x0450000000000006,
        0x00921f9f01b866ee,
        0x0590000000000006,
        0x059000000000000e,
        0x0000000000000012,
        0xfffffffffffffff7,
    };
    pub fn decode_valid(iterations: u64) void {
        for (0..iterations / decode_values.len) |_| {
            for (decode_values) |val| {
                _ = decode(val);
            }
        }
    }
};
const s = struct {
    pub inline fn decode(self: u64) f64 {
        if (self <= 0xC) {@branchHint(.unlikely);
            if (self == 4) {
                return 0.0;
            } else if (self == 0xC) {
                return -0.0;
            }
        }
        var r = std.math.rotl(u64, self, 1);
        r -%= 1;
        r = std.math.rotr(u64, r, 5);
        return @bitCast(r);
    }
    const decode_values = [_]u64{
        0x0000000000000004, // encoded +0.0
        0x000000000000000c, // encoded -0.0
        0x7f00000000000004, // encoded 1.0
        0x7f0000000000000c, // encoded -1.0
        0x80921fb54442d184, // encoded π (pi)
        0x8450000000000004, // encoded 42.0
        0x80921f9f01b866ec, // encoded -3.14159
        0x8590000000000004, // encoded 100.0
        0x859000000000000c, // encoded -100.0
        0x0000000000000014, // encoded smallest positive value
        0xfffffffffffffff4, // encoded largest positive value
        0x7f000c0000000004, // repetition of valid values
        0x7f000c000000000c,
        0x80921cb54442d184,
        0x84500c0000000004,
        0x80921c9f01b866ec,
        0x85900c0000000004,
        0x85900c000000000c,
        0xfffffcfffffffff4,
    };
    pub fn decode_valid(iterations: u64) void {
        for (0..iterations / decode_values.len) |_| {
            for (decode_values) |val| {
                _ = decode(val);
            }
        }
    }
};
pub fn main() void {
    const iterations = 100_000_000;
    const ns = 1.0 / @as(f64, @floatFromInt(iterations));
    var timer = std.time.Timer.start() catch unreachable;

    _ = timer.lap();
    z.decode_valid(iterations);
    const z_decode_time = timer.lap();
    std.debug.print("z time: {d:.3}ns\n", .{ @as(f64, @floatFromInt(z_decode_time))*ns });

    _ = timer.lap();
    s.decode_valid(iterations);
    const s_decode_time = timer.lap();
    std.debug.print("Spur time: {d:.3}ns\n", .{ @as(f64, @floatFromInt(s_decode_time))*ns });

}
