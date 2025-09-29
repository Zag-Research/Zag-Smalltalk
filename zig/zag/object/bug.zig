const std = @import("std");
const math = std.math;
// zig run -Doptimize=ReleaseFast bug.zig
pub noinline fn decodez(self: u64) f64 {
    return @bitCast(math.rotr(u64, self - 2, 4));
}
pub noinline fn decodes(self: u64) f64 {
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
fn decode_valid(iterations: u64, decoder: *const fn (u64) f64) f64 {
    var result: f64 = 0.0;
    for (0..iterations / decode_values.len) |_| {
        inline for (decode_values) |val| {
            result += decoder(val);
        }
    }
    return result;
}
pub fn main() void {
    const iterations = 10_000_000_000;
    const ns = 1.0 / @as(f64, @floatFromInt(iterations));
    var timer = std.time.Timer.start() catch unreachable;

    _ = timer.lap();
    std.mem.doNotOptimizeAway(decode_valid(iterations, &decodez));
    const z_decode_time = timer.lap();
    std.debug.print("z time: {d:.3}ns\n", .{ @as(f64, @floatFromInt(z_decode_time))*ns });

    _ = timer.lap();
    std.mem.doNotOptimizeAway(decode_valid(iterations, &decodes));
    const s_decode_time = timer.lap();
    std.debug.print("s time: {d:.3}ns\n", .{ @as(f64, @floatFromInt(s_decode_time))*ns });
}
