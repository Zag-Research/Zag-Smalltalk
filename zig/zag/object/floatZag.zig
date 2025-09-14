const std = @import("std");
const math = std.math;
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;

pub inline fn encode(x: f64) !u64 {
    const u = math.rotl(u64, @bitCast(x), 4) + 2;
    if (u & 6 != 0)
        return u;
    return error.Unencodable;
}
pub inline fn decode(self: u64) f64 {
    return @bitCast(math.rotr(u64, self - 2, 4));
}
const smallest: f64 = @bitCast(@as(u64, 0x0000_0000_0000_0001));
const largest: f64 = @bitCast(@as(u64, 0x5FFF_FFFF_FFFF_FFFF));
const tooLarge: f64 = @bitCast(@as(u64, 0x6000_0000_0000_0000));
test "encode/decode" {
    try expectEqual(0.0, decode(try encode(0.0)));
    try expectEqual(-0.0, decode(try encode(-0.0)));
    try expectEqual(0.5, decode(try encode(0.5)));
    try expectEqual(-0.5, decode(try encode(-0.5)));
    try expectEqual(1.0, decode(try encode(1.0)));
    try expectEqual(-1.0, decode(try encode(-1.0)));
    try expectEqual(2.0, decode(try encode(2.0)));
    try expectEqual(-2.0, decode(try encode(-2.0)));
    try expectEqual(math.pi, decode(try encode(math.pi)));
    try expectEqual(smallest, decode(try encode(smallest)));
    try expectEqual(-smallest, decode(try encode(-smallest)));
    try expectEqual(largest, decode(try encode(largest)));
    try expectEqual(-largest, decode(try encode(-largest)));
    try expectEqual(error.Unencodable, encode(tooLarge));
    try expectEqual(error.Unencodable, encode(math.nan(f64)));
    try expectEqual(error.Unencodable, encode(math.inf(f64)));
    try expectEqual(error.Unencodable, encode(-math.inf(f64)));
}
const iterations = 100000000;
const valid_values = [_]f64{0.0, -0.0} ** 1 ++
    [_]f64{ 1.0, -1.0, math.pi, 42.0, -3.14159, 100.0, -100.0 } ** 16 ++
    [_]f64{ smallest, largest } ** 16;
const iterations_v = 100000000 / valid_values.len;
const invalid_values =
    [_]f64{tooLarge} ** 1 ++
    [_]f64{math.nan(f64)} ** 1 ++
    [_]f64{math.inf(f64)} ** 1 ++
    [_]f64{-math.inf(f64)} ** 1;
const iterations_i = 100000000 / invalid_values.len;
const decode_values = [_]u64{
    0x0000000000000002,
    0x000000000000000a,
    0xff00000000000005,
    0xff0000000000000d,
    0x00921fb54442d186,
    0x0450000000000006,
    0x00921f9f01b866ee,
    0x0590000000000006,
    0x059000000000000e,
    0x0000000000000012,
    0xfffffffffffffff7,
};
const iterations_d = 100000000 / decode_values.len;
// zig run -Doptimize=ReleaseFast floatSpur.zig
pub fn main() void {

    if (false) {
        for (valid_values) |val| {
            std.debug.print("0x{x:0>16},\n", .{encode(val) catch unreachable});
        }
    }
    // Benchmark encode_spec
    var timer = std.time.Timer.start() catch unreachable;

    _ = timer.lap();
    for (0..iterations_v) |_| {
        for (valid_values) |val| {
            _ = encode(val) catch return;
        }
    }
    const valid_time = timer.lap();
    for (0..iterations_i) |_| {
        for (invalid_values) |val| {
            _ = encode(val) catch continue;
        }
    }
    const invalid_time = timer.lap();
    for (0..iterations_d) |_| {
        for (decode_values) |val| {
            _ = decode(val);
        }
    }
    const decode_time = timer.lap();
    std.debug.print("zag time: {}ns {}ns {}ns\n", .{ valid_time, invalid_time, decode_time });

    _ = timer.lap();
    const spur = @import("floatSpur.zig");
    spur.encode_valid(iterations);
    const spur_valid_time = timer.lap();
    spur.encode_invalid(iterations);
    const spur_invalid_time = timer.lap();
    spur.decode_valid(iterations);
    const spur_decode_time = timer.lap();
    std.debug.print("Spur time: {}ns {}ns {}ns\n", .{ spur_valid_time, spur_invalid_time, spur_decode_time });

    std.debug.print("Zag is {d:.2}x {d:.2}x {d:.2}x faster\n",
        .{ delta(valid_time, spur_valid_time), delta(invalid_time, spur_invalid_time), delta(decode_time, spur_decode_time) });
}
fn delta(zag: u64, spur: u64) f64 {
    return @as(f64, @floatFromInt(spur)) / @as(f64, @floatFromInt(zag));
}
