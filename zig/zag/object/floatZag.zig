const std = @import("std");
const math = std.math;
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;

pub inline fn encode(x: f64) !u64 {
    const u = math.rotl(u64, @bitCast(x), 4) + 2;
    if (u & 6 != 0) {
        @branchHint(.likely);
        return u;
    }
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
const valid_values = [_]f64{0.0, -0.0} ** 1 ++
    [_]f64{ 1.0, -1.0, math.pi, 42.0, -3.14159, 100.0, -100.0 } ** 16 ++
    [_]f64{ smallest, largest } ** 16;
const invalid_values =
    [_]f64{tooLarge} ** 1 ++
    [_]f64{math.nan(f64)} ** 1 ++
    [_]f64{math.inf(f64)} ** 1 ++
    [_]f64{-math.inf(f64)} ** 1;
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
pub fn encode_valid(iterations: u64) void {
    for (0..iterations / valid_values.len) |_| {
        for (valid_values) |val| {
            _ = encode(val) catch return;
        }
    }
}
pub fn encode_invalid(iterations: u64) void {
    for (0..iterations / invalid_values.len) |_| {
        for (invalid_values) |val| {
            std.mem.doNotOptimizeAway(encode(val) catch 0);
        }
    }
}
pub fn decode_valid(iterations: u64) void {
    for (0..iterations / decode_values.len) |_| {
        for (decode_values) |val| {
            std.mem.doNotOptimizeAway(decode(val));
        }
    }
}

// zig run -Doptimize=ReleaseFast floatSpur.zig
pub fn main() void {
    const iterations = 100_000_000;
    const ns = 1.0 / @as(f64, @floatFromInt(iterations));

    if (false) {
        for (valid_values) |val| {
            std.log.err("0x{x:0>16},\n", .{encode(val) catch unreachable});
        }
    }

    // Benchmark encode_spec
    var timer = std.time.Timer.start() catch unreachable;

    _ = timer.lap();
    encode_valid(iterations);
    const valid_time = timer.lap();
    encode_invalid(iterations);
    const invalid_time = timer.lap();
    decode_valid(iterations);
    const decode_time = timer.lap();
    std.log.err("encode time: {d:.3}s {d:.3}s {d:.3}s\n", .{ @as(f64, @floatFromInt(valid_time))*ns, @as(f64, @floatFromInt(invalid_time))*ns, @as(f64, @floatFromInt(decode_time))*ns });
}

fn delta(spec: u64, check: u64) f64 {
    return @as(f64, @floatFromInt(check)) / @as(f64, @floatFromInt(spec));
}
