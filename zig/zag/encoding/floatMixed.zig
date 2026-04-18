// This is derived from a close reading of
// It captures 94+% of the floating point values in
// It captures all the floating point values from
const std = @import("std");
const math = std.math;
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;

pub inline fn encode(x: f64) !u64 {
    const u = math.rotl(u64, @bitCast(x), 5) -% 3;
    if (u & 6 == 4) {
        @branchHint(.likely);
        return u;
    }
    return error.Unencodable;
}
// It has been verified via godbolt that:
// - this skips generating the conversion code
// export fn t1(x: u64) bool {
//     if (decode(x)) |_| return true;
//     return false;
// }
// - this generates the test code and the conversion code
// export fn t3(x: u64) f64 {
//     if (decode(x)) |y| return y;
//     return 0;
// }
// - this skips generating the test code
// export fn t2(x: u64) f64 {
//     if (decode(x)) |y| return y;
//     unreachable;
// }
pub inline fn decode(self: u64) ?f64 {
    if (self & 6 == 4) {
        @branchHint(.likely);
        return @bitCast(math.rotr(u64, self +% 3, 5));
    }
    return null;
}
const small0: f64 = @bitCast(@as(u64, 0x0000_0000_0000_0001));
const large0: f64 = @bitCast(@as(u64, 0x07ff_ffff_ffff_ffff));
const tooLg0: f64 = @bitCast(@as(u64, 0x0800_0000_0000_0000));
const tooSm1: f64 = @bitCast(@as(u64, 0x37ff_ffff_ffff_ffff));
const small1: f64 = @bitCast(@as(u64, 0x3800_0000_0000_0000));
const large1: f64 = @bitCast(@as(u64, 0x47ff_ffff_ffff_ffff));
const tooLg1: f64 = @bitCast(@as(u64, 0x4800_0000_0000_0000));
const tooSm2: f64 = @bitCast(@as(u64, 0x77ff_ffff_ffff_ffff));
const small2: f64 = @bitCast(@as(u64, 0x7800_0000_0000_0000));
const large2: f64 = @bitCast(@as(u64, 0x7eff_ffff_ffff_ffff));
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
    try expectEqual(small0, decode(try encode(small0)));
    try expectEqual(-small0, decode(try encode(-small0)));
    try expectEqual(large0, decode(try encode(large0)));
    try expectEqual(-large0, decode(try encode(-large0)));
    try expectEqual(small1, decode(try encode(small1)));
    try expectEqual(-small1, decode(try encode(-small1)));
    try expectEqual(large1, decode(try encode(large1)));
    try expectEqual(-large1, decode(try encode(-large1)));
    try expectEqual(small2, decode(try encode(small2)));
    try expectEqual(-small2, decode(try encode(-small2)));
    try expectEqual(large2, decode(try encode(large2)));
    try expectEqual(-large2, decode(try encode(-large2)));
    try expectEqual(error.Unencodable, encode(tooLg0));
    try expectEqual(error.Unencodable, encode(tooSm1));
    try expectEqual(error.Unencodable, encode(tooLg1));
    try expectEqual(error.Unencodable, encode(tooSm2));
    try expect(math.isNan(decode(try encode(math.nan(f64)))));
    try expectEqual(math.inf(f64), decode(try encode(math.inf(f64))));
    try expectEqual(-math.inf(f64), decode(try encode(-math.inf(f64))));
}
const valid_values = [_]f64{0.0, -0.0} ** 1 ++
    [_]f64{ 1.0, -1.0, math.pi, 42.0, -3.14159, 100.0, -100.0 } ** 16 ++
    [_]f64{ small0, large0, small1, large1, small2, large2 } ** 5 ++
    [_]f64{math.nan(f64)} ** 1 ++
    [_]f64{math.inf(f64)} ** 1 ++
    [_]f64{-math.inf(f64)} ** 1;
const invalid_values =
[_]f64{tooLg0} ** 1 ++
[_]f64{tooSm1} ** 1 ++
[_]f64{tooLg1} ** 1 ++
[_]f64{tooSm2} ** 1 ;
const decode_values = [_]u64{
    0xfffffffffffffffd, // +0.0
    0x000000000000000d, // -0.0
    0xfe00000000000004, // +1.0
    0xfe00000000000014, // -1.0
    0x01243f6a8885a305, // +pi
    0x08a0000000000005, // +42.0
    0x01243f3e0370cdd5, // -3.14159
    0x0b20000000000005, // +100.0
    0x0b20000000000015, // -100.0
    0x000000000000001d, // small0
    0xffffffffffffffdd, // large0
    0x0000000000000004, // small1
    0xffffffffffffffe5, // large1
    0x000000000000000c, // small2
    0xdfffffffffffffec, // large2
    0xff0000000000000c, // nan
    0xfe0000000000000c, // +inf
    0xfe0000000000001c, // -inf
};
pub fn encode_valid(iterations: u64) void {
    for (0..iterations / valid_values.len) |_| {
        for (valid_values) |val| {
            _ = encode(val) catch 0;
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
            if (decode(val)) |decoded|
                _ = decoded;
        }
    }
}

// zig run -Doptimize=ReleaseFast floatMixed.zig
pub fn main() void {
    const iterations = 100_000_000;
    const ns = 1.0 / @as(f64, @floatFromInt(iterations));

    if (false) {
        std.debug.print("const decode_values = [_]u64{{\n",.{});
        for (valid_values) |val| {
            std.debug.print("    0x{x:0>16},\n", .{encode(val) catch unreachable});
        }
        std.debug.print("}};\n",.{});
    }

    // Benchmark encode_spec
    var timer = std.time.Timer.start() catch @panic("unreachable");

    _ = timer.lap();
    encode_valid(iterations);
    const valid_time = timer.lap();
    encode_invalid(iterations);
    const invalid_time = timer.lap();
    decode_valid(iterations);
    const decode_time = timer.lap();
    std.debug.print("time encode:{d:.3}s invalid:{d:.3}s decode:{d:.3}s\n", .{ @as(f64, @floatFromInt(valid_time))*ns, @as(f64, @floatFromInt(invalid_time))*ns, @as(f64, @floatFromInt(decode_time))*ns });
}

fn delta(spec: u64, check: u64) f64 {
    return @as(f64, @floatFromInt(check)) / @as(f64, @floatFromInt(spec));
}
