const std = @import("std");
const math = std.math;
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;
const rotl = std.math.rotl;
const rotr = std.math.rotr;

const EXPONENT_MASK: u64 = 0x7FF0000000000000;
const MANTISSA_MASK: u64 = 0x000FFFFFFFFFFFFF;
const EXPONENT_BIAS: u16 = 896; // exp8 = (exp11 - 1023) + 127 = exp11 - 896

const TAG = 0b100; // immediate float tag
const SHIFTED_EXP: u64 = 0xFFE0_0000_0000_0000;
const SIGN_MASK: u64 = 0x8000_0000_0000_0000;
const ZERO_SHAPE: u64 = 0x8000_0000_0000_0008;

pub const encode = encode_n; // used by the spur.zig
pub const EncodeError = error{ Unencodable, PosInf, NegInf, NaN };

// immediate float layout: [exp8(8)][mant(52)][sign(1)][tag(3)]
// Ref: https://clementbera.wordpress.com/2018/11/09/64-bits-immediate-floats/

pub inline fn encode_n(v: f64) !u64 {
    const bits: u64 = @bitCast(v);
    const y = rotr(u64, rotl(u64, bits, 5) +% 1, 1);
    if ((y & 0x7) == TAG and (y | 0x8) != 0xC) return y;
    if (bits & 0x7FFFFFFF_FFFFFFFF == 0) return 4 + ((bits >> 61) & 8);
    return error.Unencodable;
}

inline fn encode_dave(v: f64) EncodeError!u64 {
    const bits: u64 = @bitCast(v);
    var y = std.math.rotl(u64, bits, 5);
    if (y <= 0x10) { // probably 0
        if (y == 0) return 4;
        if (y == 0x10) return 0xc;
        y +%= 1;
        y = std.math.rotr(u64, y, 1);
        // handle normal immediate float; with zero-collision check
        if ((y & 0x7) == TAG and (y | 0x8) != 0xC) return y;
        // if ((y & 0x7) == TAG and (y & 0xffff_ffff_ffff_ffff) != 0) return y;
        // if ((y & 0x7) == TAG and (y >> 4) != 0) return y;
        return error.Unencodable;
    }
    y +%= 1;
    y = std.math.rotr(u64, y, 1);

    // handle normal immediate float
    if ((y & 0x7) == TAG) return y;
    return error.Unencodable;
}

inline fn encode_spec(v: f64) EncodeError!u64 {
    const bits: u64 = @bitCast(v);
    var y = std.math.rotl(u64, bits, 5);
    y +%= 1;
    y = std.math.rotr(u64, y, 1);

    // handle normal immediate float; with zero-collision check
    if ((y & 0x7) == TAG) return if ((y | 0x8) == 0xC) return error.Unencodable else y;
    // handle zero
    if ((y | 0x8) == ZERO_SHAPE) return ((bits >> 60) & 0x8) | TAG;
    // handle Inf
    const shifted = (bits << 1);
    if (shifted == SHIFTED_EXP) return if ((bits & SIGN_MASK) != 0) error.NegInf else error.PosInf;
    // handle NaN
    if (shifted > SHIFTED_EXP) return error.NaN;

    return error.Unencodable; // unencodable (subnormal vs out-of-range)
}

inline fn encode_check(value: f64) !u64 {
    const bits: u64 = @bitCast(value);
    const exp: u64 = (bits & EXPONENT_MASK);
    const mant = bits & MANTISSA_MASK;

    // Handle zero / subnormal / special
    if (exp == 0) {
        return if (mant == 0) (((bits >> 60) & 0x8) | TAG) else error.Unencodable;
    }

    // Handle IEEE special values (Inf/NaN)
    if (exp == EXPONENT_MASK) {
        if (mant == 0) {
            return if ((bits & SIGN_MASK) != 0) EncodeError.NegInf else EncodeError.PosInf;
        }

        return EncodeError.NaN;
    }

    const e11: u32 = @intCast(exp >> 52);

    // Handle Integer underflow
    if (e11 < EXPONENT_BIAS) return error.Unencodable;

    const exp8: u32 = e11 - EXPONENT_BIAS;

    // Handle out-of-range exponents
    if (exp8 > 255) return error.Unencodable;

    // Handle Zero-Collision Case
    if (exp8 == 0 and mant == 0) return error.Unencodable;

    var r = std.math.rotl(u64, bits, 5);
    r +%= 1;
    return std.math.rotr(u64, r, 1);
}

pub inline fn decode(self: u64) f64 {
    if (self <= 0xC) {
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

const smallest: f64 = @bitCast(@as(u64, 0x3800_0000_0000_0001));
const largest: f64 = @bitCast(@as(u64, 0x47FF_FFFF_FFFF_FFFF));
const tooSmall: f64 = @bitCast(@as(u64, 0x3800_0000_0000_0000));
const tooLarge: f64 = @bitCast(@as(u64, 0x4800_0000_0000_0000));

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
    try expectEqual(error.Unencodable, encode(tooSmall));
    try expectEqual(error.Unencodable, encode(tooLarge));
    try expectEqual(error.Unencodable, encode(math.nan(f64)));
    try expectEqual(error.Unencodable, encode(math.inf(f64)));
    try expectEqual(error.Unencodable, encode(-math.inf(f64)));
}

const iterations_spur = 100000000;

const valid_values = [_]f64{ 0.0, -0.0 } ** 1 ++
    [_]f64{ 1.0, -1.0, math.pi, 42.0, -3.14159, 100.0, -100.0 } ** 16 ++
    [_]f64{ smallest, largest } ** 16;

const iterations_v = iterations_spur / valid_values.len;
const invalid_values =
    [_]f64{tooSmall} ** 1 ++
    [_]f64{tooLarge} ** 1 ++
    [_]f64{math.nan(f64)} ** 1 ++
    [_]f64{math.inf(f64)} ** 1 ++
    [_]f64{-math.inf(f64)} ** 1;

const iterations_i = iterations_spur / invalid_values.len;
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
    0xfffffffffffffff4, // encoded largest negative value
};

const iterations_d = iterations_spur / decode_values.len;
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
            _ = encode(val) catch continue;
        }
    }
}
pub fn decode_valid(iterations: u64) void {
    for (0..iterations / decode_values.len) |_| {
        for (decode_values) |val| {
            _ = decode(val);
        }
    }
}
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
            _ = encode_dave(val) catch return;
        }
    }
    const dave_valid_time = timer.lap();
    for (0..iterations_i) |_| {
        for (invalid_values) |val| {
            _ = encode_dave(val) catch continue;
        }
    }
    const dave_invalid_time = timer.lap();
    std.debug.print("dave time: {}ns {}ns\n", .{ dave_valid_time, dave_invalid_time });

    _ = timer.lap();
    for (0..iterations_v) |_| {
        for (valid_values) |val| {
            _ = encode_spec(val) catch return;
        }
    }
    const spec_valid_time = timer.lap();
    for (0..iterations_i) |_| {
        for (invalid_values) |val| {
            _ = encode_spec(val) catch continue;
        }
    }
    const spec_invalid_time = timer.lap();
    std.debug.print("Spec time: {}ns {}ns\n", .{ spec_valid_time, spec_invalid_time });

    _ = timer.lap();
    for (0..iterations_v) |_| {
        for (valid_values) |val| {
            _ = encode_n(val) catch return;
        }
    }
    const check_valid_time = timer.lap();
    for (0..iterations_i) |_| {
        for (invalid_values) |val| {
            _ = encode_n(val) catch continue;
        }
    }
    const check_invalid_time = timer.lap();

    std.debug.print("Foo time: {}ns {}ns\n", .{ check_valid_time, check_invalid_time });

    std.debug.print("Dave is {d:.2}x {d:.2}x faster than Foo\n", .{ delta(dave_valid_time, check_valid_time), delta(dave_invalid_time, check_invalid_time) });
    std.debug.print("Spec is {d:.2}x {d:.2}x faster than Foo\n", .{ delta(spec_valid_time, check_valid_time), delta(spec_invalid_time, check_invalid_time) });
    std.debug.print("Dave is {d:.2}x {d:.2}x faster than Spec\n", .{ delta(dave_valid_time, spec_valid_time), delta(dave_invalid_time, spec_invalid_time) });
}

fn delta(spec: u64, check: u64) f64 {
    return @as(f64, @floatFromInt(check)) / @as(f64, @floatFromInt(spec));
}
