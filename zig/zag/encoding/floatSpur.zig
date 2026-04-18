const std = @import("std");
const builtin = @import("builtin");
const math = std.math;
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;
const rotl = std.math.rotl;
const rotr = std.math.rotr;

const EXPONENT_MASK: u64 = 0x7FF0000000000000;
const MANTISSA_MASK: u64 = 0x000FFFFFFFFFFFFF;
const EXPONENT_OFFS: u64 = 0x7000000000000000;
const EXPONENT_BIAS: u16 = 896; // exp8 = (exp11 - 1023) + 127 = exp11 - 896

const TAG = 0b100; // immediate float tag
const SHIFTED_EXP: u64 = 0xFFE0_0000_0000_0000;
const SIGN_MASK: u64 = 0x8000_0000_0000_0000;
const ZERO_SHAPE: u64 = 0x8000_0000_0000_0008;
const raise_specific_errors = false;

//const encode = encode_spur; // used by spur.zig
//const encode = encode_medium; // used by spur.zig
pub const EncodeError = error{ Unencodable, PosInf, NegInf, NaN };

// immediate float layout: [exp8(8)][mant(52)][sign(1)][tag(3)]
// Ref: https://clementbera.wordpress.com/2018/11/09/64-bits-immediate-floats/

//inline
fn encode_default(v: f64) EncodeError!u64 {
    const bits: u64 = @bitCast(v);
    const y = rotr(u64, rotl(u64, bits, 5) +% 1, 1);
    if ((y & 0x7) == TAG and (y | 0x8) != 0xC) return y;
    if (bits & 0x7FFFFFFF_FFFFFFFF == 0) return TAG + ((bits >> 61) & 8);
    return error.Unencodable;
}

//const debug_print = std.debug.print;
fn debug_print(_: anytype, _: anytype) void {}
//inline
fn encode_medium(value: f64) EncodeError!u64 {
    const bits: u64 = @bitCast(value);
    const rotated = rotl(u64, bits, 5);
    const inc = rotated +% 1;
    debug_print("medium value= {}\n bits=    {b:0>64}\n rotated= {b:0>64}\n inc=     {b:0>64}\n back=    {b:0>64} unencodeable={}\n",
        .{value, bits, rotated, inc, rotr(u64, inc, 1), inc & 14 != TAG << 1 and inc != 1 and inc != 17});
    if (inc & 14 == TAG << 1) {@branchHint(.likely);
        if (inc != TAG << 1) {@branchHint(.likely);
            return rotr(u64, inc, 1);
        }
    }
    if (inc == 1) return 0x4;
    if (inc == 17) return 0xC;
    return error.Unencodable;
}

//inline
fn encode_fast(v: f64) EncodeError!u64 {
    const bits: u64 = @bitCast(v);
    var y = std.math.rotl(u64, bits, 5);
    if (y <= 0x10) { // probably 0
        @branchHint(.unlikely);
        if (y == 0) return TAG; // 0
        if (y == 0x10) return 8 + TAG; // -0
        y = std.math.rotr(u64, y +% 1, 1);
        // handle normal immediate float; with zero-collision check
        if ((y & 0x7) == TAG and (y | 0x8) != 0xC) return y;
        // if ((y & 0x7) == TAG and (y & 0xffff_ffff_ffff_ffff) != 0) return y;
        // if ((y & 0x7) == TAG and (y >> 4) != 0) return y;
    } else {
        @branchHint(.likely);
        y = std.math.rotr(u64, y +% 1, 1);

        // handle normal immediate float
        if ((y & 0x7) == TAG) return y;
    }
    return error.Unencodable;
}

//inline
fn encode_spec(v: f64) EncodeError!u64 {
    const bits: u64 = @bitCast(v);
    var y = std.math.rotl(u64, bits, 5);
    y +%= 1;
    y = std.math.rotr(u64, y, 1);

    // handle normal immediate float; with zero-collision check
    if ((y & 0x7) == TAG) return if ((y | 0x8) == 0xC) return error.Unencodable else y;
    // handle zero
    if ((y | 0x8) == ZERO_SHAPE) return ((bits >> 60) & 0x8) | TAG;
    if (raise_specific_errors) {
        // handle Inf
        const shifted = (bits << 1);
        if (shifted == SHIFTED_EXP) return if ((bits & SIGN_MASK) != 0) error.NegInf else error.PosInf;
        // handle NaN
        if (shifted > SHIFTED_EXP) return error.NaN;
    }
    return error.Unencodable; // unencodable (subnormal vs out-of-range)
}

//inline
fn encode_spur(value: f64) EncodeError!u64 {
    const bits: u64 = @bitCast(value);
    const rotated = rotl(u64, bits, 1);
    if (rotated <= 1) {@branchHint(.unlikely);
        debug_print("spur value= {}\n bits=    {b:0>64}\n rotated= {b:0>64}\n result={}\n",
            .{value, bits, rotated, (rotated << 3) + TAG});
        return (rotated << 3) + TAG;
    }
    const offset =  rotated -% EXPONENT_OFFS;
    const shifted = rotl(u64, offset, 3);
    debug_print("spur value= {}\n bits=    {b:0>64}\n rotated= {b:0>64}\n offset=  {b:0>64}\n shifted= {b:0>64} unencodeable={}\n",
        .{value, bits, rotated, offset, shifted, shifted & 7 != 0 or offset == 0});
    if (shifted & 7 != 0) {@branchHint(.unlikely);
        return error.Unencodable;
    }
    if (offset == 0) {@branchHint(.unlikely);
        return error.Unencodable;
    }
    return shifted + TAG;
}
//inline
fn encode_check(value: f64) EncodeError!u64 {
    const bits: u64 = @bitCast(value);
    const exp: u64 = (bits & EXPONENT_MASK);
    const mant = bits & MANTISSA_MASK;

    // Handle zero / subnormal / special
    if (exp == 0) {
        return if (mant == 0) (((bits >> 60) & 0x8) | TAG) else error.Unencodable;
    }

    // Handle IEEE special values (Inf/NaN)
    if (exp == EXPONENT_MASK) {
        if (raise_specific_errors) {
            if (mant == 0) {
                return if ((bits & SIGN_MASK) != 0) EncodeError.NegInf else EncodeError.PosInf;
            }
            return EncodeError.NaN;
        } else return EncodeError.Unencodable;
    }

    const e11: u32 = @intCast(exp >> 52);

    // Handle Integer underflow
    if (e11 < EXPONENT_BIAS) return error.Unencodable;

    const exp8: u32 = e11 - EXPONENT_BIAS;

    // Handle out-of-range exponents
    if (exp8 > 255) return error.Unencodable;

    // Handle Zero-Collision Case
    if (exp8 == 0 and mant == 0) return error.Unencodable;

    var r = std.math.rotl(u64, bits, 4);
    r +%= 1;
    return std.math.rotr(u64, r, 1);
}
fn checkEqual(str: []const u8, i: usize, expected: anytype, actual: @TypeOf(expected)) !void {
    if (expectEqual(expected,actual)) |_| {
    } else |err| {
        const f: f64 = @bitCast(i << (64-BITS));
        std.debug.print("for i={}({x})({}) in {s}\n",.{i, i, f, str});
        return err;
    }
}
const BITS = 6;
test "encode accuracy - medium" {
    for (0..1<<BITS) |i| {
        const f: f64 = @bitCast(i << (64-BITS));
        try checkEqual("medium",i,encode_medium(f),encode_spur(f));
    }
}
test "encode accuracy - fast" {
    for (0..1<<BITS) |i| {
        const f: f64 = @bitCast(i << (64-BITS));
        try checkEqual("fast",i,encode_fast(f),encode_spur(f));
    }
}
// test "encode accuracy - spec" {
//     for (0..1<<BITS) |i| {
//         const f: f64 = @bitCast(i << (64-BITS));
//         try checkEqual("spec",i,encode_spec(f),encode_spur(f));
//     }
// }
pub fn decode(self: u64) ?f64 {
    if (self & TAG == 0) return null;
    if (self <= 0xC) {@branchHint(.unlikely);
        if (self == TAG) {
            return 0.0;
        } else {
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
    inline for (.{encode_spur,encode_medium,encode_fast}) |encode| {
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
}

const valid_values = [_]f64{ 0.0, -0.0 } ** 1 ++
    [_]f64{ 1.0, -1.0, math.pi, 42.0, -3.14159, 100.0, -100.0 } ** 16 ++
    [_]f64{ smallest, largest } ** 16;

const invalid_values =
    [_]f64{tooSmall} ** 1 ++
    [_]f64{tooLarge} ** 1 ++
    [_]f64{math.nan(f64)} ** 1 ++
    [_]f64{math.inf(f64)} ** 1 ++
    [_]f64{-math.inf(f64)} ** 1;

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

fn encode_valid(iterations: u64, comptime encode: fn (_: f64) EncodeError!u64) void {
    for (0..iterations / valid_values.len) |_| {
        for (valid_values) |val| {
            std.mem.doNotOptimizeAway(encode(val) catch 0);
        }
    }
}
fn encode_invalid(iterations: u64, comptime encode: fn (_: f64) EncodeError!u64) void {
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
// zig run -Doptimize=ReleaseFast floatSpur.zig
pub fn main() void {
    const highBits = 11;
    const count = 1 << highBits;
    var spurCount: usize = 0;
    for (0..count) |i| {
        if (encode_spur(@bitCast(((i << 1 | 1) << (62 - highBits)) + 1))) |_| {
            std.debug.assert(i == 0 or (i >> (highBits-4) == 7) or (i >> (highBits-4) == 8));
            spurCount += 1;
        } else |_| {}
    }
    std.debug.print("spur coverage is {d:.2}%\n",
        .{@as(f64, @floatFromInt(spurCount))*100.0/@as(f64, @floatFromInt(count))});

    const iterations = if (builtin.mode == .ReleaseFast) 1_000_000_000 else 100_000_000;
    const ns = 1.0 / @as(f64, @floatFromInt(iterations));

    if (false) {
        for (valid_values) |val| {
            std.debug.print("0x{x:0>16},\n", .{encode_spur(val) catch unreachable});
        }
    }

    // Benchmark encode_spec
    var timer = std.time.Timer.start() catch @panic("unreachable");

    _ = timer.lap();
    encode_valid(iterations,encode_fast);
    const fast_valid_time = timer.lap();
    encode_invalid(iterations,encode_fast);
    const fast_invalid_time = timer.lap();
    std.debug.print("fast time: {d:.3}ns {d:.3}ns\n", .{ @as(f64, @floatFromInt(fast_valid_time))*ns, @as(f64, @floatFromInt(fast_invalid_time))*ns });

    _ = timer.lap();
    encode_valid(iterations,encode_medium);
    const medium_valid_time = timer.lap();
    encode_invalid(iterations,encode_medium);
    const medium_invalid_time = timer.lap();
    std.debug.print("medium time: {d:.3}ns {d:.3}ns\n", .{ @as(f64, @floatFromInt(medium_valid_time))*ns, @as(f64, @floatFromInt(medium_invalid_time))*ns });

    _ = timer.lap();
    encode_valid(iterations,encode_spur);
    const spur_valid_time = timer.lap();
    encode_invalid(iterations,encode_spur);
    const spur_invalid_time = timer.lap();
    std.debug.print("spur time: {d:.3}ns {d:.3}ns\n", .{ @as(f64, @floatFromInt(spur_valid_time))*ns, @as(f64, @floatFromInt(spur_invalid_time))*ns });

    _ = timer.lap();
    encode_valid(iterations,encode_check);
    const check_valid_time = timer.lap();
    encode_invalid(iterations,encode_check);
    const check_invalid_time = timer.lap();
    std.debug.print("check time: {d:.3}ns {d:.3}ns\n", .{ @as(f64, @floatFromInt(check_valid_time))*ns, @as(f64, @floatFromInt(check_invalid_time))*ns });

    std.debug.print("fast is {d:.2}x {d:.2}x faster than check\n", .{ delta(fast_valid_time, check_valid_time), delta(fast_invalid_time, check_valid_time) });
    std.debug.print("medium is {d:.2}x {d:.2}x faster than check\n", .{ delta(medium_valid_time, check_valid_time), delta(medium_invalid_time, check_valid_time) });
    std.debug.print("spur is {d:.2}x {d:.2}x faster than check\n", .{ delta(spur_valid_time, check_valid_time), delta(spur_invalid_time, check_valid_time) });
    std.debug.print("fast is {d:.2}x {d:.2}x faster than spur\n", .{ delta(fast_valid_time, spur_valid_time), delta(fast_invalid_time, spur_invalid_time) });
    std.debug.print("medium is {d:.2}x {d:.2}x faster than spur\n", .{ delta(medium_valid_time, spur_valid_time), delta(medium_invalid_time, spur_invalid_time) });

}

fn delta(spec: u64, check: u64) f64 {
    return @as(f64, @floatFromInt(check)) / @as(f64, @floatFromInt(spec));
}
