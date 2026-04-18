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

pub const Fast = switch (builtin.target.cpu.arch) {
    .x86_64 => SpurAlt1,
    else => SpurAlt2,
}; // used by spur.zig
pub const EncodeError = error{ Unencodable, PosInf, NegInf, NaN };

// immediate float layout: [exp8(8)][mant(52)][sign(1)][tag(3)]
// Ref: https://clementbera.wordpress.com/2018/11/09/64-bits-immediate-floats/

//const debug_print = std.debug.print;
fn debug_print(_: anytype, _: anytype) void {}
pub const Spur = struct {
    pub fn encode(value: f64) EncodeError!u64 {
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
    pub fn decode(self: u64) ?f64 {
        if (self & TAG == 0) return null;
        const shifted = self >> 3;
        const offset = if (shifted <= 1) shifted else shifted + EXPONENT_OFFS;
        return @bitCast(std.math.rotr(u64, offset, 1));
    }
};
const SpurAlt1 = struct {
    pub fn encode(value: f64) EncodeError!u64 {
        const bits: u64 = @bitCast(value);
        const rotated = rotl(u64, bits, 5);
        const inc = rotated +% 1;
        debug_print("spurAlt1 value= {}\n bits=    {b:0>64}\n rotated= {b:0>64}\n inc=     {b:0>64}\n back=    {b:0>64} unencodeable={}\n",
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
    pub const decode = SpurAlt2.decode;
};
const SpurAlt2 = struct {
    pub fn encode(v: f64) EncodeError!u64 {
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
};

//inline
fn encode_default(v: f64) EncodeError!u64 {
    const bits: u64 = @bitCast(v);
    const y = rotr(u64, rotl(u64, bits, 5) +% 1, 1);
    if ((y & 0x7) == TAG and (y | 0x8) != 0xC) return y;
    if (bits & 0x7FFFFFFF_FFFFFFFF == 0) return TAG + ((bits >> 61) & 8);
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
test "encode accuracy - spurAlt1" {
    for (0..1<<BITS) |i| {
        const f: f64 = @bitCast(i << (64-BITS));
        try checkEqual("spurAlt1",i,SpurAlt1.encode(f),Spur.encode(f));
    }
}
test "encode accuracy - spurAlt2" {
    for (0..1<<BITS) |i| {
        const f: f64 = @bitCast(i << (64-BITS));
        try checkEqual("spurAlt2",i,SpurAlt2.encode(f),Spur.encode(f));
    }
}
// test "encode accuracy - spec" {
//     for (0..1<<BITS) |i| {
//         const f: f64 = @bitCast(i << (64-BITS));
//         try checkEqual("spec",i,encode_spec(f),Spur.encode(f));
//     }
// }

const smallest: f64 = @bitCast(@as(u64, 0x3800_0000_0000_0001));
const largest: f64 = @bitCast(@as(u64, 0x47FF_FFFF_FFFF_FFFF));
const tooSmall: f64 = @bitCast(@as(u64, 0x3800_0000_0000_0000));
const tooLarge: f64 = @bitCast(@as(u64, 0x4800_0000_0000_0000));

test "encode/decode" {
    inline for (.{Spur,SpurAlt1,SpurAlt2}) |model| {
        try expectEqual(0.0, model.decode(try model.encode(0.0)));
        try expectEqual(-0.0, model.decode(try model.encode(-0.0)));
        try expectEqual(0.5, model.decode(try model.encode(0.5)));
        try expectEqual(-0.5, model.decode(try model.encode(-0.5)));
        try expectEqual(1.0, model.decode(try model.encode(1.0)));
        try expectEqual(-1.0, model.decode(try model.encode(-1.0)));
        try expectEqual(2.0, model.decode(try model.encode(2.0)));
        try expectEqual(-2.0, model.decode(try model.encode(-2.0)));
        try expectEqual(math.pi, model.decode(try model.encode(math.pi)));
        try expectEqual(smallest, model.decode(try model.encode(smallest)));
        try expectEqual(-smallest, model.decode(try model.encode(-smallest)));
        try expectEqual(largest, model.decode(try model.encode(largest)));
        try expectEqual(-largest, model.decode(try model.encode(-largest)));
        try expectEqual(error.Unencodable, model.encode(tooSmall));
        try expectEqual(error.Unencodable, model.encode(tooLarge));
        try expectEqual(error.Unencodable, model.encode(math.nan(f64)));
        try expectEqual(error.Unencodable, model.encode(math.inf(f64)));
        try expectEqual(error.Unencodable, model.encode(-math.inf(f64)));
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

fn encode_valid(iters: u64, comptime encoder: fn (_: f64) EncodeError!u64) void {
    for (0..iters / valid_values.len) |_| {
        for (valid_values) |val| {
            std.mem.doNotOptimizeAway(encoder(val) catch 0);
        }
    }
}
fn encode_invalid(iters: u64, comptime encoder: fn (f64) EncodeError!u64) void {
    for (0..iters / invalid_values.len) |_| {
        for (invalid_values) |val| {
            std.mem.doNotOptimizeAway(encoder(val) catch 0);
        }
    }
}
pub fn decode_valid(iters: u64, comptime decoder: fn (u64) ?f64) void {
    for (0..iters / decode_values.len) |_| {
        for (decode_values) |val| {
            if (decoder(val)) |decoded|
                _ = std.mem.doNotOptimizeAway(decoded);
        }
    }
}
const iterations = if (builtin.mode == .ReleaseFast) 1_000_000_000 else 100_000_000;
fn ns(time: u64) f64 {
    return @as(f64, @floatFromInt(time)) / @as(f64, @floatFromInt(iterations));
}
// zig run -Doptimize=ReleaseFast floatSpur.zig
pub fn main() void {
    if (false) {
        for (valid_values) |val| {
            std.debug.print("0x{x:0>16},\n", .{Spur.encode(val) catch unreachable});
        }
    }

    // Benchmark encode_spec
    var timer = std.time.Timer.start() catch @panic("unreachable");

    _ = timer.lap();
    encode_valid(iterations,Spur.encode);
    const spur_valid_time = timer.lap();
    encode_invalid(iterations,Spur.encode);
    const spur_invalid_time = timer.lap();
    decode_valid(iterations,Spur.decode);
    const spur_decode_time = timer.lap();
    std.debug.print("spur time: {d:.3}ns {d:.3}ns {d:.3}ns\n", .{ ns(spur_valid_time), ns(spur_invalid_time), ns(spur_decode_time) });

    _ = timer.lap();
    encode_valid(iterations,SpurAlt1.encode);
    const spurAlt1_valid_time = timer.lap();
    encode_invalid(iterations,SpurAlt1.encode);
    const spurAlt1_invalid_time = timer.lap();
    decode_valid(iterations,SpurAlt1.decode);
    const spurAlt1_decode_time = timer.lap();
    std.debug.print("spurAlt1 time: {d:.3}ns {d:.3}ns {d:.3}ns\n", .{ ns(spurAlt1_valid_time), ns(spurAlt1_invalid_time), ns(spurAlt1_decode_time) });

    _ = timer.lap();
    encode_valid(iterations,SpurAlt2.encode);
    const spurAlt2_valid_time = timer.lap();
    encode_invalid(iterations,SpurAlt2.encode);
    const spurAlt2_invalid_time = timer.lap();
    decode_valid(iterations,SpurAlt2.decode);
    const spurAlt2_decode_time = timer.lap();
    std.debug.print("spurAlt2 time: {d:.3}ns {d:.3}ns {d:.3}ns\n", .{ ns(spurAlt2_valid_time), ns(spurAlt2_invalid_time), ns(spurAlt2_decode_time) });

    _ = timer.lap();
    encode_valid(iterations,encode_check);
    const check_valid_time = timer.lap();
    encode_invalid(iterations,encode_check);
    const check_invalid_time = timer.lap();
    std.debug.print("check time: {d:.3}ns {d:.3}ns\n", .{ ns(check_valid_time), ns(check_invalid_time) });

    std.debug.print("spur is {d:.2}x {d:.2}x faster than check\n", .{ delta(spur_valid_time, check_valid_time), delta(spur_invalid_time, check_valid_time) });
    std.debug.print("spurAlt1 is {d:.2}x {d:.2}x faster than check\n", .{ delta(spurAlt1_valid_time, check_valid_time), delta(spurAlt1_invalid_time, check_valid_time) });
    std.debug.print("spurAlt2 is {d:.2}x {d:.2}x faster than check\n", .{ delta(spurAlt2_valid_time, check_valid_time), delta(spurAlt2_invalid_time, check_valid_time) });
    std.debug.print("spurAlt1 is {d:.2}x {d:.2}x faster than spur\n", .{ delta(spurAlt1_valid_time, spur_valid_time), delta(spurAlt1_invalid_time, spur_invalid_time) });
    std.debug.print("spurAlt2 is {d:.2}x {d:.2}x faster than spur\n", .{ delta(spurAlt2_valid_time, spur_valid_time), delta(spurAlt2_invalid_time, spur_invalid_time) });
}

fn delta(spec: u64, check: u64) f64 {
    return @as(f64, @floatFromInt(check)) / @as(f64, @floatFromInt(spec));
}
