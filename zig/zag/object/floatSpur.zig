const std = @import("std");
const math = std.math;
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;

const EXPONENT_MASK: u64 = 0x7FF0000000000000;
const MANTISSA_MASK: u64 = 0x000FFFFFFFFFFFFF;
const TAG = 0b100; // immediate float tag

// exp8 = (exp11 - 1023) + 127 = exp11 - 896
const EXPONENT_BIAS: u16 = 896; // 1023 - 127
const REBIAS_CONST: u64 = 0x7000_0000_0000_0000; // 896 << 53

fn encode_nocheck(bits: u64) u64 {
    // spur encoding: [63:56]=exp8, [55:4]=mantissa, [3]=sign, [2:0]=tag(=0b100)
    var r: u64 = std.math.rotl(u64, bits, 1);
    r -%= REBIAS_CONST;
    r = (r << 3) + TAG;
    return r;
}

// immediate float layout: [exp8(8)][mant(52)][sign(1)][tag(3)]
// Ref: https://clementbera.wordpress.com/2018/11/09/64-bits-immediate-floats/
pub fn encode(value: f64) !u64 {
    const bits: u64 = @bitCast(value);
    const exp: u64 = (bits & EXPONENT_MASK);

    // Handle zero / subnormal / special
    if (exp == 0) {
        const mant = bits & MANTISSA_MASK;
        return if (mant == 0) (((bits >> 60) & 0x8) | TAG) else error.Unencodable;
    }

    // Handle IEEE special values (Inf/NaN)
    if (exp == EXPONENT_MASK) return error.Unencodable;

    const e11: u32 = @intCast(exp >> 52);
    const exp8_u32: u32 = e11 - EXPONENT_BIAS;
    // Handle out-of-range exponents
    if ((exp8_u32 & ~@as(u32, 0xFF)) != 0) return error.Unencodable;
    // Handle collision with zero encoding
    if (exp8_u32 == 0 and (bits & MANTISSA_MASK) == 0) return error.Unencodable;

    return encode_nocheck(bits);
}

pub fn decode(self: u64) f64 {
    var hash = self >> 3;
    if (hash <= 1) {
        return @bitCast((hash & 1) << 63);
    }
    hash +%= REBIAS_CONST;
    return @bitCast(math.rotr(u64, hash, 1));
}

test "encode/decode" {
    try expectEqual(1.0, decode(try encode(1.0)));
    try expectEqual(-1.0, decode(try encode(-1.0)));
    try expectEqual(0.0, decode(try encode(0.0)));
    try expectEqual(-0.0, decode(try encode(-0.0)));
    try expectEqual(math.pi, decode(try encode(math.pi)));
    const smallest: f64 = @bitCast(@as(u64, 0x3800_0000_0000_0001));
    const largest: f64 = @bitCast(@as(u64, 0x47FF_FFFF_FFFF_FFFF));
    try expectEqual(smallest, decode(try encode(smallest)));
    try expectEqual(-smallest, decode(try encode(-smallest)));
    try expectEqual(largest, decode(try encode(largest)));
    try expectEqual(-largest, decode(try encode(-largest)));
    const tooSmall: f64 = @bitCast(@as(u64, 0x3800_0000_0000_0000));
    const tooLarge: f64 = @bitCast(@as(u64, 0x4800_0000_0000_0000));
    try expectEqual(error.Unencodable, encode(tooSmall));
    try expectEqual(error.Unencodable, encode(tooLarge));
    try expectEqual(error.Unencodable, encode(math.nan(f64)));
    try expectEqual(error.Unencodable, encode(math.inf(f64)));
    try expectEqual(error.Unencodable, encode(-math.inf(f64)));
}
