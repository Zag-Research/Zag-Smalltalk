const std = @import("std");
const math = std.math;
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;

const SIGN_MASK: u64 = 0x8000000000000000;
const EXPONENT_MASK: u64 = 0x7FF0000000000000;
const MANTISSA_MASK: u64 = 0x000FFFFFFFFFFFFF;
const TAG = 0b100; // immediate float tag

// exp8 = (exp11 - 1023) + 127 = exp11 - 896
const EXPONENT_BIAS: u16 = 896; // 1023 - 127
const MAX_EXPONENT: u16 = EXPONENT_BIAS + 0xFF; // 896 + 255 = 1151

// immediate float layout: [exp8(8)][mant(52)][sign(1)][tag(3)]
// Ref: https://clementbera.wordpress.com/2018/11/09/64-bits-immediate-floats/
pub fn encode(value: f64) !u64 {
    const bits: u64 = @bitCast(value);
    const sign: u64 = (bits & SIGN_MASK) >> 63;
    const exp11: u64 = (bits & EXPONENT_MASK) >> 52;
    const mantissa: u64 = bits & MANTISSA_MASK;

    // Handle zero / subnormal / special
    if (exp11 == 0) {
        // zero if mantissa==0, else subnormal (reject or normalize)
        if (mantissa == 0) {
            // Represent ±0 with exp8=0, mant=0 and keep sign; tag=0b100 example
            return (0 << 56) | (0 << 4) | (sign << 3) | TAG;
        }
        return error.Unencodable;
    }

    // IEEE special (Inf/NaN)
    if (exp11 == 0x7FF) return error.Unencodable;

    // normal window: 0x380..0x47F inclusive
    const e11: u16 = @intCast(exp11);
    if (e11 < EXPONENT_BIAS or e11 > MAX_EXPONENT) return error.Unencodable;

    // avoid collision with immediate zero
    if (e11 == EXPONENT_BIAS and mantissa == 0) return error.Unencodable;

    const exp8: u8 = @intCast(e11 - EXPONENT_BIAS);

    // spur encoding: [63:56]=exp8, [55:4]=mantissa, [3]=sign, [2:0]=tag(=0b100)
    return (@as(u64, exp8) << 56) | (mantissa << 4) | (sign << 3) | TAG;
}

pub fn decode(self: u64) f64 {
    const hash = self >> 3;
    const sign: u64 = hash & 1;
    const mantissa: u64 = (hash >> 1) & MANTISSA_MASK;
    const exp8: u64 = hash >> 53;

    // handle +- zeros
    if (exp8 == 0 and mantissa == 0) {
        const bits: u64 = sign << 63; 
        return @bitCast(bits);
    }

    const exponent = exp8 + EXPONENT_BIAS;
    return @bitCast((sign << 63) | (exponent << 52) | mantissa);
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
