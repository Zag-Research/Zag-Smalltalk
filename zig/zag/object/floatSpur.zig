const std = @import("std");
const math = std.math;
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;

const SIGN_MASK: u64 = 0x8000000000000000;
const EXPONENT_MASK: u64 = 0x7FF0000000000000;
const MANTISSA_MASK: u64 = 0x000FFFFFFFFFFFFF;
const EXPONENT_BIAS = 896;
const MAX_EXPONENT = EXPONENT_BIAS + 0xFF;

// immediate float: [exponent(8)][mantissa(52)][sign(1)][tag(3)]
pub fn encode(value: f64) !u64 {
    const bits: u64 = @bitCast(value);
    const sign = (bits & SIGN_MASK) >> 63;
    const exponent = (bits & EXPONENT_MASK) >> 52;
    const mantissa = bits & MANTISSA_MASK;

    if (exponent == 0) {
        return if (mantissa == 0) 0 else error.Unencodable;
    }

    if (exponent == 0x7FF or exponent <= EXPONENT_BIAS or exponent >= MAX_EXPONENT) {
        return error.Unencodable;
    }

    const adjusted_exponent = exponent - EXPONENT_BIAS;
    return (adjusted_exponent << 53) | (mantissa << 1) | sign;
}

pub fn decode(self: u64) f64 {
    const hash = self >> 3;
    if (hash == 0) return 0.0;

    const sign: u64 = hash & 1;
    const mantissa: u64 = (hash >> 1) & MANTISSA_MASK;
    const adjusted_exponent: u64 = hash >> 53;

    const exponent = adjusted_exponent + EXPONENT_BIAS;
    return @bitCast((sign << 63) | (exponent << 52) | mantissa);
}

test "encode/decode" {
    try expectEqual(1.0, decode(try encode(1.0)));
    try expectEqual(-1.0, decode(try encode(-1.0)));
    try expectEqual(0.0, decode(try encode(0.0)));
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
