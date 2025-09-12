const std = @import("std");
const math = std.math;
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;

const EXPONENT_MASK: u64 = 0x7FF0000000000000;
const MANTISSA_MASK: u64 = 0x000FFFFFFFFFFFFF;
const EXPONENT_BIAS: u16 = 896; // exp8 = (exp11 - 1023) + 127 = exp11 - 896
const REBIAS_CONST: u64 = 0x7000_0000_0000_0000; // 896 << 53

const TAG = 0b100; // immediate float tag
const SHIFTED_EXP: u64 = 0xFFE0_0000_0000_0000;
const SIGN_MASK: u64 = 0x8000_0000_0000_0000;
const ZERO_SHAPE: u64 = 0x8000_0000_0000_0008;

pub const EncodeError = error{ Unencodable, PosInf, NegInf, NaN };

pub fn encode_norebias(bits: u64) u64 {
    // when the top 4 exp bits (0111 or 1000) are in the low 4 bits
    // +1 turns them into 1000/1001
    var r = std.math.rotl(u64, bits, 5);
    r +%= 1;
    return std.math.rotr(u64, r, 1);
}

pub fn encode_spec(v: f64) !u64 {
    const bits: u64 = @bitCast(v);
    const y = encode_norebias(bits);

    // handle normal immediate float; with zero-collision check
    if ((y & 0x7) == TAG) return if ((y | 0x8) == 0xC) return EncodeError.Unencodable else y;
    // handle zero
    if ((y | 0x8) == ZERO_SHAPE) return ((bits >> 60) & 0x8) | TAG;
    // handle Inf
    const shifted = (bits << 1);
    if (shifted == SHIFTED_EXP) return if ((bits & SIGN_MASK) != 0) EncodeError.NegInf else EncodeError.PosInf;
    // handle NaN
    if (shifted > SHIFTED_EXP) return EncodeError.NaN;

    return EncodeError.Unencodable; // unencodable (subnormal vs out-of-range)
}

// immediate float layout: [exp8(8)][mant(52)][sign(1)][tag(3)]
// Ref: https://clementbera.wordpress.com/2018/11/09/64-bits-immediate-floats/
pub fn encode_check(value: f64) !u64 {
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

    return encode_norebias(bits);
}

pub fn decode(self: u64) f64 {
    var r = std.math.rotl(u64, self, 1);
    r -%= 1;
    if (r <= 0x18) {
        r &= 0x10;
    }
    r = std.math.rotr(u64, r, 5);
    return @bitCast(r);
}


const encode = encode_spec;

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
    try expectEqual(EncodeError.Unencodable, encode(tooSmall));
    try expectEqual(EncodeError.Unencodable, encode(tooLarge));
    try expectEqual(EncodeError.NaN, encode(math.nan(f64)));
    try expectEqual(EncodeError.PosInf, encode(math.inf(f64)));
    try expectEqual(EncodeError.NegInf, encode(-math.inf(f64)));
}
