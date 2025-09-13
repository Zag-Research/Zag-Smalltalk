const std = @import("std");
const math = std.math;
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;

const TAG = 0b100; // immediate float tag
pub const EncodeError = error{ Unencodable, PosInf, NegInf, NaN };

// immediate float layout: [exp8(8)][mant(52)][sign(1)][tag(3)]
// Ref: https://clementbera.wordpress.com/2018/11/09/64-bits-immediate-floats/

const NEG_ZERO: u64 = 0x8000_0000_0000_0000; 
const NEG_INFINITY: u64 = 0xFFF0_0000_0000_0000; 
const EXPONENT_MASK: u64 = 0x7FF0_0000_0000_0000; 
const MANTISSA_MASK: u64 = 0x000F_FFFF_FFFF_FFFF;  // also inf
const SPUR_POS_ZERO: u64 = 4; 
const SPUR_NEG_ZERO: u64 = 12; 

pub fn encode(v: f64) !u64 {
    const bits: u64 = @bitCast(v);
    var y = std.math.rotl(u64, bits, 5);
    y +%= 1;
    y = std.math.rotr(u64, y, 1);

    if ((y & 0x7) == TAG and (y >> 4 != 0)) return y;

    switch (bits) {
        0 => return SPUR_POS_ZERO,
        NEG_ZERO => return SPUR_NEG_ZERO,
        EXPONENT_MASK => return EncodeError.PosInf,
        NEG_INFINITY => return EncodeError.NegInf,
        else => {
            // Check for NaN: exponent = 0x7FF and mantissa ≠ 0
            if ((bits & EXPONENT_MASK) == EXPONENT_MASK and
                (bits & MANTISSA_MASK) != 0)
            {
                return EncodeError.NaN;
            }
        },
    }

    return EncodeError.Unencodable; // unencodable (subnormal vs out-of-range)
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
