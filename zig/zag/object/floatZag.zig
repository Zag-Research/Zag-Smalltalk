const std = @import("std");
const math = std.math;
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;

pub inline fn encode(x: f64) !u64 {
    const u = math.rotl(u64, @bitCast(x), 4) + 2;
    if (u & 6 != 0)
        return u;
    return error.Unencodable;
}
pub inline fn decode(self: u64) f64 {
    return @bitCast(math.rotr(u64, self - 2, 4));
}
test "encode/decode" {
    try expectEqual(1.0, decode(try encode(1.0)));
    try expectEqual(-1.0, decode(try encode(-1.0)));
    try expectEqual(0.0, decode(try encode(0.0)));
    try expectEqual(math.pi, decode(try encode(math.pi)));
    const smallest: f64 = @bitCast(@as(u64,0x0000_0000_0000_0001));
    const largest: f64 = @bitCast(@as(u64,0x5FFF_FFFF_FFFF_FFFF));
    try expectEqual(smallest, decode(try encode(smallest)));
    try expectEqual(-smallest, decode(try encode(-smallest)));
    try expectEqual(largest, decode(try encode(largest)));
    try expectEqual(-largest, decode(try encode(-largest)));
    const tooLarge: f64 = @bitCast(@as(u64,0x6000_0000_0000_0000));
    try expectEqual(error.Unencodable, encode(tooLarge));
    try expectEqual(error.Unencodable, encode(math.nan(f64)));
    try expectEqual(error.Unencodable, encode(math.inf(f64)));
    try expectEqual(error.Unencodable, encode(-math.inf(f64)));
}
