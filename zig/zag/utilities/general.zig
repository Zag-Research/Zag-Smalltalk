const std = @import("std");
const phi = std.math.phi;
// returns an odd number (changes u8) so all possible values are generated
pub fn inversePhi(comptime T: type) T {
    switch (@typeInfo(T)) {
        .int => |int_info| switch (int_info.signedness) {
            .unsigned => return @as(T, @intFromFloat(@as(f128, @floatFromInt(1 << int_info.bits)) / phi)) | 1,
            else => {},
        },
        else => {},
    }
    @compileError("invalid type for inversePhi: " ++ @typeName(T));
}
test "check inversePhi" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(inversePhi(u64), 11400714819323198485);
    try expectEqual(inversePhi(u32), 2654435769);
    try expectEqual(inversePhi(u24), 10368889);
    try expectEqual(inversePhi(u16), 40503);
    try expectEqual(inversePhi(u8), 159);
}
// there isn't a closed form way to calculate this, but
// https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Computing_multiplicative_inverses_in_modular_structures
// shows the Extended Eclidean algorithm can calculate in log time
pub fn undoPhi(comptime T: type) T {
    return inverse(T, inversePhi(T)) catch @panic("not invertible");
}
fn inverse(comptime T: type, a: T) !T {
    const n = 1 << @typeInfo(T).int.bits;
    var t: i128 = 0;
    var newt: i128 = 1;
    var r: i128 = n;
    var newr: i128 = a;
    while (newr != 0) {
        const quotient = @divTrunc(r, newr);
        var temp = newt;
        newt = t - quotient * newt;
        t = temp;
        temp = newr;
        newr = r - quotient * newr;
        r = temp;
    }
    if (r > 1) return error.NotInvertible;
    if (t < 0) t = t + n;
    return @truncate(@as(u128, @intCast(t)));
}

test "check undoPhi is inverse" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(undoPhi(u64) *% inversePhi(u64), 1);
    try expectEqual(undoPhi(u32) *% inversePhi(u32), 1);
    try expectEqual(undoPhi(u24) *% inversePhi(u24), 1);
    try expectEqual(undoPhi(u16) *% inversePhi(u16), 1);
    try expectEqual(undoPhi(u8) *% inversePhi(u8), 1);
}
test "check undoPhi" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(undoPhi(u64), 17428512612931826493);
    try expectEqual(undoPhi(u32), 340573321);
    try expectEqual(undoPhi(u24), 11764425);
    try expectEqual(undoPhi(u16), 30599);
    try expectEqual(undoPhi(u8), 95);
}
test "randomness of /phi - all values enumerated" {
    const expectEqual = @import("std").testing.expectEqual;
    var counts = [_]u32{0} ** 3;
    // 24 case crashes because it's too big
    // var data24 = [_]u24{0} ** (65536*256);
    // const phi24 = inversePhi(u24);
    // for (data24, 0..) |_, index| {
    //     data24[@as(u24, @truncate(index)) *% phi24] += 1;
    // }
    // for (data24) |count| {
    //     counts[count] += 1;
    // }
    // try expectEqual(counts[1], 65536*256);
    var data16 = [_]u16{0} ** 65536;
    const phi16 = inversePhi(u16);
    for (data16, 0..) |_, index| {
        data16[@as(u16, @truncate(index)) *% phi16] += 1;
    }
    for (data16) |count| {
        counts[count] += 1;
    }
    try expectEqual(counts[1], 65536);

    var data8 = [_]u16{0} ** 256;
    const phi8 = inversePhi(u8);
    for (data8, 0..) |_, index| {
        data8[@as(u8, @truncate(index)) *% phi8] += 1;
    }
    counts[1] = 0;
    for (data8) |count| {
        counts[count] += 1;
    }
    try expectEqual(counts[1], 256);
}
pub inline fn bitsToRepresent(value: anytype) u7 {
    const T = @TypeOf(value);
    switch (@typeInfo(T)) {
        .comptime_int => {
            comptime var n = value;
            n |= n >> 32;
            n |= n >> 16;
            n |= n >> 8;
            n |= n >> 4;
            n |= n >> 2;
            n |= n >> 1;
            return comptime @ctz(~@as(u64, n));
        },
        .int => |int_info| switch (int_info.signedness) {
            .unsigned => return @intCast(int_info.bits - @clz(value)),
            else => {},
        },
        else => {},
    }
    @compileError("bitsToRepresent not implemented for " ++ @typeName(T));
}

test "checking bitsToRepresent" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(bitsToRepresent(15), 4);
    try expectEqual(bitsToRepresent(@as(u16, 1)), 1);
    try expectEqual(bitsToRepresent(@as(u16, 15)), 4);
    try expectEqual(bitsToRepresent(@as(u16, 16)), 5);
    try expectEqual(bitsToRepresent(@as(u16, 17)), 5);
    try expectEqual(bitsToRepresent(@as(u16, 33)), 6);
    try expectEqual(bitsToRepresent(@as(u16, 255)), 8);
    const t4092: u12 = 4092;
    try expectEqual(bitsToRepresent(t4092), 12);
}
pub inline fn largerPowerOf2(value: anytype) u64 {
    if (value <= 1) return 1;
    const bits = bitsToRepresent(value - 1);
    return @as(u64, 1) << @as(u6, @truncate(bits));
}
test "check largerPowerOf2" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(largerPowerOf2(@as(u16, 1)), 1);
    try expectEqual(largerPowerOf2(@as(u16, 2)), 2);
    try expectEqual(largerPowerOf2(@as(u16, 15)), 16);
    try expectEqual(largerPowerOf2(@as(u16, 16)), 16);
    try expectEqual(largerPowerOf2(@as(u16, 17)), 32);
    try expectEqual(largerPowerOf2(@as(u16, 33)), 64);
    try expectEqual(largerPowerOf2(@as(u16, 255)), 256);
    const t4092: u12 = 4092;
    try expectEqual(largerPowerOf2(t4092), 4096);
}
pub inline fn smallerPowerOf2(value: anytype) u64 {
    return largerPowerOf2(value + 1) / 2;
}
test "check smallerPowerOf2" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(smallerPowerOf2(@as(u16, 1)), 1);
    try expectEqual(smallerPowerOf2(@as(u16, 2)), 2);
    try expectEqual(smallerPowerOf2(@as(u16, 15)), 8);
    try expectEqual(smallerPowerOf2(@as(u16, 16)), 16);
    try expectEqual(smallerPowerOf2(@as(u16, 17)), 16);
    try expectEqual(smallerPowerOf2(@as(u16, 33)), 32);
    try expectEqual(smallerPowerOf2(@as(u16, 255)), 128);
}
pub inline fn sqrtPowerOf2(value: anytype) u64 {
    if (value <= 1) return 1;
    const bits = @divTrunc(bitsToRepresent(value-1)+1,2);
    return @as(u64, 1) << @as(u6, @truncate(bits));
}
test "check sqrtPowerOf2" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(sqrtPowerOf2(@as(u16, 1)), 1);
    try expectEqual(sqrtPowerOf2(@as(u16, 2)), 2);
    try expectEqual(sqrtPowerOf2(@as(u16, 15)), 4);
    try expectEqual(sqrtPowerOf2(@as(u16, 16)), 4);
    try expectEqual(sqrtPowerOf2(@as(u16, 17)), 8);
    try expectEqual(sqrtPowerOf2(@as(u16, 33)), 8);
    try expectEqual(sqrtPowerOf2(@as(u16, 64)), 8);
    try expectEqual(sqrtPowerOf2(@as(u16, 65)), 16);
    try expectEqual(sqrtPowerOf2(@as(u16, 255)), 16);
}
