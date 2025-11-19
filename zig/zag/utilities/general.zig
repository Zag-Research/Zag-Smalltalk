const std = @import("std");
// https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Computing_multiplicative_inverses_in_modular_structures
// shows the Extended Eclidean algorithm can calculate in log time
pub fn inverseMod(comptime T: type, a: T) !T {
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
pub inline fn largeEnoughType(value: anytype) type {
    return std.meta.Int(.unsigned, bitsToRepresent(value));
}
test "check largeEnoughType" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(u4, largeEnoughType(15));
    try expectEqual(u1, largeEnoughType(@as(u16, 1)));
    try expectEqual(u4, largeEnoughType(@as(u16, 15)));
    try expectEqual(u5, largeEnoughType(@as(u16, 16)));
    try expectEqual(u5, largeEnoughType(@as(u16, 17)));
    try expectEqual(u6, largeEnoughType(@as(u16, 33)));
    try expectEqual(u8, largeEnoughType(@as(u16, 255)));
    const t4092: u12 = 4092;
    try expectEqual(u12, largeEnoughType(t4092));
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
    const bits = @divTrunc(bitsToRepresent(value - 1) + 1, 2);
    return @as(u64, 1) << @as(u6, @truncate(bits));
}
test "check sqrtPowerOf2" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(sqrtPowerOf2(@as(u16, 1)), 1);
    try expectEqual(sqrtPowerOf2(@as(u16, 2)), 2);
    try expectEqual(sqrtPowerOf2(@as(u16, 15)), 4);
    try expectEqual(sqrtPowerOf2(@as(u16, 16)), 4);
    try expectEqual(sqrtPowerOf2(@as(u16, 17)), 8);
    try expectEqual(sqrtPowerOf2(@as(u16, 32)), 8);
    try expectEqual(sqrtPowerOf2(@as(u16, 33)), 8);
    try expectEqual(sqrtPowerOf2(@as(u16, 64)), 8);
    try expectEqual(sqrtPowerOf2(@as(u16, 65)), 16);
    try expectEqual(sqrtPowerOf2(@as(u16, 255)), 16);
}
pub inline fn length(obj: anytype) u11 {
    const T = @TypeOf(obj);
    return switch (@typeInfo(T)) {
        .pointer => |p| lengthOfType(p.child),
        .type => lengthOfType(obj),
        else => lengthOfType(T),
    };
}
inline fn lengthOfType(T: type) u11 {
    return @sizeOf(T) / 8 -
        if (@hasField(T,"header") and @offsetOf(T,"header") == 0) 1 else 0;
}
test "length" {
    const expectEqual = std.testing.expectEqual;
    const s1 = struct{x:u64,y:u64};
    const s2 = struct{header:u64,y:u64};
    const s3 = struct{x:u64,header:u64};
    const v1:s1 = undefined;
    const v2:s2 = undefined;
    const v3:s3 = undefined;
    try expectEqual(2, length(s1));
    try expectEqual(1, length(s2));
    try expectEqual(2, length(s3));
    try expectEqual(2, length(v1));
    try expectEqual(1, length(v2));
    try expectEqual(2, length(v3));
    try expectEqual(2, length(&v1));
    try expectEqual(1, length(&v2));
    try expectEqual(2, length(&v3));
    _ = std.posix.clock_gettime(42);
}
