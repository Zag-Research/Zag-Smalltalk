const std = @import("std");
const phi = std.math.phi;
pub fn inversePhi(comptime T: type) T {
    switch (@typeInfo(T)) {
        .Int => |int_info| switch (int_info.signedness) {
            .unsigned => return @floatToInt(T,@intToFloat(f64,1<<int_info.bits)/phi),
            else => {},
        },
        else => {},
    }
    @compileError("invalid type for inversePhi: "++@typeName(T));
}
test "check inversePhi" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(inversePhi(u32),2654435769);
    try expectEqual(inversePhi(u16),40503);
    try expectEqual(inversePhi(u8),158);
    try expectEqual(inversePhi(u64),11400714819323197440);
}
test "randomness of /phi - all values enumerated" {
    var data = [_]u16{0} ** 65536;
    var counts = [_]u32{0} ** 2;
    const phi16 = inversePhi(u16);
    for (data) |_,index| {
        data[@truncate(u16,index)*%phi16] += 1;
    }
    for (data) |count| {
        counts[count] += 1;
    }
    //try std.io.getStdOut().writer().print("\n counts: {any}",.{counts});
    const expectEqual = @import("std").testing.expectEqual;
    try expectEqual(counts[1],65536);
}
inline fn po2(size:anytype,comptime not1: u1) @TypeOf(size) {
    var n = size-1;
    n |= not1;
    const bits = @typeInfo(@TypeOf(size)).Int.bits;
    if (comptime bits>32) n |= n>>32;
    if (comptime bits>16) n |= n>>16;
    if (comptime bits>8) n |= n>>8;
    if (comptime bits>4) n |= n>>4;
    n |= n>>2;
    n |= n>>1;
    return n+1;
}
pub inline fn largerPowerOf2(size:anytype) @TypeOf(size) {
    return po2(size,0);
}
test "check largerPowerOf2" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(largerPowerOf2(@as(u16,1)),1);
    try expectEqual(largerPowerOf2(@as(u16,16)),16);
    try expectEqual(largerPowerOf2(@as(u16,33)),64);
    try expectEqual(largerPowerOf2(@as(u16,255)),256);
}
pub inline fn largerPowerOf2Not1(size:anytype) @TypeOf(size) {
    return po2(size,1);
}
test "check largerPowerOf2" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(largerPowerOf2Not1(@as(u16,1)),2);
    try expectEqual(largerPowerOf2Not1(@as(u16,2)),2);
    try expectEqual(largerPowerOf2Not1(@as(u16,16)),16);
    try expectEqual(largerPowerOf2Not1(@as(u16,33)),64);
    try expectEqual(largerPowerOf2Not1(@as(u16,255)),256);
}
pub inline fn bitToRepresent(size:anytype) u16 {
    var n = size;
    const bits = @typeInfo(@TypeOf(size)).Int.bits;
    if (comptime bits>32) n |= n>>32;
    if (comptime bits>16) n |= n>>16;
    if (comptime bits>8) n |= n>>8;
    if (comptime bits>4) n |= n>>4;
    n |= n>>2;
    n |= n>>1;
    return @ctz(~n);
}
test "check bitToRepresent" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(bitToRepresent(@as(u16,1)),1);
    try expectEqual(bitToRepresent(@as(u16,15)),4);
    try expectEqual(bitToRepresent(@as(u16,16)),5);
    try expectEqual(bitToRepresent(@as(u16,17)),5);
}
