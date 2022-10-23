const std = @import("std");
const phi = std.math.phi;
pub fn inversePhi(comptime T: type) T {
    switch (@typeInfo(T)) {
        .Int => |int_info| switch (int_info.signedness) {
            .unsigned => return @floatToInt(T,(1<<int_info.bits)/phi),
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
    try expectEqual(inversePhi(u64),11400714819323198485);
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
pub inline fn largerPowerOf2(comptime T: type,size:T) T {
    var n = size-1;
    const bits = @typeInfo(T).Int.bits;
    if (comptime bits>32) n |= n>>32;
    if (comptime bits>16) n |= n>>16;
    if (comptime bits>8) n |= n>>8;
    if (comptime bits>4) n |= n>>4;
    n |= n>>2;
    n |= n>>1;
    return n+1;
}
