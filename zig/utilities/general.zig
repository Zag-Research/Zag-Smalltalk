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
