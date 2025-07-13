const std = @import("std");
const Tests = enum {
    True,
    False,
    T_repeated,
    F_repeated,
    TF,
    T2F2,
    TFFF,
    T4F4,
    TFTFFFFF,
    T8F8,
    T16F16,
    T32F32,
    T64F64,
};
fn compute(comptime test_type: Tests, proof: usize) usize {
    const max: usize = 0x40000009; // a prime number just over a billion - non-primes produce very different timings
    var proof1: usize = proof;
    var proof2: usize = 0;
    for (0..max) |i| {
        // Let’s measure the performance of this loop with different conditions:
        if (switch (test_type) {
            .True => true,
            .False => false,
            .T_repeated => i & 0x40000000 == 0,
            .F_repeated => i & 0x40000000 != 0,
            .TF => i & 1 == 0,
            .T2F2 => i & 2 == 0,
            .TFFF => i & 3 == 0,
            .T4F4 => i & 4 == 0,
            .TFTFFFFF => i & 5 == 0,
            .T8F8 => i & 8 == 0,
            .T16F16 => i & 16 == 0,
            .T32F32 => i & 32 == 0,
            .T64F64 => i & 64 == 0,
        }) {
            proof1 = proof1 ^ i;
        } else {
            proof2 = proof2 ^ i;
        }
    }
    return proof1 ^ proof2;
}
pub fn main() void {
    inline for (std.meta.fields(Tests)) |field| {
        const test_type = @field(Tests, field.name);
        var stat = @import("zag/utilities/stats.zig").Stats(Tests, usize, 10, .milliseconds).init();
        stat.time(compute, test_type);
        std.debug.print("{s:<12}: {mu+%s:.2}\n", .{ @tagName(test_type), stat });
    }
}
