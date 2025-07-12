const std = @import("std");
const Tests = enum {
    T_repeated,
    F_repeated,
    TF,
    T2F2,
    TFFF,
    T4F4,
    TFTFFFFF,
    T8F8,
    T16F16,
};
fn compute(comptime test_type: Tests, proof: usize) usize {
    const max: usize = 1_000_000_007; // changing to 1 billion (a non-prime) produces very different timing
    var sum: usize = proof;
    for (0..max) |i| {
        // Let’s measure the performance of this loop with different conditions:
        if (switch (test_type) {           // M2Pro         i7-2.6GHz
            .T_repeated => i != 1_000,     // 1.172s±0.4%   0.255s±0.5%
            .F_repeated => i == 1_000,     // optimized     0.063s±1.4%
            .TF => i & 1 == 0,             // 1.478s±0.7%   0.131s±8.5%
            .T2F2 => i & 2 == 0,           // 1.467s±1.5%   0.128s±1.3%
            .TFFF => i & 3 == 0,           // 1.472x±1.0%   0.063s±0.8%
            .T4F4 => i & 4 == 0,           // 1.476s±1.1%   0.164s±5.5%
            .TFTFFFFF => i & 5 == 0,       // 1.477s±1.3%   0.095s±0.7%
            .T8F8 => i & 8 == 0,           // 1.472s±0.5%   0.320s±0.8%
            .T16F16 => i & 16 == 0,        // 1.470s±1.0%   0.320s±0.5%
        }) sum <<= @as(u6,@truncate(i));
    }
    return sum;
}
pub fn main() void {
    inline for (std.meta.fields(Tests)) |field| {
        const test_type = @field(Tests, field.name);
        var stat = @import("zag/utilities/stats.zig").Stats(Tests, usize, 10, .milliseconds).init();
        stat.time(compute, test_type);
        std.debug.print("{s:<12}: {mu+%s:.2}\n", .{ @tagName(test_type), stat });
    }
}
