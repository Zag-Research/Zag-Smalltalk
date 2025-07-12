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
    const max: usize = 1_000_000_000;
    var sum: usize = proof;
    for (0..max) |i| {
        // Let’s measure the performance of this loop with different conditions:
        if (switch (test_type) {           // M2Pro         i7-2.6GHz
            .T_repeated => i & 0x80000000, // 1.172s±0.4%   1.15s
            .F_repeated => i & 0xffffffff, // optimized     1.15s
            .TF => i & 1,                  // 1.478s±0.7%   0.75s
            .T2F2 => i & 2,                // 1.467s±1.5%   0.76s
            .TFFF => i & 3,                // 1.472x±1.0%   0.75s
            .T4F4 => i & 4,                // 1.476s±1.1%   0.87s
            .TFTFFFFF => i & 5,            // 1.477s±1.3%   1.05s
            .T8F8 => i & 8,                // 1.472s±0.5%   1.10s
            .T16F16 => i & 16,             // 1.470s±1.0%   1.10s
            } == 0) sum ^= sum *% i;
    }
    //std.debug.print("Result for {s}: {}\n", .{ @tagName(test_type), sum });
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
