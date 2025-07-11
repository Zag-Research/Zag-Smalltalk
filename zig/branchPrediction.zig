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
const test_type: Tests = .TF;
pub fn main() u8 {
    const max: usize = 10_000_000_000;
    var sum: usize = 0;
    for (0..max) |i| {
        // Let’s measure the performance of this loop with different conditions:
        if (switch (test_type) {           // M2Pro   i7-2.6GHz
            .T_repeated => i & 0x80000000, // 1.84s    1.15s
            .F_repeated => i & 0xffffffff, // 1.84s    1.15s
            .TF => i & 1,                  // 1.55s    0.75s
            .T2F2 => i & 2,                // 1.68s    0.76s
            .TFFF => i & 3,                // 1.82s    0.75s
            .T4F4 => i & 4,                // 1.82s    0.87s
            .TFTFFFFF => i & 5,            // 1.84s    1.05s
            .T8F8 => i & 8,                // 1.84s    1.10s
            .T16F16 => i & 16,             // 1.84s    1.10s
            } == 0) sum += i;
    }
    return @truncate(sum);
}
