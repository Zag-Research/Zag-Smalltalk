const general = @import("utilities/general.zig");
pub const inversePhi = general.inversePhi;
pub const undoPhi = general.undoPhi;
pub const bitsToRepresent = general.bitsToRepresent;
pub const smallerPowerOf2 = general.smallerPowerOf2;
pub const largerPowerOf2 = general.largerPowerOf2;
pub const largerPowerOf2Not1 = general.largerPowerOf2Not1;
pub const Treap = @import("utilities/treap.zig").Treap;
const primes = @import("utilities/primes.zig");
pub const next_prime_larger_than = primes.next_prime_larger_than;
pub const smallestPrimeAtLeast = primes.smallestPrimeAtLeast;
pub const largestPrimeLessThan = primes.largestPrimeLessThan;

pub const findFib = @import("utilities/fibonacci.zig").findFib;
pub const Stats = @import("utilities/stats.zig").Stats;
pub fn checkEqual(comptime int: u32, comptime int2: u32) ?[]const u8 {
    if (int == int2) return null;
    const b = struct {
        var buf: [20]u8 = undefined;
    };
    return @import("std").fmt.bufPrint(b.buf[0..], "{} instead of {}", .{ int, int2 }) catch unreachable;
}
