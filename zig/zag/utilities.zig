const general = @import("utilities/general.zig");
pub const bitsToRepresent = general.bitsToRepresent;
pub const smallerPowerOf2 = general.smallerPowerOf2;
pub const largerPowerOf2 = general.largerPowerOf2;
pub const largeEnoughType = general.largeEnoughType;
pub const structLength = general.length;
const hash = @import("utilities/hash.zig");
pub const PhiHash = hash.Phi;
pub const ProspectorHash = hash.Prospector;
pub const objectHash = hash.objectHash;
pub const randomHash = hash.randomHash;
pub const Treap = @import("utilities/treap.zig").Treap;
const primes = @import("utilities/primes.zig");
pub const smallestPrimeAtLeast = primes.smallestPrimeAtLeast;
pub const largestPrimeLessThan = primes.largestPrimeLessThan;

pub const findFib = @import("utilities/fibonacci.zig").findFib;
pub const Stats = @import("utilities/stats.zig").Stats;
pub fn checkEqual(comptime int: u32, comptime int2: u32) ?[]const u8 {
    if (int == int2) return null;
    const b = struct {
        var buf: [20]u8 = undefined;
    };
    return @import("std").fmt.bufPrint(b.buf[0..], "{} instead of {}", .{ int, int2 }) catch @panic("unreachable");
}
test {
    _ = .{
        general,
        hash,
        Treap,
        primes,
        Stats,
    };
}
