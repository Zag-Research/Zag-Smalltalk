const general = @import("utilities/general.zig");
pub const inversePhi = general.inversePhi;
pub const bitToRepresent = general.bitToRepresent;
pub const largerPowerOf2 = general.largerPowerOf2;
pub const largerPowerOf2Not1 = general.largerPowerOf2Not1;
pub const Treap = @import("utilities/Treap.zig").Treap;
pub const next_prime_larger_than = @import("utilities/primes.zig").next_prime_larger_than;
pub const findFib =  @import("utilities/fibonacci.zig").findFib;
pub const Stats = @import("utilities/stats.zig").Stats;
pub fn checkEqual(int: u32,int2: u32) ?[]const u8 {
    if (int==int2) return null;
    var buf: [20]u8 = undefined;
    return @import("std").fmt.bufPrint(buf[0..], "{} instead of {}", .{int,int2}) catch unreachable;
}
