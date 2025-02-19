const std = @import("std");
const math = std.math;
const Order = math.Order;
const stdout = std.io.getStdOut().writer();
//const expect = @import("std").testing.expect;
const treap = @import("treap.zig");
const stats = @import("stats.zig");

// better primality tests can be found described at: https://t5k.org/prove/prove2_3.html (and generally more about primes at t5k.org)

const bits = init: {
    var initial_value: [0x10000]bool = undefined;
    init_bits(initial_value[0..]);
    break :init initial_value;
};
fn init_bits(array: []bool) void {
    @setEvalBranchQuota(300000);
    var i: usize = 3;
    while (i < array.len) : (i += 2) {
        array[i - 1] = false;
        array[i] = true;
    }
    array[0] = false;
    array[1] = false;
    array[2] = true;
    i = 3;
    while (i < array.len) : (i += 2) {
        if (array[i]) {
            var j: usize = i + i;
            while (j < array.len) : (j += i) {
                array[j] = false;
            }
        }
    }
}
inline fn isPrime(p: anytype) bool {
    if (p < bits.len) return bits[p];
    return isPrimeSearch(p);
}
fn isPrimeSearch(p: anytype) bool {
    if (p & 1 == 0) return false;
    var i: usize = 3;
    while (i * i < p) : (i += 2) {
        if (i < bits.len and bits[i]) {
            if (p % i == 0) return false;
        } else if (isPrime(i)) {
            if (p % i == 0) return false;
        }
    }
    return true;
}
fn isProbablyPrime(p: anytype) bool {
    if (p < bits.len) return bits[p];
    if (p & 1 == 0) return false;
    var i: usize = 3;
    while (i < bits.len and i * i < p) : (i += 2) {
        if (bits[i]) {
            if (p % i == 0) return false;
        }
    }
    return true;
}
pub fn smallestPrimeAtLeast(min: usize) usize {
    var i = (min & ~@as(usize, 1)) + 1;
    while (!isPrime(i)) : (i += 2) {}
    return i;
}
pub fn largestPrimeLessThan(max: usize) usize {
    if (max <= 4) return max - 1;
    var i = (max & ~@as(usize, 1)) - 1;
    while (!isPrime(i)) : (i -= 2) {}
    return i;
}
test "primes" {
    try std.testing.expect(!isPrime(1));
    try std.testing.expect(isPrime(2));
    try std.testing.expect(isPrime(3));
    try std.testing.expect(!isPrime(4));
    try std.testing.expect(isPrime(5));
    try std.testing.expect(!isPrime(6));
    try std.testing.expect(!isPrime(3215031751));
    try std.testing.expect(isPrime(4294836197));
    try std.testing.expect(!isPrime(4294836198));
    try std.testing.expect(!isPrime(4294836199));
    try std.testing.expect(isPrime(8589934583));
}
test "probable primes" {
    try std.testing.expect(isProbablyPrime(9223372036854775783));
    // 9223372036854775783 is actually prime, but isPrime is too slow for such a large number
    try std.testing.expect(!isProbablyPrime(9223372036854775784));
    try std.testing.expect(!isProbablyPrime(9223372036854775785));
    try std.testing.expect(!isProbablyPrime(9223372036854775787));
    try std.testing.expect(isProbablyPrime(9223372036854775777));
    try std.testing.expect(!isPrime(9223372036854775777));
}
test "primes less than" {
    try std.testing.expectEqual(largestPrimeLessThan(3), 2);
    try std.testing.expectEqual(largestPrimeLessThan(4), 3);
    try std.testing.expectEqual(largestPrimeLessThan(5), 3);
    try std.testing.expectEqual(largestPrimeLessThan(6), 5);
    try std.testing.expectEqual(largestPrimeLessThan(7), 5);
    try std.testing.expectEqual(largestPrimeLessThan(65535), 65521);
    try std.testing.expectEqual(largestPrimeLessThan(65535 * 65535), 4294836197);
    try std.testing.expectEqual(largestPrimeLessThan(0x100000000), 4294967291);
    try std.testing.expectEqual(largestPrimeLessThan(0x100000001), 4294967291);
    try std.testing.expectEqual(largestPrimeLessThan(0x200000001), 8589934583);
//    try std.testing.expectEqual(largestPrimeLessThan(0x7fffffffffffffff), 4294836197);
}
test "primes at least" {
    try std.testing.expectEqual(smallestPrimeAtLeast(5), 5);
    try std.testing.expectEqual(smallestPrimeAtLeast(6), 7);
    try std.testing.expectEqual(smallestPrimeAtLeast(4294836197 - 13), 4294836197);
}
test "prime diffs" {
    if (true) return error.SkipZigTest;
    var tests: usize = 0;
    var n: usize = 0;
    var p: usize = 16;
    for (&bits, 0..) |b, i| {
        if (b) {
            n += 1;
            tests += 1;
        }
        else if (i & 1 == 1) {
            tests += 1;
        }
        if (i + 1 == p) {
            std.debug.print("average bit tests - first {}: {d:.2} ({} primes)\n", .{ p, @as(f64, @floatFromInt(tests)) / @as(f64, @floatFromInt(n)) , n});
            p *= 16;
            tests = 0;
        }
    }
}

var prime: u32 = 0;
fn compareU64(l: u64, r: u64) Order {
    return math.order(l, r);
}
const Treap_u64 = treap.Treap(u64, u32, u0);
const stats_u32 = stats.Stats(u32);

var best = stats_u32.init(0);
pub fn main() void {
    var prng = std.rand.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        std.os.getrandom(std.mem.asBytes(&seed)) catch unreachable;
        break :blk seed;
    });
    const rand = prng.random();

    var memory_ = [_]Treap_u64.Element{undefined} ** (32767 * 16);
    var depths_ = [_]u32{0} ** 32767;
    var b: u5 = 5;
    while (b < 16) : (b += 1) {
        best = stats_u32.init(0);
        const k = (@as(u32, 1) << b) - 1;
        const memory = memory_[0 .. k * 16];
        const depths = depths_[0..k];
        stdout.print("Searching for optimal primes for treap of size {} optimal height {}\n", .{ k, b }) catch unreachable;
        const primes = [_]u32{
            0xa1fdc7a3,
            // 0x9e480773,
            // 0x95eac4e9,
            // 0x9e2fda23,
            // 0x9cfdb27d,

            // 0x6a14d18f,
            // 0x61c5a08b,
            // 2718047303,
            // 0x61fcc927,
            // 0x9e3a146b,
            // 0x9e37a06f,

            // 1605146423,
            // 2524803221,
            // 3383910533,
            // 2652726113,
            // 2652447943,
            // 1999999973,
        };
        for (primes) |v| {
            tryTreap(false, v, memory, depths);
        }
        var i: usize = 0; //100_000/@intCast(usize,b);
        while (i > 0) : (i -= 1) {
            const j = largestPrimeLessThan(rand.int(u32) | 0x80000000);
            tryTreap(false, @intCast(j), memory, depths);
        }
        // stdout.print("*",.{}) catch unreachable;
        // const max = 0x100000000;
        // i = max;
        // while (i > max/2)
        //     : (i -= 500*k) {
        //         i = largestPrimeLessThan(i);
        //         tryTreap(@intCast(u32,i),memory,depths);
        // }
    }
}
fn tryTreap(printAnyway: bool, i: u32, memory: []Treap_u64.Element, depths: []u32) void {
    prime = i;
    var trp = Treap_u64.init(memory, compareU64, 0);
    var index: u64 = 1;
    while (index < depths.len) : (index += 1) {
        _ = trp.insert(index) catch unreachable;
    }
    trp.depths(depths);
    var current = stats_u32.init(i);
    for (depths[1..]) |depth| {
        current.addData(@floatFromInt(depth));
    }
    var print = printAnyway;
    if (best.noData() or best.max() > current.max() or (best.max() == current.max() and best.mean() > current.mean())) {
        best = current;
        stdout.print("new best", .{}) catch unreachable;
        print = true;
    } else if (print) stdout.print("current", .{}) catch unreachable;
    if (print) stdout.print(" is {} 0x{x:0>8} with max={d:2.0} mean={d:5.2} stdev={d:5.2}\n", .{ i, i, current.max(), current.mean(), current.stddev() }) catch unreachable;
}
