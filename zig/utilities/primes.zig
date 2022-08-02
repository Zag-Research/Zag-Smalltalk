const std = @import("std");
const math = std.math;
const Order = math.Order;
const stdout = std.io.getStdOut().writer();
//const expect = @import("std").testing.expect;
const treap = @import("treap.zig");
const stats = @import("stats.zig");

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
fn isPrime(p: u32) bool {
    if (p < bits.len) return bits[p];
    if (p % 2 == 0) return false;
    var i: usize = 3;
    while (i * i < p) : (i += 2) {
        if (bits[i]) {
            if (p % i == 0) return false;
        }
    }
    return true;
}
fn largestPrimeLessThan(max: u32) u32 {
    if (max <= 4) return max - 1;
    var i = (max & ~@as(u32, 1)) - 1;
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
    try std.testing.expectEqual(largestPrimeLessThan(65535),65521);
    try std.testing.expectEqual(largestPrimeLessThan(65535*65535),4294836197);
}

var prime: u32 = 0;
fn priority(pos:u32) u32 {
    return (pos+1)*%prime;
}
fn compareU64(l: u64, r: u64) Order {
    return math.order(l,r);
}
const Treap_u64 = treap.Treap(u64);
const stats_u32 = stats.Stats(u32);

var best = stats_u32.init(0);
pub fn main() void {
    var prng = std.rand.DefaultPrng.init(blk: {
        var seed: u64 = undefined;
        std.os.getrandom(std.mem.asBytes(&seed)) catch unreachable;
        break :blk seed;
    });
    const rand = prng.random();
    
    var memory_ = [_]u8{0} ** (32767*16);
    var depths_ = [_]u32{0} ** 32767;
    var b : u5 = 5;
    while (b<16) : (b += 1) {
        best = stats_u32.init(0);
        const k = (@as(u32,1)<<b)-1;
        const memory = memory_[0..k*16];
        const depths = depths_[0..k];
        stdout.print("Searching for optimal primes for treap of size {} optimal height {}\n",.{k,b}) catch unreachable; 
        const primes = [_]u32{0xa1fdc7a3,
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
            tryTreap(false,v,memory,depths);
        }
        var i: usize = 0; //100_000/@intCast(usize,b);
        while (i > 0)
            : (i -= 1) {
                const j = largestPrimeLessThan(rand.int(u32)|0x80000000);
                tryTreap(false,@intCast(u32,j),memory,depths);
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
fn tryTreap(printAnyway: bool,i: u32, memory: []u8, depths: []u32) void {
    prime =i;
    var trp = Treap_u64.init(memory,compareU64,0,priority);
    var index : u64 = 1;
    while (index<depths.len) : (index += 1) {
        _ = trp.insert(index) catch unreachable;
    }
    trp.depths(depths);
    var current = stats_u32.init(i);
    for (depths[1..]) |depth| {
        current.addData(@intToFloat(f64,depth));
    }
    var print = printAnyway;
    if (best.noData() or best.max()>current.max() or (best.max()==current.max() and best.mean()>current.mean())) {
        best = current;
        stdout.print("new best",.{}) catch unreachable;
        print = true;
    } else
        if (print) stdout.print("current",.{}) catch unreachable;
    if (print) stdout.print(" is {} 0x{x:0>8} with max={d:2.0} mean={d:5.2} stdev={d:5.2}\n", .{i,i,current.max(),current.mean(),current.stddev()}) catch unreachable;
}
fn gen_primes(comptime T : type, n_primes: usize) [n_primes]T {
    var p : [n_primes]T = undefined;
    var possible : T = 3;
    var previous : T = 0;
    var i: usize = 0;
    if (n_primes>13) @setEvalBranchQuota(100000);
    next:
        while (true) : (possible += 2) {
            var j: usize = 3;
            while (j*j <= possible) : (j += 2) {
                if (possible%j==0) continue :next;
            }
            if (possible < previous * 13 / 10) continue :next;
            previous = possible;
            p[i] =  possible;
            i += 1;
            if (i>=n_primes) return p;
    }
}
const primes_type = u16;
const prime_values = gen_primes(primes_type,if (builtin.is_test) 15 else 22);
const default_prime = 11959; // max size of dispatch table - must be less than 32767
pub fn next_prime_larger_than(n : primes_type) primes_type {
    var low : usize = 0;
    var high : usize = prime_values.len-1;
    while (low<=high) {
        const mid = (low+high)/2;
        if (mid==0) return prime_values[0];
        if (prime_values[mid]>=n) {
            if (prime_values[mid-1]<n) return prime_values[mid];
            high=mid;
        } else
            low=mid+1;
    }
    return default_prime;
}
test "primes" {
//    const stdout = @import("std").io.getStdOut().writer();
//    try stdout.print("primes: {any}\n",.{prime_values});
    const expectEqual = @import("std").testing.expectEqual;
    try expectEqual(next_prime_larger_than(1),3);
    try expectEqual(next_prime_larger_than(3),3);
    try expectEqual(next_prime_larger_than(6),7);
    try expectEqual(next_prime_larger_than(24),29);
    try expectEqual(next_prime_larger_than(167),167);
    try expectEqual(next_prime_larger_than(224),293);
    if (prime_values.len<22) {
        try expectEqual(next_prime_larger_than(294),default_prime);
    } else
        try expectEqual(next_prime_larger_than(1889),1889);
    try expectEqual(next_prime_larger_than(1890),default_prime);
}
