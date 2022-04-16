const std = @import("std");
//    const allocator = @import("std").heap.page_allocator;
//    bits = allocator.alloc(bool, n) catch @panic("Failed Allocating");
//    defer allocator.free(bits);

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

// define Fibonacci sequence tables
// almost all allocations will be for sizes <= 65535 words long
// so define the sequence up to that size in u16s and unroll the search
const fibonacci_u16 = init: {
    var initial_value: [24]u16 = undefined;
    initFibs(u16,initial_value[0..23]);
    initial_value[23] = 0xffff;
    break :init initial_value;
};
// but just in case it's a bigger allocation, match everything up to the maximum u64 value
const fibonacci_u64 = init: {
    var initial_value: [93]u64 = undefined;
    initFibs(u64,initial_value[0..92]);
    initial_value[92] = 0xffff_ffff_ffff_ffff;
    break :init initial_value;
};
fn initFibs(comptime T: type,array: []T) void {
    array[0] = 1;
    array[1] = 2;
    const start = 2;
    for(array[start..]) |*value,index| {
        value.* = array[index+start-2] + array[index+start-1];
    }
}
fn findFib(size: u64) usize {
    if (size<=fibonacci_u16[fibonacci_u16.len-1]) {
        const s_16 = @intCast(u16,size);
        inline for (fibonacci_u16) |value,index| {
            if (value>=s_16) return index;
        }
        unreachable;
    }
    const start = fibonacci_u16.len-1;
    for (fibonacci_u64[start..]) |value,index| {
        if (value>=size) return index+start;
    }
    unreachable;
}
test "fibonacci sizes" {
//    const stdout = @import("std").io.getStdOut().writer();
//    try stdout.print("\nfibonacci_u16 {} {any}\n", .{fibonacci_u16.len,fibonacci_u16});
//    try stdout.print("fibonacci_u64 {} {any}\n", .{fibonacci_u64.len,fibonacci_u64});
    try std.testing.expectEqual(findFib(5),3);
    try std.testing.expectEqual(findFib(12),5);
    try std.testing.expectEqual(findFib(13),5);
    try std.testing.expectEqual(findFib(14),6);
    try std.testing.expectEqual(findFib(10945),19);
    try std.testing.expectEqual(findFib(10946),19);
    try std.testing.expectEqual(findFib(10947),20);
    try std.testing.expectEqual(findFib(17711),20);
    try std.testing.expectEqual(findFib(17712),21);
    try std.testing.expectEqual(findFib(28657),21);
    try std.testing.expectEqual(findFib(28658),22);
    try std.testing.expectEqual(findFib(46368),22);
    try std.testing.expectEqual(findFib(46369),23);
    try std.testing.expectEqual(findFib(75025),23);
    try std.testing.expectEqual(findFib(75026),24);
}
