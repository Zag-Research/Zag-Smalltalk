const std = @import("std");
const os = std.os;
//    const allocator = @import("std").heap.page_allocator;
//    bits = allocator.alloc(bool, n) catch @panic("Failed Allocating");
//    defer allocator.free(bits);

// define Fibonacci sequence tables
// almost all allocations will be for sizes <= 65535 words long
// so define the sequence up to that size in u16s and unroll the search
const fibonacci_u16 = init: {
    var initial_value: [23]u16 = undefined;
    initFibs(u16, initial_value[0..]);
    break :init initial_value;
}; //[_]u16{ 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368};
// but just in case it's a bigger allocation, match everything up to the maximum u64 value
pub const n_u48_fibonacci = findFib(0xffff_ffff_ffff) + 1; //69+1;
pub const findFib = findFib2;
const fibonacci_u64 = init: {
    var initial_value: [93]u64 = undefined;
    initFibs(u64, initial_value[0..92]);
    initial_value[92] = 0xffff_ffff_ffff_ffff;
    break :init initial_value;
};
fn initFibs(comptime T: type, array: []T) void {
    array[0] = 1;
    array[1] = 2;
    const start = 2;
    for (array[start..], 0..) |*value, index| {
        value.* = array[index + start - 2] + array[index + start - 1];
    }
}
fn findFib1(target: u64) usize {
    for (fibonacci_u64[0..], 0..) |value, index| {
        if (value >= target) return index;
    }
    unreachable;
}
inline fn search(comptime low: usize, comptime high: usize, t_16: u16) u16 {
    if (low < high) {
        const mid = (low + high) / 2;
        if (t_16 > fibonacci_u16[mid]) {
            return search(mid + 1, high, t_16);
        } else {
            return search(low, mid, t_16);
        }
    }
    return low;
}
fn findFib2(target: u64) usize {
    const start = fibonacci_u16.len - 1;
    if (target <= fibonacci_u16[start])
        return search(0, start, @as(u16, @truncate(target)));
    for (fibonacci_u64[start..], 0..) |value, index| {
        if (value >= target) return index + start;
    }
    unreachable;
}
const fib_index = [_]u8{ 0, 0, 1, 2, 3, 3, 4, 4, 4, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7, 7 };
fn findFib6(target: u64) usize {
    if (target <= 34) return fib_index[target];
    return findFib2(target);
}
test "fibonacci values" {
    //    const stdout = @import("std").io.getStdOut().writer();
    //    try stdout.print("\nfibonacci_u16 {} {any}\n", .{fibonacci_u16.len,fibonacci_u16});
    //    try stdout.print("fibonacci_u64 {} {any}\n", .{fibonacci_u64.len,fibonacci_u64});
    try std.testing.expectEqual(findFib1(1), 0);
    try std.testing.expectEqual(findFib1(2), 1);
    try std.testing.expectEqual(findFib1(3), 2);
    try std.testing.expectEqual(findFib1(4), 3);
    try std.testing.expectEqual(findFib1(5), 3);
    try std.testing.expectEqual(findFib1(12), 5);
    try std.testing.expectEqual(findFib1(13), 5);
    try std.testing.expectEqual(findFib1(14), 6);
    try std.testing.expectEqual(findFib1(10945), 19);
    try std.testing.expectEqual(findFib1(10946), 19);
    try std.testing.expectEqual(findFib1(10947), 20);
    try std.testing.expectEqual(findFib1(17711), 20);
    try std.testing.expectEqual(findFib1(17712), 21);
    try std.testing.expectEqual(findFib1(28657), 21);
    try std.testing.expectEqual(findFib1(28658), 22);
    try std.testing.expectEqual(findFib1(46368), 22);
    try std.testing.expectEqual(findFib1(46369), 23);
    try std.testing.expectEqual(findFib1(75025), 23);
    try std.testing.expectEqual(findFib1(75026), 24);
    try std.testing.expectEqual(findFib1(75026000), 38);
    try std.testing.expectEqual(findFib1(75026000000), 52);
    try std.testing.expectEqual(findFib1(0xffff_ffff_ffff), 69);
    try std.testing.expectEqual(findFib1(0xffff_ffff_ffff_ffff), 92);
}
test "fibonacci2 values" {
    var fib: usize = 1;
    while (fib < 200000) : (fib += 1) {
        try std.testing.expectEqual(findFib1(fib), findFib2(fib));
    }
}
test "fibonacci6 values" {
    var fib: usize = 1;
    while (fib < 200000) : (fib += 1) {
        try std.testing.expectEqual(findFib1(fib), findFib6(fib));
    }
}
//test "timing" {
//    try time(400_000);
//}
pub fn main() !void {
    try time(1_000_000 * os.argv.len);
}
fn genRandom(sizes: []u64) void {
    //     var prng = std.rand.DefaultPrng.init(blk: {
    //        var seed: u64 = undefined;
    //        try std.os.getrandom(std.mem.asBytes(&seed));
    //        break :blk seed;
    //    });
    var prng = std.rand.DefaultPrng.init(1625953);
    const rand = prng.random();
    for (sizes) |*value| {
        value.* = @as(u64, @intFromFloat((75000 * std.math.exp(-13 * rand.float(f32)) + 2)));
    }
}
fn time(count: u64) !void {
    const stdout = std.io.getStdOut().writer();
    const ts = std.time.nanoTimestamp;
    var sizes: [50]u64 = undefined;
    genRandom(sizes[0..]);
    var result: u64 = 0;
    std.mem.doNotOptimizeAway(&result);
    var loop = count * 10;
    while (loop > 0) : (loop -= 1) {
        for (sizes[0..]) |value| {
            result +%= findFib1(value);
        }
    }
    loop = count;
    var start = ts();
    while (loop > 0) : (loop -= 1) {
        for (sizes[0..]) |value| {
            result +%= findFib1(value);
        }
    }
    try stdout.print("\nruntime  linear: {d:12}\n", .{ts() - start});
    loop = count;
    start = ts();
    while (loop > 0) : (loop -= 1) {
        for (sizes[0..]) |value| {
            result +%= findFib2(value);
        }
    }
    try stdout.print("comptime binary: {d:12}\n", .{ts() - start});
    loop = count;
    start = ts();
    while (loop > 0) : (loop -= 1) {
        for (sizes[0..]) |value| {
            result +%= findFib6(value);
        }
    }
    try stdout.print("shortcut binary: {d:12}\n", .{ts() - start});
}
