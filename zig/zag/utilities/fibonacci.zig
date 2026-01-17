const std = @import("std");
const os = std.os;
//    const allocator = @import("std").heap.page_allocator;
//    bits = allocator.alloc(bool, n) catch @panic("Failed Allocating");
//    defer allocator.free(bits);

pub const findFib = findFib7;
// define Fibonacci sequence tables
// almost all allocations will be for sizes <= 65535 words long
// so define the sequence up to that size in u16s and unroll the search
const fibonacci_u16 = init: {
    var initial_value: [23]u16 = undefined;
    initFibs(u16, initial_value[0..]);
    break :init initial_value;
}; //[_]u16{ 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, 144, 233, 377, 610, 987, 1597, 2584, 4181, 6765, 10946, 17711, 28657, 46368};
// but just in case it's a bigger allocation, match everything up to the maximum u64 value
const fib_index = [_]u8{ 0, 0, 1, 2} ++ [_]u8{3} ** 2 ++ [_]u8{4} ** 3 ++ [_]u8{5} ** 5 ++
                    [_]u8{6} ** 8 ++ [_]u8{7} ** 13 ++ [_]u8{8} ** 21 ++ [_]u8{9} ** 34 ++ [_]u8{10} ** 55;
const fibonacci_u64 = init: {
    var initial_value: [93]u64 = undefined;
    initFibs(u64, initial_value[0..92]);
    initial_value[92] = 0xffff_ffff_ffff_ffff;
    break :init initial_value;
};
fn initFibs(comptime T: type, array: []T) void {
    array[0] = 1;
    array[1] = 2;
    for (array[2..], 2..) |*value, index| {
        value.* = array[index - 2] + array[index - 1];
    }
}
fn findFib1(target: u64) usize {
    for (fibonacci_u64[0..], 0..) |value, index| {
        if (value >= target) return index;
    }
    @panic("unreachable");
}
inline fn search(T: type, comptime low: usize, comptime high: usize, target: T, comptime buffer: []const T) usize {
    if (low < high) {
        const mid = (low + high) / 2;
        if (target > buffer[mid]) {
            return search(T, mid + 1, high, target, buffer);
        } else {
            return search(T, low, mid, target, buffer);
        }
    }
    return low;
}
inline fn binarySearch(comptime T: type, target: T, comptime buffer: []const T) usize {
        var low: usize = 0;
        var high: usize = buffer.len;

        while (low < high) {
            const mid = low + (high - low) / 2;
            if (buffer[mid] < target) {
                low = mid + 1;
            } else {
                high = mid;
            }
        }
        return low;
    }
fn findFib2(target: u64) usize {
    const start = fibonacci_u16.len - 1;
    if (target <= fibonacci_u16[start])
        return search(u16, 0, start, @truncate(target), &fibonacci_u16);
    for (fibonacci_u64[start..], start..) |value, index| {
        if (value >= target) return index;
    }
    @panic("unreachable");
}
fn findFib3(target: u64) usize {
    var high = fibonacci_u16.len - 1;
    if (target <= fibonacci_u16[high]) {
        var low: usize = 0;
        while (low < high) {
            const mid = (low + high) / 2;
            if (target > fibonacci_u16[mid]) {
                low = mid + 1;
            } else {
                high = mid;
            }
        }
        return low;
    }
    for (fibonacci_u64[high..], high..) |value, index| {
        if (value >= target) return index;
    }
    @panic("unreachable");
}
fn findFib4(target: u64) usize {
    return search(u64, 0, fibonacci_u64.len - 1, @truncate(target), &fibonacci_u64);
}
fn findFib5(target: u64) usize {
    return binarySearch(u64, @truncate(target), &fibonacci_u64);
}
fn findFib6(target: u64) usize {
    if (target < fib_index.len) return fib_index[target];
    return findFib2(target);
}
fn findFib7(target: u64) usize {
    if (target < fib_index.len) return fib_index[target];
    // skip the ones handled by the above.
    return search(u64, fib_index[fib_index.len - 1] + 1, fibonacci_u64.len - 1, @truncate(target), &fibonacci_u64);
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
    while (fib < 100000) : (fib += 1) {
        try std.testing.expectEqual(findFib1(fib), findFib2(fib));
    }
}
test "fibonacci3 values" {
    var fib: usize = 1;
    while (fib < 100000) : (fib += 1) {
        try std.testing.expectEqual(findFib1(fib), findFib3(fib));
    }
}
test "fibonacci4 values" {
    var fib: usize = 1;
    while (fib < 100000) : (fib += 1) {
        try std.testing.expectEqual(findFib1(fib), findFib4(fib));
    }
}
test "fibonacci5 values" {
    var fib: usize = 1;
    while (fib < 100000) : (fib += 1) {
        try std.testing.expectEqual(findFib1(fib), findFib5(fib));
    }
}
test "fibonacci6 values" {
    var fib: usize = 1;
    while (fib < 100000) : (fib += 1) {
        try std.testing.expectEqual(findFib1(fib), findFib6(fib));
    }
}
test "fibonacci7 values" {
    var fib: usize = 1;
    while (fib < 100000) : (fib += 1) {
        try std.testing.expectEqual(findFib1(fib), findFib7(fib));
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
    var prng = std.Random.DefaultPrng.init(1625953);
    const rand = prng.random();
    for (sizes) |*value| {
        value.* = @as(u64, @intFromFloat((75000 * std.math.exp(-13 * rand.float(f32)) + 2)));
    }
}
fn time(count: u64) !void {
    var stdout_buffer: [1024]u8 = undefined;
    var stdout_writer = std.fs.File.stdout().writer(&stdout_buffer);
    const stdout = &stdout_writer.interface;
    const ts = std.time.nanoTimestamp;
    var sizes: [100]u64 = undefined;
    genRandom(sizes[0..]);
    try stdout.print("sizes: {any}\n", .{sizes});
    var result: u64 = 412345;
    var loop = count * 10;
    while (loop > 0) : (loop -= 1) {
        for (sizes[0..]) |value| {
            result = std.math.rotr(u64, result, findFib1(value));
            std.mem.doNotOptimizeAway(result);
        }
    }
    loop = count;
    var start = ts();
    while (loop > 0) : (loop -= 1) {
        for (sizes[0..]) |value| {
            result = std.math.rotr(u64, result, findFib1(value));
            std.mem.doNotOptimizeAway(result);
        }
    }
    try stdout.print("\nruntime  linear:  {d:12}\n", .{ts() - start});
    loop = count;
    start = ts();
    while (loop > 0) : (loop -= 1) {
        for (sizes[0..]) |value| {
            result = std.math.rotr(u64, result, findFib2(value));
            std.mem.doNotOptimizeAway(result);
        }
    }
    try stdout.print("comptime binary:  {d:12}\n", .{ts() - start});
    loop = count;
    start = ts();
    while (loop > 0) : (loop -= 1) {
        for (sizes[0..]) |value| {
            result = std.math.rotr(u64, result, findFib3(value));
            std.mem.doNotOptimizeAway(result);
        }
    }
    try stdout.print("unrolled binary3: {d:12}\n", .{ts() - start});
    loop = count;
    start = ts();
    while (loop > 0) : (loop -= 1) {
        for (sizes[0..]) |value| {
            result = std.math.rotr(u64, result, findFib4(value));
            std.mem.doNotOptimizeAway(result);
        }
    }
    try stdout.print("unrolled binary4: {d:12}\n", .{ts() - start});
    loop = count;
    start = ts();
    while (loop > 0) : (loop -= 1) {
        for (sizes[0..]) |value| {
            result = std.math.rotr(u64, result, findFib5(value));
            std.mem.doNotOptimizeAway(result);
        }
    }
    try stdout.print("unrolled binary5: {d:12}\n", .{ts() - start});
    loop = count;
    start = ts();
    while (loop > 0) : (loop -= 1) {
        for (sizes[0..]) |value| {
            result = std.math.rotr(u64, result, findFib6(value));
            std.mem.doNotOptimizeAway(result);
        }
    }
    try stdout.print("shortcut binary:  {d:12}\n", .{ts() - start});
    loop = count;
    start = ts();
    while (loop > 0) : (loop -= 1) {
        for (sizes[0..]) |value| {
            result = std.math.rotr(u64, result, findFib7(value));
            std.mem.doNotOptimizeAway(result);
        }
    }
    try stdout.print("shortcut binary7: {d:12}\n", .{ts() - start});
    try stdout.flush();
}
