const std = @import("std");

const Category = enum { int, float, ptr, imm };

fn dispatchIfElse(v: usize, x: usize) usize {
    if (v & 2 != 0) return x+%12345;      // 1, 2, 3, 6, 7
    if (v & 4 != 0) return x-%23456789;    // 4, 5
    if (v == 0) return x*%3453245343234232143;          // 0
    return x/234232143;                       // 1
}

fn dispatchSwitch(v: usize, x: usize) usize {
    switch (@as(u3,@truncate(v))) {
        2, 3, 6, 7 => {return x+%12345;},
        4, 5       => {return x-%23456789;},
        0          => {return x*%3453245343234232143;},
        1          => {return x/234232143;},
    }
}

fn dispatchSwitch2(v: usize, x: usize) usize {
    switch (@as(u3,@truncate(v))) {
        2,         => {return x+%12345;},
        4, 5       => {return x-%23456789;},
        0          => {return x*%3453245343234232143;},
        1          => {return x/234232143;},
        3, 6, 7    => unreachable,
    }
}

pub fn main() !void {
    var prng = std.Random.DefaultPrng.init(42);
    const random = prng.random();
    const iterations = 10_000_000; // Increased for stability

    const tags = try std.heap.page_allocator.alloc(usize, iterations);
    defer std.heap.page_allocator.free(tags);

    const intPercent = 70;
    const floatPercent = 20;
    const ptrPercent = 5;
    // Generate Biased Data
    for (tags) |*t| {
        const r = random.float(f32);
        if (r < intPercent*0.01) {
            // Pick a random integer tag: {2, 3, 6, 7}
            const int_tags = [_]u3{ 2,};// 3, 6, 7 };
            const i = random.int(usize);
            t.* = (i & 0xfffffffffc) + int_tags[random.int(usize) % int_tags.len];
        } else if (r < (intPercent+floatPercent)*0.01) {
            // Pick a random float tag: {4, 5}
            const float_tags = [_]u3{ 4, 5 };
            const i = random.int(usize);
            t.* = (i & 0xfffffffffc) + float_tags[random.int(usize) % float_tags.len];
        } else if (r < (intPercent+floatPercent+ptrPercent)*0.01) {
            const i = random.int(usize);
            t.* = (i & 0xfffffffffc); // ptr
        } else {
            const i = random.int(usize);
            t.* = (i & 0xfffffffffc) + 1; // imm
        }
    }

    var timer = try std.time.Timer.start();

    // Warm up the cache
    _ = dispatchIfElse(tags[0],0);

    const start_if = timer.read();
    var sum_if: usize = 0;
    for (tags) |tag| {
        sum_if = dispatchIfElse(tag,sum_if);
    }
    const end_if = timer.read();

    const start_sw = timer.read();
    var sum_sw: usize = 0;
    for (tags) |tag| {
        sum_sw = dispatchSwitch(tag,sum_sw);
    }
    const end_sw = timer.read();

    const start_sw2 = timer.read();
    var sum_sw2: usize = 0;
    for (tags) |tag| {
        sum_sw2 = dispatchSwitch2(tag,sum_sw2);
    }
    const end_sw2 = timer.read();

    std.debug.print("Distribution: {}% Int, {}% Float, {}% Ptr, {}% Imm\n", .{intPercent, floatPercent, ptrPercent, 100 - intPercent - floatPercent - ptrPercent});
    std.debug.print("If-Else: {d:>10} ns\n", .{end_if - start_if});
    std.debug.print("Switch:  {d:>10} ns\n", .{end_sw - start_sw});
    std.debug.print("Switch2: {d:>10} ns\n", .{end_sw2 - start_sw2});

    std.mem.doNotOptimizeAway(sum_if);
    std.mem.doNotOptimizeAway(sum_sw);
    std.mem.doNotOptimizeAway(sum_sw2);
}
