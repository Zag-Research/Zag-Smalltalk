const std = @import("std");
const zag = @import("../zag.zig");
const Process = zag.Process;
const Object = zag.object.Object;
const heap = zag.heap;
const Age = heap.Age;
const HeapHeader = heap.HeapHeader;
const HeapObjectPtr = heap.HeapObjectPtr;
const c = zag.object.ClassIndex;
const compileRaw = zag.execute.compileRaw;
const si = c.SmallInteger;
pub const ZERO = compileRaw(.{ si, 0 });
pub const False = compileRaw(.{c.False});
pub const True = compileRaw(.{c.True});
pub const Nil = compileRaw(.{c.UndefinedObject});
const SmallIntegerCache = compileRaw(.{
    si, -5,
    si, -4,
    si, -3,
    si, -2,
    si, -1,
    si, 0,
    si, 1,
    si, 2,
    si, 3,
    si, 4,
    si, 5,
    si, 6,
    si, 7,
    si, 8,
    si, 9,
    si, 10,
    si, 11,
    si, 12,
    si, 13,
    si, 14,
    si, 15,
    si, 16,
    si, 17,
    si, 18,
    si, 19,
    si, 20,
    si, 21,
    si, 22,
    si, 23,
    si, 24,
    si, 25,
    si, 26,
    si, 27,
    si, 28,
    si, 29,
    si, 30,
    si, 31,
    si, 32,
    si, 33,
    si, 34,
    si, 35,
    si, 36,
    si, 37,
    si, 38,
    si, 39,
    si, 40,
    si, 41,
    si, 42,
});
const SICacheMin = -5;
const SICacheMax = 42;
pub inline fn int(i: i64, maybeProcess: ?*Process) HeapObjectPtr {
    if (SICacheMin <= i and i <= SICacheMax)
        return @ptrCast(@constCast(&SmallIntegerCache.objects[(i - SICacheMin) << 1]));
    if (maybeProcess) | process | {
        const allocReturn = process.alloc(.SmallInteger, 1, null, Object, false);
        _ = .{allocReturn, unreachable};
    }
    unreachable;
}
test "inMemory int()" {
    std.debug.print("inMemory int()\n",.{});
}

pub const MemoryFloat = struct {
    header: HeapHeader,
    value: f64,
};
pub inline fn simpleFloat(v: f64, age: Age) MemoryFloat {
    const u: u64 = @bitCast(v);
    const hash: u24 = @truncate(u ^ (u >> 24) ^ (u >> 48));
    return .{
        .header = .{ .classIndex = .Float, .hash = hash, .format = .notIndexable, .age = age, .length = 1 },
        .value = v,
    };
}
pub const nanMemObject = simpleFloat(std.math.nan(f64), .static);
pub const pInfMemObject = simpleFloat(std.math.inf(f64), .static);
pub const nInfMemObject = simpleFloat(-std.math.inf(f64), .static);
