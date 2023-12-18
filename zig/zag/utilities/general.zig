const std = @import("std");
const phi = std.math.phi;
pub fn inversePhi(comptime T: type) T {
    if (T == u64) return 11400714819323198485; // the calculation gives 11400714819323197441
    switch (@typeInfo(T)) {
        .Int => |int_info| switch (int_info.signedness) {
            .unsigned => return @as(T, @intFromFloat(@as(f64, @floatFromInt(1 << int_info.bits)) / phi)) | 1,
            else => {},
        },
        else => {},
    }
    @compileError("invalid type for inversePhi: " ++ @typeName(T));
}
test "check inversePhi" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(inversePhi(u32), 2654435769);
    try expectEqual(inversePhi(u16), 40503);
    try expectEqual(inversePhi(u8), 159);
    try expectEqual(inversePhi(u64), 11400714819323198485);
}
const undoType = enum { immediate, euclidean, iterative };
pub fn undoPhi(comptime T: type) T {
    // there isn't a closed form way to calculate this, but
    // https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Computing_multiplicative_inverses_in_modular_structures
    // shows the Extended Eclidean algorithm can calculate in log time
    switch (undoType.immediate) {
        .immediate => switch (T) {
            u32 => return 340573321,
            u24 => return 11764425,
            u16 => return 30599,
            u8 => return 95,
            else => @compileError("invalid type for undoPhi: " ++ @typeName(T)),
        },
        .euclidean => {
            // function inverse(a, n)
            //     t := 0;     newt := 1
            //     r := n;     newr := a
            //     while newr ≠ 0 do
            //         quotient := r div newr
            //         (t, newt) := (newt, t − quotient × newt)
            //         (r, newr) := (newr, r − quotient × newr)
            //     if r > 1 then
            //         return "a is not invertible"
            //     if t < 0 then
            //         t := t + n
            //     return t
        },
        .iterative => {
            const phiT = inversePhi(T);
            var undoT: T = 1;
            var temp = phiT;
            while (temp != 1) {
                undoT *%= phiT;
                temp *%= phiT;
            }
            return undoT;
        },
    }
}
test "check undoPhi" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(undoPhi(u32), 340573321);
    try expectEqual(undoPhi(u24), 11764425);
    try expectEqual(undoPhi(u16), 30599);
    try expectEqual(undoPhi(u8), 95);
}
test "randomness of /phi - all values enumerated" {
    var data16 = [_]u16{0} ** 65536;
    var counts = [_]u32{0} ** 2;
    const phi16 = inversePhi(u16);
    for (data16, 0..) |_, index| {
        data16[@as(u16, @truncate(index)) *% phi16] += 1;
    }
    for (data16) |count| {
        counts[count] += 1;
    }
    const expectEqual = @import("std").testing.expectEqual;
    try expectEqual(counts[1], 65536);
    var data8 = [_]u16{0} ** 256;
    const phi8 = inversePhi(u8);
    for (data8, 0..) |_, index| {
        data8[@as(u8, @truncate(index)) *% phi8] += 1;
    }
    counts[1] = 0;
    for (data8) |count| {
        counts[count] += 1;
    }
    try expectEqual(counts[1], 256);
}
test "undo phi" {
    const expectEqual = @import("std").testing.expectEqual;
    const phi8 = inversePhi(u8);
    const undo8 = undoPhi(u8);
    try expectEqual(phi8 *% undo8, 1);
    try expectEqual((42 *% phi8) *% undo8, 42);
    try expectEqual((128 *% phi8) *% undo8, 128);
    const phi16 = inversePhi(u16);
    const undo16 = undoPhi(u16);
    try expectEqual(phi16 *% undo16, 1);
    try expectEqual((42 *% phi16) *% undo16, 42);
    try expectEqual((42321 *% phi16) *% undo16, 42321);
    const phi24 = inversePhi(u24);
    const undo24 = undoPhi(u24);
    try expectEqual(phi24 *% undo24, 1);
    try expectEqual((42 *% phi24) *% undo24, 42);
    try expectEqual((3242321 *% phi24) *% undo24, 3242321);
    const phi32 = inversePhi(u32);
    const undo32 = undoPhi(u32);
    try expectEqual(phi32 *% undo32, 1);
    try expectEqual((42 *% phi32) *% undo32, 42);
    try expectEqual((123242321 *% phi32) *% undo32, 123242321);
}
inline fn po2(size: anytype, comptime not1: u1) @TypeOf(size) {
    var n: @TypeOf(size) = if (size == 0) 0 else size - 1;
    n |= @as(@TypeOf(n), not1);
    const bits = @typeInfo(@TypeOf(size)).Int.bits;
    if (comptime bits > 32) n |= n >> 32;
    if (comptime bits > 16) n |= n >> 16;
    if (comptime bits > 8) n |= n >> 8;
    if (comptime bits > 4) n |= n >> 4;
    n |= n >> 2;
    n |= n >> 1;
    return n + 1;
}
pub inline fn largerPowerOf2(size: anytype) @TypeOf(size) {
    return po2(size, 0);
}
test "check largerPowerOf2" {
    const expectEqual = std.testing.expectEqual;
    const t4092: u16 = 4092; // should work with u12
    try expectEqual(largerPowerOf2(t4092), 4096);
    try expectEqual(largerPowerOf2(@as(u16, 1)), 1);
    try expectEqual(largerPowerOf2(@as(u16, 16)), 16);
    try expectEqual(largerPowerOf2(@as(u16, 33)), 64);
    try expectEqual(largerPowerOf2(@as(u16, 255)), 256);
}
pub inline fn smallerPowerOf2(size: anytype) @TypeOf(size) {
    return po2(size / 2 + 1, 0);
}
test "check smallerPowerOf2" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(smallerPowerOf2(@as(u16, 1)), 1);
    try expectEqual(smallerPowerOf2(@as(u16, 16)), 16);
    try expectEqual(smallerPowerOf2(@as(u16, 33)), 32);
    try expectEqual(smallerPowerOf2(@as(u16, 255)), 128);
}
pub inline fn largerPowerOf2Not1(size: anytype) @TypeOf(size) {
    return po2(size, 1);
}
test "check largerPowerOf2Not1" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(largerPowerOf2Not1(@as(u16, 1)), 2);
    try expectEqual(largerPowerOf2Not1(@as(u16, 2)), 2);
    try expectEqual(largerPowerOf2Not1(@as(u16, 16)), 16);
    try expectEqual(largerPowerOf2Not1(@as(u16, 33)), 64);
    try expectEqual(largerPowerOf2Not1(@as(u16, 255)), 256);
}
pub inline fn bitsToRepresent(size: anytype) u7 {
    const T = @TypeOf(size);
    switch (@typeInfo(T)) {
        .ComptimeInt => {
            comptime var n = size;
            n |= n >> 32;
            n |= n >> 16;
            n |= n >> 8;
            n |= n >> 4;
            n |= n >> 2;
            n |= n >> 1;
            return comptime @ctz(~@as(u64, n));
        },
        .Int => |int_info| switch (int_info.signedness) {
            .unsigned => return @intCast(int_info.bits - @clz(size)),
            else => {},
        },
        else => {},
    }
    @compileError("bitsToRepresent not implemented for " ++ @typeName(T));
}
test "check bitsToRepresent" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(bitsToRepresent(15), 4);
    try expectEqual(bitsToRepresent(@as(u16, 1)), 1);
    try expectEqual(bitsToRepresent(@as(u16, 15)), 4);
    try expectEqual(bitsToRepresent(@as(u16, 16)), 5);
    try expectEqual(bitsToRepresent(@as(u16, 17)), 5);
    try expectEqual(bitsToRepresent(@as(u64, 4000)), 12);
}
pub const crc24 = std.hash.crc.Crc24Openpgp.hash;
