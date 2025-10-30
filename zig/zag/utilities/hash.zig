const std = @import("std");
fn fact(n: u64) f64 {
    var result: f64 = 1.0;
    var i: u64 = 2;
    while (i <= n) : (i += 1) {
        result *= @floatFromInt(i);
    }
    return result;
}
fn prob(n: u64, k: u64) f64 {
    return (fact(n) / fact(k) / fact(n - k)) * pow(0.5, n);
}
fn pow(base: f64, exponent: u64) f64 {
    return @exp(@log(base) * @as(f64, @floatFromInt(exponent)));
}
const Sequence = struct {
    ptr: *anyopaque,
    vtable: *const VTable,
    const VTable = struct {
        next32: *const fn (*anyopaque) ?u32,
        next24: *const fn (*anyopaque) ?u24,
        next16: *const fn (*anyopaque) ?u16,
        next8: *const fn (*anyopaque) ?u8,
    };
    pub inline fn next32(seq: Sequence) ?u32 {
        return seq.vtable.next32(seq.ptr);
    }
    pub inline fn next24(seq: Sequence) ?u24 {
        return seq.vtable.next24(seq.ptr);
    }
    pub inline fn next16(seq: Sequence) ?u16 {
        return seq.vtable.next16(seq.ptr);
    }
    pub inline fn next8(seq: Sequence) ?u8 {
        return seq.vtable.next8(seq.ptr);
    }
};
const BitPattern = struct {
    current: u32 = 0,
    aux: u32 = 1,
    auxBits: u32 = 1,
    state: State = .sliding,
    complement: bool = false,
    const State = enum {
        sliding,
        block,
        end,
    };
    const maxInt = std.math.maxInt;
    pub fn sequence(self: *BitPattern) Sequence {
        return .{
            .ptr = self,
            .vtable = &.{
                .next32 = next32,
                .next24 = next24,
                .next16 = next16,
                .next8 = next8,
            },
        };
    }
    fn next(self: *BitPattern, T: anytype) ?T {
        //const limit = maxInt(T);
        const bits = @bitSizeOf(T);
        if (self.complement) {
            self.complement = false;
            return @truncate(~self.current);
        }
        sw: switch (self.state) {
            .sliding => {
                const result: T = @truncate(if (self.current == 0) self.aux else self.current << 1);
                if (@popCount(result) < self.auxBits or ~result == self.aux) {
                    if (self.auxBits * 2 < bits) {
                        self.current = 0;
                        self.auxBits += 1;
                        self.aux = (self.aux << 1) | 1;
                        continue :sw .sliding;
                    }
                    self.current = 0;
                    self.aux = 11;
                    self.state = .block;
                    continue :sw .block;
                }
                self.current = result;
                self.complement = true;
                return result;
            },
            // .block => {
            //     if (self.current == 0) self.current = 1 else self.current <<= 1;
            //     if (self.current & limit == 0) {
            //         self.state = .block;
            //         continue :sw .block;
            //     }
            //     return self.current;
            // },
            .end => {
                return null;
            },
            else => {
                self.state = .end;
                continue :sw .end;
            },
        }
    }
    fn next32(ptr: *anyopaque) ?u32 {
        const self: *BitPattern = @ptrCast(@alignCast(ptr));
        if (self.next(u32)) |value| return value;
        return null;
    }
    fn next24(ptr: *anyopaque) ?u24 {
        const self: *BitPattern = @ptrCast(@alignCast(ptr));
        if (self.next(u24)) |value| return value;
        return null;
    }
    fn next16(ptr: *anyopaque) ?u16 {
        const self: *BitPattern = @ptrCast(@alignCast(ptr));
        if (self.next(u16)) |value| return value;
        return null;
    }
    fn next8(ptr: *anyopaque) ?u8 {
        const self: *BitPattern = @ptrCast(@alignCast(ptr));
        if (self.next(u8)) |value| return value;
        return null;
    }
};
const Sequential = struct {
    current: u32 = 0,
    complete: bool = false,
    pub fn sequence(self: *Sequential) Sequence {
        return .{
            .ptr = self,
            .vtable = &.{
                .next32 = next32,
                .next24 = next24,
                .next16 = next16,
                .next8 = next8,
            },
        };
    }
    fn put(self: *Sequential, next: u32) void {
        if (next == 0) self.complete = true;
        self.current = next;
    }
    fn next32(ptr: *anyopaque) ?u32 {
        const self: *Sequential = @ptrCast(@alignCast(ptr));
        if (self.complete) return null;
        const result = self.current;
        self.put(result +% 1);
        return result;
    }
    fn next24(ptr: *anyopaque) ?u24 {
        const self: *Sequential = @ptrCast(@alignCast(ptr));
        if (self.complete) return null;
        const result: u24 = @truncate(self.current);
        self.put(result +% 1);
        return result;
    }
    fn next16(ptr: *anyopaque) ?u16 {
        const self: *Sequential = @ptrCast(@alignCast(ptr));
        if (self.complete) return null;
        const result: u16 = @truncate(self.current);
        self.put(result +% 1);
        return result;
    }
    fn next8(ptr: *anyopaque) ?u8 {
        const self: *Sequential = @ptrCast(@alignCast(ptr));
        if (self.complete) return null;
        const result: u8 = @truncate(self.current);
        self.put(result +% 1);
        return result;
    }
};
fn showSequence(sequence: Sequence) void {
    while (sequence.next8()) |value| {
        std.log.err("{x} ", .{value});
    }
    std.log.err("\n", .{});
}
// From: "The strict avalanche criterion randomness test"
//     February 2005Mathematics and Computers in Simulation 68(1):1-7
// DOI:10.1016/j.matcom.2004.09.001

fn SAC_test(T: type, rng: fn (T) T, order: u64) f64 {
    const bits = @typeInfo(T).int.bits;
    const m = 10000;
    var previous = rng(0);
    var eacount: [bits + 1]f64 = undefined;
    var expected: f64 = 0.0;
    var suma: f64 = 0.0;
    var chi: f64 = 0.0;
    for (0..bits + 1) |i|
        eacount[i] = 0;
    for (0..m) |i| {
        const next = rng(@truncate(i + 1));
        var xor = previous ^ next;
        previous = next;
        var hamming: usize = 0;
        for (0..bits) |j| {
            if (xor & 1 != 0)
                hamming += 1;
            xor = xor >> 1;
            if ((j % order) == order - 1) {
                eacount[hamming] += 1;
                hamming = 0;
            }
        }
    }
    chi = 0.0;
    for (0..order + 1) |i| {
        expected = @as(f64, @floatFromInt(bits / order * m)) * prob(order, i);
        if (expected > 5.0) {
            suma = (expected - eacount[i]) * (expected - eacount[i]) / expected;
        } else {
            suma = 0;
        }
        chi = chi + suma;
    }
    return chi;
}
pub const Prospector = struct{
    // From: https://nullprogram.com/blog/2018/07/31/
    // the 64 bit version is derived from https://xoshiro.di.unimi.it/splitmix64.c
    // which is derived from Java 8's SplittableRandom generator
    //   See http://dx.doi.org/10.1145/2714064.2660195
    pub fn hash64(orig: u64) u64 {
        var x = orig;
        x ^= x >> 30;
        x *%= 0xbf58476d1ce4e5b9;
        x ^= x >> 27;
        x *%= 0x94d049bb133111eb;
        x ^= x >> 31;
        return x;
    }
    pub fn unhash64(orig: u64) u64 {
        var x = orig;
        x ^= x >> 31 ^ x >> 62;
        x *%= 0x319642b2d24d8ec3;
        x ^= x >> 27 ^ x >> 54;
        x *%= 0x96de1b173f119089;
        x ^= x >> 30 ^ x >> 60;
        return x;
    }
    pub fn hash32(orig: u32) u32 {
        var x = orig;
        x ^= x >> 17;
        x *%= 0xed5ad4bb;
        x ^= x >> 11;
        x *%= 0xac4c1b51;
        x ^= x >> 15;
        x *%= 0x31848bab;
        x ^= x >> 14;
        return x;
    }
    pub fn unhash32(orig: u32) u32 {
        var x = orig;
        x ^= x >> 14 ^ x >> 28;
        x *%= 0x32b21703;
        x ^= x >> 15 ^ x >> 30;
        x *%= 0x469e0db1;
        x ^= x >> 11 ^ x >> 22;
        x *%= 0x79a85073;
        x ^= x >> 17;
        return x;
    }
    pub fn hash24(orig: u24) u24 {
        var x = orig;
        x ^= x >> 13;
        x *%= 0xed54bb;
        x ^= x >> 8;
        x *%= 0xac4b51;
        x ^= x >> 11;
        x *%= 0x318bab;
        x ^= x >> 14;
        return x;
    }
    pub fn unhash24(orig: u24) u24 {
        var x = orig;
        x ^= x >> 14;
        x *%= 0x32b21703;
        x ^= x >> 15 ^ x >> 30;
        x *%= 0x469e0db1;
        x ^= x >> 11 ^ x >> 22;
        x *%= 0x79a85073;
        x ^= x >> 17;
        return x;
    }
};
pub const Phi = struct{
    const phi = std.math.phi;
    // returns an odd number (changes u8) so all possible values are generated
    pub fn inversePhi(comptime T: type) T {
        switch (@typeInfo(T)) {
            .int => |int_info| switch (int_info.signedness) {
                .unsigned => return @as(T, @intFromFloat(@as(f128, @floatFromInt(1 << int_info.bits)) / phi)) | 1,
                else => {},
            },
            else => {},
        }
        @compileError("invalid type for inversePhi: " ++ @typeName(T));
    }
    // there isn't a closed form way to calculate this, but
    pub fn undoPhi(comptime T: type) T {
        return @import("general.zig").inverseMod(T, inversePhi(T)) catch @panic("not invertible");
    }
    pub fn MakePhiHash(comptime T: type) type {
        return struct {
            pub inline fn hash(v: T) T {
                return v *% inversePhi(T);
            }
            pub inline fn unhash(v: T) T {
                return v *% undoPhi(T);
            }
        };
    }
    pub fn hash(comptime T: type) fn (v: T) callconv(.@"inline") T {
        return MakePhiHash(T).hash;
    }
    pub const hash64= MakePhiHash(u64).hash;
    pub const hash32= MakePhiHash(u32).hash;
    pub const hash24= MakePhiHash(u24).hash;
    pub const hash16= MakePhiHash(u16).hash;
    pub const hash8= MakePhiHash(u8).hash;
    pub const unhash64= MakePhiHash(u64).unhash;
    pub const unhash32= MakePhiHash(u32).unhash;
    pub const unhash24= MakePhiHash(u24).unhash;
    pub const unhash16= MakePhiHash(u16).unhash;
    pub const unhash8= MakePhiHash(u8).unhash;
    test "check inversePhi" {
        const expectEqual = std.testing.expectEqual;
        try expectEqual(hash64(1), 11400714819323198485);
        try expectEqual(hash32(1), 2654435769);
        try expectEqual(hash24(1), 10368889);
        try expectEqual(hash16(1), 40503);
        try expectEqual(hash8(1), 159);
    }
    test "check undoPhi" {
        const expectEqual = std.testing.expectEqual;
        try expectEqual(unhash64(1), 17428512612931826493);
        try expectEqual(unhash32(1), 340573321);
        try expectEqual(unhash24(1), 11764425);
        try expectEqual(unhash16(1), 30599);
        try expectEqual(unhash8(1), 95);
    }
    test "check undoPhi is inverse" {
        const expectEqual = std.testing.expectEqual;
        try expectEqual(unhash64(hash64(1)), 1);
        try expectEqual(unhash32(hash32(1)), 1);
        try expectEqual(unhash24(hash24(1)), 1);
        try expectEqual(unhash16(hash16(1)), 1);
        try expectEqual(unhash8(hash8(1)), 1);
    }
    test "randomness of /phi - all values enumerated" {
        // data24 is too big and causes a crash
        // var data24 = [_]bool{false} ** (65536*256);
        // for (data24, 0..) |_, index| {
        //     const int = inversePhi24(@truncate(index));
        //     if (data24[int]) {
        //         return error.Failed;
        //     } else {
        //         data24[int] = true;
        //     }
        // }
        var data16 = [_]bool{false} ** 65536;
        for (data16, 0..) |_, index| {
            const int = hash16(@truncate(index));
            if (data16[int]) {
                return error.Failed;
            } else {
                data16[int] = true;
            }
        }
        var data8 = [_]bool{false} ** 256;
        for (data8, 0..) |_, index| {
            const int = hash8(@truncate(index));
            if (data8[int]) {
                return error.Failed;
            } else {
                data8[int] = true;
            }
        }
    }
};
// u32 key[2]= {
//     0xdeadbeef,
//     0xdeadbeef
// };
// uint32_t doHalfSipHash(uint32_t x) {
//     uint32_t in = x, out;
//     halfsiphash(&in, sizeof(in), key, &out, sizeof(out));
//     return out;
// }
fn SAC_print() void {
    // Significance level:
    // d.o.f.  0.05   0.01
    // 8      15.50  20.09
    // 16     26.29  32.00
    // 32     46.19  53.48
    // 64     83.67  93.21
    const print = std.log.err;
    print("'triple64' has a chi-square-8 value of {d}\n", .{SAC_test(u64, Prospector.hash64, 8)});
    print("'triple64' has a chi-square-16 value of {d}\n", .{SAC_test(u64, Prospector.hash64, 16)});
    print("'triple64' has a chi-square-32 value of {d}\n", .{SAC_test(u64, Prospector.hash64, 32)});
    print("'triple64' has a chi-square-64 value of {d}\n", .{SAC_test(u64, Prospector.hash64, 64)});
    print("'triple32' has a chi-square-8 value of {d}\n", .{SAC_test(u32, Prospector.hash32, 8)});
    print("'triple32' has a chi-square-16 value of {d}\n", .{SAC_test(u32, Prospector.hash32, 16)});
    print("'triple32' has a chi-square-32 value of {d}\n", .{SAC_test(u32, Prospector.hash32, 32)});
    print("'triple32_r' has a chi-square-8 value of {d}\n", .{SAC_test(u32, Prospector.unhash32, 8)});
    print("'triple32_r' has a chi-square-16 value of {d}\n", .{SAC_test(u32, Prospector.unhash32, 16)});
    print("'triple32_r' has a chi-square-32 value of {d}\n", .{SAC_test(u32, Prospector.unhash32, 32)});
    // print("'doHalfSipHash' has a chi-square-8 value of {d}\n", .{SAC_test(doHalfSipHash, 8)});
    // print("'doHalfSipHash' has a chi-square-16 value of {d}\n", .{SAC_test(doHalfSipHash, 16)});
    // print("'doHalfSipHash' has a chi-square-32 value of {d}\n", .{SAC_test(doHalfSipHash, 32)});
    print("'Phi.hash64' has a chi-square-8 value of {d}\n", .{SAC_test(u64, Phi.hash64, 8)});
    print("'Phi.hash64' has a chi-square-16 value of {d}\n", .{SAC_test(u64, Phi.hash64, 16)});
    print("'Phi.hash64' has a chi-square-32 value of {d}\n", .{SAC_test(u64, Phi.hash64, 32)});
    print("'Phi.hash64' has a chi-square-64 value of {d}\n", .{SAC_test(u64, Phi.hash64, 64)});
    print("'inversePhi32' has a chi-square-8 value of {d}\n", .{SAC_test(u32, Phi.hash32, 8)});
    print("'inversePhi32' has a chi-square-16 value of {d}\n", .{SAC_test(u32, Phi.hash32, 16)});
    print("'inversePhi32' has a chi-square-32 value of {d}\n", .{SAC_test(u32, Phi.hash32, 32)});
    print("'triple24' has a chi-square-8 value of {d}\n", .{SAC_test(u24, Prospector.hash24, 8)});
    print("'triple24' has a chi-square-24 value of {d}\n", .{SAC_test(u24, Prospector.hash24, 24)});
}
test {
    _ = Phi;
}
pub fn main() void {
    SAC_print();
    // var sequential = Sequential{};
    // showSequence(sequential.sequence());
    var bitPattern = BitPattern{};
    showSequence(bitPattern.sequence());
}
