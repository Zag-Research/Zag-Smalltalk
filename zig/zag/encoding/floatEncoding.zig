const std = @import("std");
const builtin = @import("builtin");
const math = std.math;
const expectEqual = std.testing.expectEqual;
const expect = std.testing.expect;
const rotl = std.math.rotl;
const rotr = std.math.rotr;

const raise_specific_errors = false;

pub const FastSpur = switch (builtin.target.cpu.arch) {
    .x86_64 => SpurAlt1,
    else => SpurAlt2,
}; // used by spur.zig
pub const EncodeError = error{ Unencodable, PosInf, NegInf, NaN };

// immediate float layout: [exp8(8)][mant(52)][sign(1)][tag(3)]
// Ref: https://clementbera.wordpress.com/2018/11/09/64-bits-immediate-floats/

//const debug_print = std.debug.print;
fn debug_print(_: anytype, _: anytype) void {}
const Range = struct {
    low: u64,
    high: u64,
    fn includes(self: Range, n: u64) bool {
        return n >= self.low and n <= self.high;
    }
    fn covers(ranges: []const Range, n: u64) bool {
        for (ranges) |range| {
            if (range.includes(n)) return true;
        }
        return false;
    }
};
// It has been verified via godbolt that:
// - this skips generating the conversion code
// export fn t1(x: u64) bool {
//     if (decode(x)) |_| return true;
//     return false;
// }
// - this skips generating the test code
// export fn t2(x: u64) f64 {
//     if (decode(x)) |y| return y;
//     unreachable;
// }
// - this generates the test code and the conversion code
// export fn t3(x: u64) f64 {
//     if (decode(x)) |y| return y;
//     return 0;
// }
pub const Spur = struct {
    const name = "spur";
    const uses = "4 (5,6,7 reserved)";
    const TAG = 0b100; // immediate float tag
    const EXPONENT_OFFS: u64 = 0x7000000000000000;
    pub inline fn encode(value: f64) EncodeError!u64 {
        const bits: u64 = @bitCast(value);
        const rotated = rotl(u64, bits, 1);
        if (rotated <= 1) {@branchHint(.unlikely);
            debug_print("spur value= {}\n bits=    {b:0>64}\n rotated= {b:0>64}\n result={}\n",
                .{value, bits, rotated, (rotated << 3) + TAG});
            return (rotated << 3) + TAG;
        }
        const offset =  rotated -% EXPONENT_OFFS;
        const shifted = rotl(u64, offset, 3);
        debug_print("spur value= {}\n bits=    {b:0>64}\n rotated= {b:0>64}\n offset=  {b:0>64}\n shifted= {b:0>64} unencodeable={}\n",
            .{value, bits, rotated, offset, shifted, shifted & 7 != 0 or offset == 0});
        if (shifted & 7 != 0) {@branchHint(.unlikely);
            return error.Unencodable;
        }
        if (offset == 0) {@branchHint(.unlikely);
            return error.Unencodable;
        }
        return shifted + TAG;
    }
    pub inline fn decode(self: u64) ?f64 {
        if (self & TAG == 0) return null;
        const shifted = self >> 3;
        const offset = if (shifted <= 1) shifted else shifted + EXPONENT_OFFS;
        return @bitCast(rotr(u64, offset, 1));
    }
    const valid_ranges = [_]Range{
      .{.low = 0x0000_0000_0000_0000, .high = 0x0000_0000_0000_0000},
      .{.low = 0x3800_0000_0000_0001, .high = 0x47FF_FFFF_FFFF_FFFF},
    };
};
const SpurAlt1 = struct {
    const name = "spurAlt1";
    const uses = Spur.uses;
    const TAG = Spur.TAG;
    pub inline fn encode(value: f64) EncodeError!u64 {
        const bits: u64 = @bitCast(value);
        const rotated = rotl(u64, bits, 5);
        const inc = rotated +% 1;
        debug_print("spurAlt1 value= {}\n bits=    {b:0>64}\n rotated= {b:0>64}\n inc=     {b:0>64}\n back=    {b:0>64} unencodeable={}\n",
            .{value, bits, rotated, inc, rotr(u64, inc, 1), inc & 14 != TAG << 1 and inc != 1 and inc != 17});
        if (inc & 14 == TAG << 1) {@branchHint(.likely);
            if (inc != TAG << 1) {@branchHint(.likely);
                return rotr(u64, inc, 1);
            }
        }
        if (inc == 1) return 0x4;
        if (inc == 17) return 0xC;
        return error.Unencodable;
    }
    pub const decode = SpurAlt2.decode;
    const valid_ranges = Spur.valid_ranges;
};
pub const SpurAlt2 = struct {
    const name = "spurAlt2";
    const uses = Spur.uses;
    const TAG = Spur.TAG;
    pub inline fn encode(v: f64) EncodeError!u64 {
        var y = rotl(u64, @bitCast(v), 5);
        if (y <= 0x10) { // probably 0
            @branchHint(.unlikely);
            if (y == 0) return TAG; // 0
            if (y == 0x10) return 8 + TAG; // -0
            y = rotr(u64, y +% 1, 1);
            // handle normal immediate float; with zero-collision check
            if ((y & 0x7) == TAG and (y | 0x8) != 0xC) return y;
            // if ((y & 0x7) == TAG and (y & 0xffff_ffff_ffff_ffff) != 0) return y;
            // if ((y & 0x7) == TAG and (y >> 4) != 0) return y;
        } else {
            @branchHint(.likely);
            y = rotr(u64, y +% 1, 1);

            // handle normal immediate float
            if ((y & 0x7) == TAG) {
                @branchHint(.likely);
                return y;
            }
        }
        return error.Unencodable;
    }
    pub inline fn decode(self: u64) ?f64 {
        if (self & TAG == 0) return null;
        if (self <= 0xC) {@branchHint(.unlikely);
            if (self == TAG) {
                return 0.0;
            } else {
                return -0.0;
            }
        }
        var r = rotl(u64, self, 1);
        r -%= 1;
        r = rotr(u64, r, 5);
        return @bitCast(r);
    }
    const valid_ranges = Spur.valid_ranges;
};
pub const Fst1 = struct {
    const name = "fst1";
    const uses = "4 (5,6,7 reserved)";
    const OFFSET: u64 = 9 << 58;
    pub inline fn encode(x: f64) EncodeError!u64 {
        const u = rotl(u64, @as(u64, @bitCast(x)) +% OFFSET, 5);
        if (u & 7 == 4) {
            @branchHint(.likely);
            return u;
        }
        return error.Unencodable;
    }
    pub inline fn decode(self: u64) ?f64 {
        if (self & 4 != 0) {
            @branchHint(.likely);
            return @bitCast(rotr(u64, self, 5) -% OFFSET);
        }
        return null;
    }
    const valid_ranges = [_]Range{
      .{.low = 0x0000_0000_0000_0000, .high = 0x03FF_FFFF_FFFF_FFFF},
      .{.low = 0x3C00_0000_0000_0000, .high = 0x43FF_FFFF_FFFF_FFFF},
      .{.low = 0x7C00_0000_0000_0000, .high = 0x7FFF_FFFF_FFFF_FFFF},
    };
};
pub const Fst2 = struct {
    const name = "fst2";
    const uses = "4,5";
    pub inline fn encode(x: f64) EncodeError!u64 {
        const u = rotl(u64, @bitCast(x), 5) -% 3;
        if (u & 6 == 4) {
            @branchHint(.likely);
            return u;
        }
        return error.Unencodable;
    }
    pub inline fn decode(self: u64) ?f64 {
        if (self & 6 == 4) {
            @branchHint(.likely);
            return @bitCast(rotr(u64, self +% 3, 5));
        }
        return null;
    }
    const valid_ranges = [_]Range{
      .{.low = 0x0000_0000_0000_0000, .high = 0x07FF_FFFF_FFFF_FFFF},
      .{.low = 0x3800_0000_0000_0000, .high = 0x47FF_FFFF_FFFF_FFFF},
      .{.low = 0x7800_0000_0000_0000, .high = 0x7FFF_FFFF_FFFF_FFFF},
    };
};
pub const Fst4 = struct {
    const name = "fst4";
    const uses = "2,3,6,7";
    pub inline fn encode(x: f64) EncodeError!u64 {
        const u = rotl(u64, @bitCast(x), 4) +% 3;
        if (u & 2 != 0) {
            @branchHint(.likely);
            return u;
        }
        return error.Unencodable;
    }
    pub inline fn decode(self: u64) ?f64 {
        if (self & 2 != 0) {
            @branchHint(.likely);
            return @bitCast(rotr(u64, self -% 3, 4));
        }
        return null;
    }
    const valid_ranges = [_]Range{
      .{.low = 0x0000_0000_0000_0000, .high = 0x0FFF_FFFF_FFFF_FFFF},
      .{.low = 0x3000_0000_0000_0000, .high = 0x4FFF_FFFF_FFFF_FFFF},
      .{.low = 0x7000_0000_0000_0000, .high = 0x7FFF_FFFF_FFFF_FFFF},
    };
};
pub const Zag6 = struct {
    const name = "zag6";
    const uses = "2,3,4,5,6,7";
    pub inline fn encode(x: f64) EncodeError!u64 {
        const u = rotl(u64, @bitCast(x), 4) +% 3;
        if (u & 6 != 0) {
            @branchHint(.likely);
            return u;
        }
        return error.Unencodable;
    }
    pub inline fn decode(self: u64) ?f64 {
        if (self & 6 != 0) {
            @branchHint(.likely);
            return @bitCast(rotr(u64, self -% 3, 4));
        }
        return null;
    }
    const valid_ranges = [_]Range{
      .{.low = 0x0000_0000_0000_0000, .high = 0x4FFF_FFFF_FFFF_FFFF},
      .{.low = 0x7000_0000_0000_0000, .high = 0x7FFF_FFFF_FFFF_FFFF},
    };
};
pub const NaN = struct {
    const name = "nan";
    const uses = "N/A";
    const NaN_one: u64 = @bitCast(math.nan(f64));
    pub inline fn encode(x: f64) EncodeError!u64 {
        return @bitCast(x);
    }
    pub inline fn decode(self: u64) ?f64 {
        const f: f64 = @bitCast(self);
        if (!math.isNan(f)) {
            @branchHint(.likely);
            return f;
        } else if (self == NaN_one) {
            @branchHint(.unlikely);
            return f;
        }
        return null;
    }
    const valid_ranges = [_]Range{
      .{.low = 0x0000_0000_0000_0000, .high = 0x7FFF_FFFF_FFFF_FFFF},
    };
};
pub const NuN = struct {
    const name = "nun";
    const uses = "N/A";
    const NuN_bias = 0x0001_ffff_ffff_ffff;
    pub inline fn encode(x: f64) EncodeError!u64 {
        return @as(u64, @bitCast(x)) +% NuN_bias;
    }
    pub inline fn decode(self: u64) ?f64 {
        if (self >= NuN_bias) {
            @branchHint(.likely);
            return @bitCast(self -% NuN_bias);
        }
        return null;
    }
    const valid_ranges = [_]Range{
      .{.low = 0x0000_0000_0000_0000, .high = 0x7FFF_FFFF_FFFF_FFFF},
    };
};
fn checkEqual(str: []const u8, i: usize, expected: anytype, actual: @TypeOf(expected)) !void {
    if (expectEqual(expected,actual)) |_| {
    } else |err| {
        const f: f64 = @bitCast(i << (64-BITS));
        std.debug.print("for i={}({x})({}) in {s}\n",.{i, i, f, str});
        return err;
    }
}
const BITS = 6;
test "encode accuracy - spurAlt1" {
    for (0..1<<BITS) |i| {
        const f: f64 = @bitCast(i << (64-BITS));
        try checkEqual("spurAlt1",i,SpurAlt1.encode(f),Spur.encode(f));
    }
}
test "encode accuracy - spurAlt2" {
    for (0..1<<BITS) |i| {
        const f: f64 = @bitCast(i << (64-BITS));
        try checkEqual("spurAlt2",i,SpurAlt2.encode(f),Spur.encode(f));
    }
}

test "encode/decode" {
    inline for (.{Spur, SpurAlt1, SpurAlt2, Fst1, Fst2, Fst4, Zag6}) |encoding| {
        var valid_v: [likely_values.len]f64 = undefined;
        var invalid_v: [likely_values.len]f64 = undefined;
        var decode_v: [likely_values.len]u64 = undefined;
        const validValues, const invalidValues, const decodeValues = validAndNot(encoding,&valid_v,&invalid_v,&decode_v);
        _ = decodeValues;
        for (validValues) |value| {
            if (std.math.isNan(value)) {
                try expect(std.math.isNan(encoding.decode(try encoding.encode(value))));
            } else
                try expectEqual(value, encoding.decode(try encoding.encode(value)));
        }
        for (invalidValues) |value|
            try expectEqual(error.Unencodable, encoding.encode(value));
    }
}

const iterations = if (builtin.mode == .ReleaseFast) 1_000_000_000 else 100_000_000;
fn ns(time: u64) f64 {
    return @as(f64, @floatFromInt(time)) / @as(f64, @floatFromInt(iterations));
}
const likely_values =
    [_]f64{0.0, -0.0, 0.5, -0.5} ++
    [_]f64{ 1.0, -1.0, math.pi, 42.0, -3.14159, 100.0, -100.0 } ** 16 ++
    [_]f64{
        @bitCast(@as(u64, 0x0000_0000_0000_0001)),
        @bitCast(@as(u64, 0x07ff_ffff_ffff_ffff)),
        @bitCast(@as(u64, 0x0800_0000_0000_0000)),
        @bitCast(@as(u64, 0x1000_0000_0000_0000)),
        @bitCast(@as(u64, 0x1800_0000_0000_0000)),
        @bitCast(@as(u64, 0x2000_0000_0000_0000)),
        @bitCast(@as(u64, 0x2800_0000_0000_0000)),
        @bitCast(@as(u64, 0x3000_0000_0000_0000)),
        @bitCast(@as(u64, 0x37ff_ffff_ffff_ffff)),
        @bitCast(@as(u64, 0x3800_0000_0000_0000)),
        @bitCast(@as(u64, 0x3800_0000_0000_0001)),
        @bitCast(@as(u64, 0x4000_0000_0000_0000)),
        @bitCast(@as(u64, 0x47ff_ffff_ffff_ffff)),
        @bitCast(@as(u64, 0x4800_0000_0000_0000)),
        @bitCast(@as(u64, 0x5000_0000_0000_0000)),
        @bitCast(@as(u64, 0x5800_0000_0000_0000)),
        @bitCast(@as(u64, 0x5FFF_FFFF_FFFF_FFFF)),
        @bitCast(@as(u64, 0x6000_0000_0000_0000)),
        @bitCast(@as(u64, 0x6800_0000_0000_0000)),
        @bitCast(@as(u64, 0x7000_0000_0000_0000)),
        @bitCast(@as(u64, 0x7800_0000_0000_0000)),
        @bitCast(@as(u64, 0x77ff_ffff_ffff_ffff)),
        @bitCast(@as(u64, 0x7800_0000_0000_0000)),
        @bitCast(@as(u64, 0x7fff_ffff_ffff_ffff)),
    } ++
    [_]f64{math.nan(f64)} ++
    [_]f64{math.inf(f64)} ++
    [_]f64{-math.inf(f64)}
;

fn validAndNot(comptime encoding: anytype, valid_v: []f64, invalid_v: []f64, decode_v: []u64) struct{[]f64,[]f64,[]u64} {
    var valid_n: usize = 0;
    var invalid_n: usize = 0;
    for (&likely_values) |value| {
        const u: u64 = @bitCast(value);
        if (Range.covers(&encoding.valid_ranges, u & 0x7fff_ffff_ffff_ffff)) {
            if (valid_v.len > 0) {
                valid_v[valid_n] = value;
                decode_v[valid_n] = encoding.encode(value) catch @panic("covered value doesn't encode");
                valid_n += 1;
            }
        } else {
            invalid_v[invalid_n] = value;
            invalid_n += 1;
        }
    }
    for (valid_v[0..valid_v.len-valid_n],valid_v[valid_n..valid_v.len]) |v,*vp|
        vp.* = v;
    if (invalid_n > 0) {
        for (invalid_v[0..invalid_v.len-invalid_n],invalid_v[invalid_n..invalid_v.len]) |v,*vp|
            vp.* = v;
        invalid_n = invalid_v.len;
    }
    for (decode_v[0..decode_v.len-valid_n],decode_v[valid_n..decode_v.len]) |v,*vp|
        vp.* = v;
    return .{valid_v, invalid_v[0..invalid_n], decode_v};
}
fn benchmark(comptime encoding: anytype) void {
    var valid_v: [likely_values.len]f64 = undefined;
    var invalid_v: [likely_values.len]f64 = undefined;
    var decode_v: [likely_values.len]u64 = undefined;
    const validValues, const invalidValues, const decodeValues = validAndNot(encoding,&valid_v,&invalid_v,&decode_v);
    // Benchmark
    var timer = std.time.Timer.start() catch @panic("unreachable");

    _ = timer.lap();
    for (0..iterations / validValues.len) |_| {
        for (validValues) |val| {
            std.mem.doNotOptimizeAway(encoding.encode(val) catch 0);
        }
    }
    const valid_time = timer.lap();
    if (invalidValues.len > 0){
        for (0..iterations / invalidValues.len) |_| {
            for (invalidValues) |val| {
                std.mem.doNotOptimizeAway(encoding.encode(val) catch 0);
            }
        }
    }
    const invalid_time = timer.lap();
    for (0..iterations / decodeValues.len) |_| {
        for (decodeValues) |val| {
            if (encoding.decode(val)) |decoded|
                _ = std.mem.doNotOptimizeAway(decoded);
        }
    }
    const decode_time = timer.lap();
    var coverage: u64 = 0;
    var total: u64 = 0;
    for (0..,&frequency) |index,count| {
        total += count;
        const addr = index << 52;
        // std.debug.print("name={s}\n addr=  {b:0>64}\n ranges={b:0>64}\n        {b:0>64} {} {}\n",.{encoding.name,addr,encoding.valid_ranges[0].low,encoding.valid_ranges[0].high, encoding.valid_ranges[0].includes(addr), count});
        if (Range.covers(&encoding.valid_ranges,addr))
            coverage += count;
    }
    const cover = @as(f64, @floatFromInt(coverage)) * 100.0 / @as(f64, @floatFromInt(total));
    std.debug.print("{s:<8} time: {d:.3}ns {d:.3}ns {d:.3}ns covers {d:6.2}% uses {s}\n",
        .{ encoding.name, ns(valid_time), ns(invalid_time), ns(decode_time), cover, encoding.uses });
    if (false) {
        for (&encoding.valid_ranges) |range| {
            if (range.low == range.high) {
                std.debug.print("{e:.2}\n",.{@as(f64,@bitCast(range.low))});
            } else {
                std.debug.print("{e:.2} - {e:.2}\n",.{@as(f64,@bitCast(range.low)), @as(f64,@bitCast(range.high))});
            }
        }
    }
}
// zig run -Doptimize=ReleaseFast floatSpur.zig
pub fn main() void {
    // if (false) {
    //     for (valid_values) |val| {
    //         std.debug.print("0x{x:0>16},\n", .{Spur.encode(val) catch unreachable});
    //     }
    // }

    inline for (.{Spur, SpurAlt1, SpurAlt2, Fst1, Fst2, Fst4, Zag6, NaN, NuN }) |encoding|
        benchmark(encoding);
    // std.debug.print("spur is {d:.2}x {d:.2}x faster than check\n", .{ delta(spur_valid_time, check_valid_time), delta(spur_invalid_time, check_valid_time) });
    // std.debug.print("spurAlt1 is {d:.2}x {d:.2}x faster than check\n", .{ delta(spurAlt1_valid_time, check_valid_time), delta(spurAlt1_invalid_time, check_valid_time) });
    // std.debug.print("spurAlt2 is {d:.2}x {d:.2}x faster than check\n", .{ delta(spurAlt2_valid_time, check_valid_time), delta(spurAlt2_invalid_time, check_valid_time) });
    // std.debug.print("spurAlt1 is {d:.2}x {d:.2}x faster than spur\n", .{ delta(spurAlt1_valid_time, spur_valid_time), delta(spurAlt1_invalid_time, spur_invalid_time) });
    // std.debug.print("spurAlt2 is {d:.2}x {d:.2}x faster than spur\n", .{ delta(spurAlt2_valid_time, spur_valid_time), delta(spurAlt2_invalid_time, spur_invalid_time) });
}

fn delta(spec: u64, check: u64) f64 {
    return @as(f64, @floatFromInt(check)) / @as(f64, @floatFromInt(spec));
}
const frequency = [_]u64{
8269066471,    201412272,     34323344,     34003560,     42577627,     33863758,     35979373,     42157753,
  34695722,     35381577,     34927456,     35320781,     35351340,     35270890,     35424442,     35371850,
  35659424,     35739204,     35485119,     35735732,     35819918,     36025874,     36101126,     35872865,
  36268360,     36376495,    136342991,     36371780,     36110643,     36441082,     36550463,     37829367,
  38590642,     38303378,     38870148,     38957293,     39001687,     38982189,     38687873,     38633799,
  38328791,     38612823,     39153464,     38982681,     38619697,     37960009,     37914219,     38736700,
  37894890,    257991426,     39345089,     41043154,     43635183,     49567799,     58349896,     69618727,
  89143885,    119356588,    168705921,    233548564,    335694280,    467452545,    573383640,    634776711,
 553177398,   3669924403,     54511849,     60860306,     68823053,     79648581,     92019250,    101111836,
 101985682,    125222694,   1183996701,    333635637,     90343065,     89950217,     93672057,    104391999,
 115462190,    122572736,    117589114,    112189327,    833058640,     99881507,     95530986,     99664383,
 109581466,    120677554,    126821827,    121058797,    123709848,    855962124,    131239906,    146192600,
 338178457,    183276893,    210722886,    246172369,    278483866,    340685097,   1122817527,    457613965,
 428557877,    449266939,    553429864,    939273790,    917719472,   1304044263,   2169353404,   4492693569,
1075921166,   2184890715,   1452197210,   2233462147,   3793069648,   4680824515,   7334951140,  15218579536,
13700439115,  11749582344,  12888045851,  19132189500,  23678277875,  37276849056,  36937799544,  45633221084,
9691305713,   6728579876,   4184333548,   4040224152,   3399056204,   5944701075,   7582303684,   6557757608,
7048617477,   6029269248,   5026897750,  14057375989,   6163645272,  19575052313,  70029779888,   3230317014,
3594290391,   3831003305,   4549739229,   6138070693,   8668383836,   8668479464,   3024363684,   2085200267,
4253095197,   2119124790,    733100645,   1842378067,   7357797610,   4068165393,   9610540056,    173538792,
2113496049,   2113943421,     72634976,     71043275,     66426778,     66893253,     66845375,     67635155,
  68609457,     68302578,     69020149,     69078770,     69319933,     70749760,    173289931,     71933338,
  70920648,     72407563,     72964253,     73393305,     73023227,     73189148,     73392928,     74449741,
  75542468,     75068057,     75329555,     75630635,     76409312,     78014139,     78423946,     78436285,
  78369774,     83929770,    298207336,     83036808,     85703848,     89694870,     96238979,    109091692,
 126667519,    153055752,    203978530,    255068704,    349938526,    472211286,    588949360,    693807129,
 569222844,    923275107,   3026540939,     84066819,     84103878,     84631657,     85434198,     86153846,
  86086355,     86290144,     86751826,     87535034,     88054294,     88037015,     88261810,     88999986,
  89674518,     91702953,     90739124,     90835312,     91603331,     96550934,     92184662,     92185866,
  92188894,     92897375,     93881986,     94255338,     94072153,     94238206,     94925431,     95631638,
  96169907,     95962626,     96427278,     96850966,     97551130,     98047274,     97961653,     98443311,
  98930793,     99423330,    100000057,     99969462,    100413905,    101008056,    101587026,    101918198,
1316665173,    102313414,    103107603,    103209444,    103761408,    323286357,    106748023,    109060327,
 111798061,    120068518,    124917055,    137803365,    155875447,    185371646,    233621059,    297133930,
 398014912,    529115789,    636848456,    718837127,    602644549,   3780036744,    110888023,    111465142,
 111644188,    112201294,    112284884,    113125264,    113458996,    113695431,    114084321,    114576361,
 115177189,    115416778,    115718715,    117410095,    116824698,    117173735,    117463814,    117851957,
 118390907,    119411858,    121083033,    120219475,    120855308,    124712242,    126982816,    126386892,
 121544668,    122035120,    122599745,    123040327,    123198903,    123593675,    124171424,    124616556,
 124848389,    125273701,    125675652,    126144383,    126728613,    126885796,    127474319,    127689884,
 128227100,    128696627,    129054940,    129477791,    129872023,    130308637,    349236431,    132864141,
 135230049,    137864787,    143486830,    151516872,    165383995,    186174442,    217045766,    265427990,
 332361546,    439906522,    569189979,    676179486,    733403276,    647063875,   3757103591,    138268628,
 138591049,    139191394,    139482730,    139848103,    140564590,    140823026,    141324280,    141722783,
 142140038,    142708155,    143169329,    143558614,    143993629,    144382413,    144986661,    145505555,
 145664526,    146358183,    146608439,    147241470,    147778667,    148056155,    148821357,    149064972,
 149496784,    151361556,    151173514,    152899099,    151991978,    155474962,    152477075,    152927543,
 153397479,    153886855,    154333837,    154755002,    155432714,    155712269,    156533489,    156544725,
 157156507,    158118785,    158002359,    158935864,    377732054,    161482885,    163887814,    166389313,
 172616318,    180347816,    193876132,    213904632,    245488592,    293604801,    361516220,    465165146,
 607503684,    704561314,    773245834,    670496233,   3811266606,    169923761,    170434799,    171191701,
 171213364,    174469775,    311385280,    172358832,    174209001,    173516206,    173875058,    174642924,
 175023254,    175713909,    176233441,    176615092,    177387482,    177975476,    178544598,    180354002,
 179484222,    180002252,    180423210,    181045020,    181457956,    181931490,    182317720,    182921440,
 183489428,    184094810,    184496079,    184984044,    185628726,    186140345,    186630221,    187230612,
 187656456,    191548024,    189481936,    190183155,    190311307,    194442007,    191349546,    191976723,
 410847781,    196742794,    199729271,    204281604,    212028249,    225168425,    243365559,    272479887,
 318503888,    381690544,    480447263,    610586717,    725105389,    824385741,    684804378,   1168984406,
2968455701,    201832071,    202472302,    203353952,    203721776,    205731060,    205108486,    205485915,
 206155969,    206717964,    207341622,    209434757,    209552357,    210000055,    210368240,    210643495,
 210910635,    216135955,    211808140,    212686421,    213292808,    213811618,    214625077,    214849589,
 217076759,    216051545,    218101804,    228407712,    218064810,    218698648,    219137189,    220045396,
 220605757,    221233094,    222104309,    222779809,    223125930,    223170476,    225133236,    224349420,
 444065356,    229501705,    231767438,    235350358,    243170325,    255312338,    271211456,    298464386,
 341252505,    399906095,    488200030,    603781779,    740868801,    839356302,    757647531,    929784110,
3418318622,    236706699,    237301413,    243592427,    238338853,    239598681,    239825199,    240748120,
 241241612,    241966895,    242423909,    243338908,    243827490,    244427661,    245477522,    245591730,
 246847909,    247330676,    248053218,    252228865,    249324683,    250335732,    250799330,    251439515,
 252224980,    252908110,    253262065,    256443137,    255314231,    256346324,    256545220,    256792568,
 257881736,    257994356,    264437004,    259740117,    261343669,    481004566,    263481965,    265844521,
 270525982,    276705935,    287076159,    301479877,    325621552,    357560476,    409380635,    487322515,
 596722263,    735539125,    838320027,    852078067,    825385257,   3792840955,    273187384,    273855576,
 274416887,    275496395,    275722152,    277052066,    277392486,    278406676,    279338338,    281494552,
 281676838,    281935374,    287501209,    282889776,    284397043,    284718569,    291241014,    425166297,
 288360081,    288531473,    288593546,    290005730,    290467405,    291345375,    292511549,    294287445,
 294533237,    294874589,    296565669,    297517980,    299732508,    298073261,    299148558,    299836188,
 519622856,    305657372,    307691775,    312879460,    321326934,    333950000,    352509109,    380976149,
 426837820,    486128270,    585370320,    712334856,    836494557,    941322352,    814311876,   1098750203,
3344308593,    316632984,    327002022,    318587704,    319476290,    320510425,    321161797,    322268573,
 323114394,    324212659,    326400507,    326270049,    327705211,    328862205,    329917943,    330138141,
 331702016,    332358232,    334211993,    334927028,    338659160,    336944180,    353737023,    340501486,
 343344175,    346170548,    341849008,    344306459,    344442828,    346274947,    349907292,    569282451,
 356797901,    355730453,    395280366,    365782480,    374452820,    391261339,    418280768,    446305943,
 500197681,    573737316,    684502792,    828317765,    934841522,    972157095,    895774502,   3945358751,
 370792731,    371076302,    371973428,    373401099,    378232749,    375891720,    377132069,    378348737,
 379418221,    381473265,    384118555,    387225017,    391529736,    396594386,    401660149,    401108606,
 399901409,    399314578,    398172994,    397903926,    399884654,    401261096,    403639856,    405773748,
 407076872,    409084357,    417183468,    413348394,    634770640,    418508327,    423058438,    427995217,
 436721254,    451899505,    472855183,    496073579,    533641272,    587935626,    680784802,    791163966,
 939721993,   1044380334,   1002391330,   1047863455,   3814130691,    441670021,    443216525,    444507114,
 447067135,    449419042,    453128118,    456506851,    460378987,    468858354,    468568661,    466760147,
 464162569,    461652846,    466936541,    464151066,    606324520,    469842710,    479312030,    475380718,
 481241063,    480585121,    481649702,    485257453,    488812784,    491623861,    715777943,    511609014,
 516617854,    524748356,    526997673,    536207288,    551490449,    578112860,    625488217,    688465497,
 789734678,    910660580,   1087291812,   1165086410,   1075444806,   1401502784,   3766239666,    541059052,
 546484784,    555553965,    556662693,    562221172,    561851656,    561717929,    569787579,    557536752,
 558157701,    561213500,    639253115,    566728607,    642818498,    574237002,    581477707,    580917897,
 584881441,    589964040,    594641887,    598988599,    603422845,    827346609,    613355922,    612390917,
 613459266,    622222824,    636761297,    657590729,    689031937,    737325455,    804332681,    907955516,
1040917769,   1193942481,   1315904181,   1206848939,   1353002933,   3840631909,    801545158,    805612868,
 665720587,    661002688,    664728381,    662895356,    665834952,    667038639,    678125223,    675983495,
 681118638,    687293247,    695477470,    701125062,    709221740,    717873542,    726843448,    734690395,
 750892465,    975941000,    731946508,    725556050,    732094533,    881428974,    899785351,    787148097,
 818366704,    872152159,    940516549,   1047689560,   1183668122,   1343279682,   1459583918,   1376076802,
1512555400,   4054963415,    807530811,    817751470,    849523523,    827680996,    821724696,    829757509,
 974661958,    986862134,    864323145,    870035790,    883508328,   1036533145,    930355101,    911459452,
 914695004,    915562380,   1140461700,    935192000,    967547189,    957258007,    965283050,    986456015,
1151562601,   1233435345,   1115390177,   1198160967,   1310311123,   1464930041,   1665319432,   1790792809,
1770463901,   1803971136,   4667963361,   1258311593,   1299723001,   1323573169,   1495384622,   1530597669,
1433243747,   1465720653,   1511384428,   1561531063,   1603522083,   1681487507,   1665454472,   1700584736,
1927764812,   1751710816,   1944898232,   2003658035,   1929538374,   1994775306,   2478324778,   2146817034,
2235143068,   2348344335,   2503674725,   2626769220,   2834795853,   3000350285,   3225725817,   3193326800,
6075941752,   2491816884,   2525962338,   2566580005,   2608093190,   2662727881,   2712003855,   2967061641,
2792232508,   2980567505,   2929555383,   3089581040,   3254721378,   3134632384,   3084453536,   3144338470,
3187671049,   3226340632,   3271689803,   3344015676,   3485872763,   3766862160,   3979255496,   4042236808,
4331102943,   4535367186,   4478370733,   5061791036,   7256778913,   4250090363,   4509912997,   4418220619,
4396177774,   5247342780,   4703250078,   4763603924,   4939665841,   4833218159,   4945164184,   5015817591,
5061664875,   5191157782,   5395647902,   5455769635,   6062668630,   5842305574,   6249193753,   6263549910,
6712734827,   6880718275,   7083833725,   7155550446,  10274698250,   6915597156,   6953844123,   6859175728,
6748873666,   6655910995,   7096825700,   7084515762,   7870238700,   7902730141,   8198987084,   9170480005,
9033252524,  11163399400,  12507586469,  15392162203,  18112578189,  19277865726,  17332419111,  19898025898,
30255562914,  79869695796,  31640342264,  24234750018,  25286375281,  25770770788,  23910942796,  24717233779,
40949786635,  33711126097,  45049080829,  35257971663,  33352547849,  33105645162,  35650542851,  40627225825,
36845929803,  38887799975,  42617542715,  46509327372,  51210729849,  57393143918,  63272212424,  73284724927,
84091752354,  94656228035, 115903673656, 128425670474, 145135185442, 159867749575, 175244086070, 229929433403,
216350138834, 242270441733, 255931556020, 282353309365, 286326078642, 291754063586, 313002876382, 355351220215,
428410485721, 459242559453, 420227446256, 408612849711, 420732530891, 534560123986, 476337246137, 447939313578,
450789267883,1085353365352, 814999025151, 668017545814, 576832136393, 795035930213,1302713172072,1186905620354,
586694401601, 706278014498, 667148689552, 652538973733, 483059291358, 320722991619, 333954964003, 253204643398,
171940724850, 186304447931, 275691964295, 174216304942, 145905412624, 156223371227, 518890923798, 135176108349,
35111636399,  33936050483,  38067673429,  13323323758,  17345076545,  15053302870,  33217972187,  77752405314,
32590633914,  71258750807,  24894469941,  13366979781,   5208854814,   4845077149,   7780417909,   3638976413,
24300646561,   6160405599,    824607206,   9007863920,    980919701,    952906861,   2238060081,   3455671864,
 942123338,    224257397,    356714736,    498833070,   2032694685,    233742571,    271397297,     28688939,
 440462770,    652922497,    775032126,  43159189723,  80153723644,   1447950475,     72689191,   1011763006,
 118885408,     25030548,    112217616,     67588939,    264463057,     80679851,    225130646,    363239470,
 103980962,     56543698,     63341594,      2835527,    102108502,      1853531,     62083162,     61756698,
  41672627,      1490542,     61593183,     61403975,     41444918,      1481333,     61267120,     61252100,
  41284646,      1189157,     61286633,     61418384,     41291165,     61438432,      1800057,    101664058,
   1741786,     61909797,      2202521,    102830478,      3786116,     62847113,      3312843,    101948885,
   1880325,     62181235,      2472211,    102248698,      3900506,     74651888,      2110935,    101720344,
   1482251,     61216607,      1490900,    101322648,      1181889,     61215451,     61235590,     41261594,
   1297937,     61271761,     61407533,     42650686,      1106282,     61165606,     61363946,     41142230,
   1117419,     61299773,     61158660,     40902148,       951588,     61000533,     60953598,     40996650,
  60933254,       945594,    101193073,      1055507,     60959103,       892490,    100924982,       630492,
  60745002,       692994,    100713871,       581824,     60583947,       569682,    100429132,       425998,
  60295771,       292343,    100384109,       539553,     60610362,       358303,    100576664,       253745,
  60292090,     60265979,     40368601,       304412,     60478131,     62275049,     41055197,      1873560,
  63911088,     70478577,     55673764,     24539265,     71716018,     60318860,     40503200,     60122683,
     83011,    100040995,       110645,     60080747,    171131164,    101566454,        69463,     61605167,
     65453,    100033012,       105492,     60052422,        55867,    100094848,       535456,     60319934,
    236866,    100257619,       317307,     60209614,       408229,    100747708,      1226840,     63558864,
   1512937,    180025685,       110020,     61575169,        14827,         8228,       178039,        55365,
  11729020,      3725699,        91601,        29410,        15533,         8863,        10393,         8361,
      9368,         7899,       105865,       107646,       125931,        85278,       493390,        55737,
     28577,        15043,        91510,        29340,        15535,         8813,         9817,      1546955,
      3978,         3154,        87576,        27677,        14705,         8326,       178251,        55218,
     28591,        15103,        91683,        29339,        15609,         8697,         9995,         5079,
      3508,         3293,        87641,        27958,        15168,         8781,       178635,        56665,
     29570,        15632,        95835,        31600,        25580,        48166,       111564,       157226,
    308413,         4068,        87904,        27712,        14991,         9127,       178351,        55351,
     28716,      1556270,        91694,        29582,        15772,         8802,        10050,         5285,
      3431,         2818,        87363,        27479,        14466,         7767,       177825,        55019,
     28315,        14868,        90444,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        29824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,       184609,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,       526948,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,       838934,
     26960,        13568,        90742,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,       788678,
      1760,         1607,        86153,      2093884,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,      5482534,        13163,         6665,       176567,       838934,
     26960,        13568,        90032,        27824,        14000,         7088,        66173,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,       788678,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,       708069,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,       265218,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         3245,        86300,        26150,        13359,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13163,         6665,       176567,        53744,
     26960,        13568,        90032,        27824,        14000,         7088,         8285,         3488,
      1760,         1607,        86153,        26150,        13702,         6665,       157745,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
      9872,          230,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       176,          176,          176,          236,          176,          176,          176,          176,
       176,          176,          176,          176,          176,          176,          176,          176,
       664,          176,          176,          176,          176,          261,          176,          176,
       176,          176,          179,          176,          186,        15644,   8837095927,            0,
};
