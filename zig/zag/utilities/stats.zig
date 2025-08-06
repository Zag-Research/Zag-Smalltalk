const std = @import("std");
const math = std.math;
const Order = math.Order;
const Units = enum {
    units,
    seconds,
    milliseconds,
    microseconds,
    nanoseconds,
    fn name(self: Units) []const u8 {
        return switch (self) {
            .units => "units",
            .seconds => "seconds",
            .milliseconds => "milliseconds",
            .microseconds => "microseconds",
            .nanoseconds => "nanoseconds",
        };
    }
    fn shortName(self: Units) []const u8 {
        return switch (self) {
            .units => "un",
            .seconds => "s ",
            .milliseconds => "ms",
            .microseconds => "us",
            .nanoseconds => "ns",
        };
    }
    fn scale(self: Units) u64 {
        return switch (self) {
            .units => 1,
            .seconds => 1_000_000_000,
            .milliseconds => 1_000_000,
            .microseconds => 1_000,
            .nanoseconds => 1,
        };
    }
};
pub fn Stats(comptime Arg: type, comptime K: type, comptime runs: comptime_int, comptime units: Units) type {
    const T = if (K == void) u64 else K;
    const A = if (Arg == void) u64 else Arg;
    return struct {
        values: [runs]T = undefined,
        minValue: T = undefined,
        maxValue: T = undefined,
        n: usize = 0,
        sum: T = 0,
        sumsq: T = 0,
        proof: usize = 0,
        const Self = @This();
        const isInt = switch (@typeInfo(T)) {
            .int => true,
            else => false,
        };
        const warmups = @min(3, @max(1, (runs + 1) / 3));
        const scale = units.scale();
        pub fn init() Self {
            return .{};
        }
        pub fn reset(self: *Self) void {
            self.n = 0;
            self.sum = 0;
            self.sumsq = 0;
        }
        pub fn run(self: *Self, runner: *const fn (usize) T) void {
            for (0..warmups) |_| _ = runner(0);
            for (1..runs + 1) |runNumber| {
                self.addData(runner(runNumber));
            }
        }
        pub fn time(self: *Self, runner: *const fn (A, usize) usize, comptime param: anytype) void {
            for (0..warmups) |_| {
                self.proof = @call(.never_inline, runner, .{ if (A == u64) 0 else param, self.proof });
            }
            var timer = std.time.Timer.start() catch @panic("no timer available");
            var diff: usize = 0;
            for (0..runs) |runNumber| {
                self.proof = @call(.never_inline, runner, .{ if (A == u64) runNumber + 1 else param, self.proof });
                diff = timer.lap() / scale;
                self.addData(diff);
            }
        }
        pub fn addData(self: *Self, data: T) void {
            if (self.n == 0 or data < self.minValue) self.minValue = data;
            if (self.n == 0 or data > self.maxValue) self.maxValue = data;
            if (runs > self.n) {
                var i = self.n;
                while (i > 0) : (i -= 1) {
                    if (self.values[i - 1] <= data) break;
                    self.values[i] = self.values[i - 1];
                }
                self.values[i] = data;
            }
            self.sum += data;
            self.sumsq += data * data;
            self.n += 1;
        }
        pub fn median(self: *Self) ?T {
            if (runs == 0 or runs < self.n) return null;
            return if (isInt or self.n % 2 == 1) self.values[self.n / 2] else (self.values[self.n / 2 - 1] + self.values[self.n / 2]) / 2;
        }
        pub fn mean(self: Self) T {
            return if (isInt) self.sum / self.n else self.sum / @as(f64, @floatFromInt(self.n));
        }
        inline fn asFloat(v: T) f64 {
            return if (isInt) @floatFromInt(v) else v;
        }
        pub fn stdDev(self: Self) f64 {
            const nInverse = 1.0 / @as(f64, @floatFromInt(self.n));
            const mn = asFloat(self.sum) * nInverse;
            return math.sqrt(asFloat(self.sumsq) * nInverse - mn * mn);
        }
        pub fn min(self: Self) T {
            return self.minValue;
        }
        pub fn max(self: Self) T {
            return self.maxValue;
        }
        pub fn noData(self: Self) bool {
            return self.n == 0;
        }
        pub fn format(
            self: *const Self,
            comptime fmt: []const u8,
            options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            if (self.noData()) {
                try writer.print("?", .{});
            } else {
                const opts: []const u8 = comptime if (isInt) "{}" else "{d:.2}";
                var percent = false;
                inline for (if (fmt.len == 0) "n--m--x--s" else fmt) |f| {
                    switch (f) {
                        'n' => try writer.print(opts, .{self.minValue}),
                        'm' => try writer.print(opts, .{self.mean()}),
                        'M' => try writer.print(opts, .{self.median()}),
                        'x' => try writer.print(opts, .{self.maxValue}),
                        'r' => try writer.print(opts, .{(self.maxValue - self.minValue) / 2}),
                        '%' => percent = true,
                        's' => {
                            if (percent) {
                                percent = false;
                                const divisor = if (isInt) @as(f64, @floatFromInt(self.mean())) else self.mean();
                                if (divisor == 0.0) {
                                    try writer.print("0", .{});
                                } else {
                                    try writer.print("{d:.1}%", .{self.stdDev() * 100 / divisor});
                                }
                            } else if (options.precision == null) {
                                try writer.print("{d}", .{self.stdDev()});
                            } else try writer.print("{d:.2}", .{self.stdDev()});
                        },
                        // change % to give a percentile or express stdDev as a percentage
                        '+' => try writer.print("±", .{}),
                        'u' => try writer.print("{s}", .{units.shortName()}),
                        'U' => try writer.print("{s}", .{units.name()}),
                        else => try writer.print("{c}", .{f}),
                    }
                }
            }
        }
    };
}
test "simple int stats" {
    const expectEqual = @import("std").testing.expectEqual;
    const expect = @import("std").testing.expect;
    var stat = Stats(void, usize, 0, .units).init();
    stat.addData(2);
    stat.addData(4);
    try expectEqual(stat.min(), 2);
    try expectEqual(stat.max(), 4);
    try expectEqual(stat.mean(), 3);
    try expectEqual(stat.median(), null);
    try expectEqual(stat.stdDev(), 1.0);
    var buf: [200]u8 = undefined;
    var buf2: [200]u8 = undefined;
    try expect(std.mem.eql(u8, try std.fmt.bufPrint(&buf2, "2--3--4--1", .{}), try std.fmt.bufPrint(&buf, "{}", .{stat})));
}
test "simple int stats with values" {
    const expectEqual = @import("std").testing.expectEqual;
    var stat = Stats(void, usize, 10, .units).init();
    stat.addData(2);
    stat.addData(4);
    try expectEqual(stat.min(), 2);
    try expectEqual(stat.max(), 4);
    try expectEqual(stat.mean(), 3);
    try expectEqual(stat.median(), 4);
    try expectEqual(stat.stdDev(), 1.0);
    stat.addData(3);
    try expectEqual(stat.mean(), 3);
    try expectEqual(stat.median(), 3);
    try expectEqual(stat.stdDev(), 0.8164965809277257);
}
fn testRunner(run: usize) usize {
    return run * 2 - (if (run == 3 or run > 8) run else 0);
}
test "simple int stats with runner" {
    const expectEqual = @import("std").testing.expectEqual;
    var stat = Stats(void, usize, 3, .units).init();
    stat.run(testRunner);
    try expectEqual(stat.min(), 2);
    try expectEqual(stat.max(), 4);
    try expectEqual(stat.mean(), 3);
    try expectEqual(stat.median(), 3);
    try expectEqual(stat.stdDev(), 0.8164965809277257);
}
test "larger int stats with runner" {
    const expectEqual = @import("std").testing.expectEqual;
    var stat = Stats(void, usize, 10, .units).init();
    stat.run(testRunner);
    try expectEqual(stat.min(), 2);
    try expectEqual(stat.max(), 16);
    try expectEqual(stat.mean(), 8);
    try expectEqual(stat.median(), 10);
    try expectEqual(stat.stdDev(), 4.422668877499195);
    //std.debug.print("\nstats {nmxsrM}",.{stat});
}
test "simple float stats" {
    const expectEqual = @import("std").testing.expectEqual;
    const expect = @import("std").testing.expect;
    var stat = Stats(void, f64, 0, .units).init();
    stat.addData(2.0);
    stat.addData(4.0);
    try expectEqual(stat.min(), 2.0);
    try expectEqual(stat.max(), 4.0);
    try expectEqual(stat.mean(), 3.0);
    try expectEqual(stat.stdDev(), 1.0);
    const buf: [200]u8 = undefined;
    const buf2: [200]u8 = undefined;
    //    const ebuf: []const u8 = "2.0--3.0--4.0--1.0";
    //    std.debug.print("\nstats {<FOO>nmxs}",.{stat});
    _ = .{ expect, buf, buf2 }; //    try expect(std.mem.eql(u8,try std.fmt.bufPrint(buf2[0..],"2--3--4--1",.{}),try std.fmt.bufPrint(buf[0..], "{}",.{stat})));
}
fn timeRunner(comptime _: usize, proof: usize) usize {
    return proof;
}
test "simple timed stats" {
    const expect = @import("std").testing.expect;
    var stat = Stats(usize, void, 3, .nanoseconds).init();
    stat.time(timeRunner, 0);
    try expect(stat.max() > 0);
    try expect(stat.mean() > 0);
}
