const std = @import("std");
const math = std.math;
const Order = math.Order;
pub fn Stats(comptime T:type) type {
    return struct {
        value: T,
        minValue : f64,
        maxValue : f64,
        n : usize,
        sum: f64,
        sumsq : f64,
        const Self = @This();
        pub fn init(v : T) Self {
            return .{
                .value = v,
                .minValue = math.inf(f64),
                .maxValue = -math.inf(f64),
                .n = 0,
                .sum = 0.0,
                .sumsq = 0.0};
        }
        pub fn addData(self : *Self, data : f64) void {
            self.n += 1;
            if (data < self.minValue) self.minValue = data;
            if (data > self.maxValue) self.maxValue = data;
            self.sum += data;
            self.sumsq += data*data;
        }
        pub fn mean(self : Self) f64 {
            return self.sum/@as(f64,@floatFromInt(self.n));
        }
        pub fn stddev(self : Self) f64 {
            const nInverse = 1.0/@as(f64,@floatFromInt(self.n));
            const mn = self.sum*nInverse;
            return math.sqrt(self.sumsq*nInverse-mn*mn);
        }
        pub fn min(self : Self) f64 {
            return self.minValue;
        }
        pub fn max(self : Self) f64 {
            return self.maxValue;
        }
        pub fn noData(self : Self) bool {
            return self.n==0;
        }
        pub fn format(
            self: *const Self,
            comptime fmt: []const u8,
            comptime options: std.fmt.FormatOptions,
            writer: anytype,
        ) !void {
            if (self.noData())
                {try writer.print("?",.{});}
            else {
                var sep: []const u8 = "";
                const opts: []const u8 = comptime if (options.precision==null) "{d}" else "{d:.2}";
                inline for (if (fmt.len==0) "nmxs" else fmt) |f| {
                    try writer.print("{s}",.{sep});
                    switch (f) {
                        'n' => try writer.print(opts,.{self.minValue}),
                        'm' => try writer.print(opts,.{self.mean()}),
                        'x' => try writer.print(opts,.{self.maxValue}),
                        's' => try writer.print(opts,.{self.stddev()}),
                        'r' => try writer.print(opts,.{(self.maxValue-self.minValue)/2}),
                        else => {},
                    }
                    sep = "--";
                }
            }
        }
    };
}
test "simple stats" {
    const expectEqual = @import("std").testing.expectEqual;
    const expect = @import("std").testing.expect;
    var stat = Stats(usize).init(42);
    stat.addData(2.0);
    stat.addData(4.0);
    try expectEqual(stat.min(),2.0);
    try expectEqual(stat.max(),4.0);
    try expectEqual(stat.mean(),3.0);
    try expectEqual(stat.stddev(),1.0);
    var buf: [200]u8 = undefined;
    var buf2: [200]u8 = undefined;
//    const ebuf: []const u8 = "2.0--3.0--4.0--1.0";
//    std.debug.print("\nstats {<FOO>nmxs}",.{stat});
    _=.{expect,buf,buf2};//    try expect(std.mem.eql(u8,try std.fmt.bufPrint(buf2[0..],"2--3--4--1",.{}),try std.fmt.bufPrint(buf[0..], "{}",.{stat})));
}
