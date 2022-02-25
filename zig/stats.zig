const math = @import("std").math;
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
                .minValue = math.inf_f64,
                .maxValue = -math.inf_f64,
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
            return self.sum/@intToFloat(f64,self.n);
        }
        pub fn stddev(self : Self) f64 {
            const nInverse = 1.0/@intToFloat(f64,self.n);
            const mn = self.sum*nInverse;
            return math.sqrt(self.sumsq*nInverse-mn*mn);
        }
        pub fn min(self : Self) f64 {
            return self.minValue;
        }
        pub fn max(self : Self) f64 {
            return self.maxValue;
        }
        pub fn largestMin(_: void, a: @This(), b: @This()) Order {
            if (a.min>b.min) return Order.lt;
            return Order.gt;
        }
    };
}
test "simple stats" {
    const expectEqual = @import("std").testing.expectEqual;
    var stat = Stats(usize).init(42);
    stat.addData(2.0);
    stat.addData(4.0);
    try expectEqual(stat.min(),2.0);
    try expectEqual(stat.max(),4.0);
    try expectEqual(stat.mean(),3.0);
    try expectEqual(stat.stddev(),1.0);
}
