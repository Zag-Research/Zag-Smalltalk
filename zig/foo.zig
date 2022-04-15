const std = @import("std");
const expect = std.testing.expect;
const expectEqual = std.testing.expectEqual;
const eql = std.mem.eql;
test "iter next" {
    const text = "robust, optimal, reusable, maintainable, ";
    var iter = std.mem.split(u8, text, ", ");
    try expect(eql(u8, iter.next().?, "robust"));
    try expect(eql(u8, iter.next().?, "optimal"));
    try expect(eql(u8, iter.next().?, "reusable"));
    try expect(eql(u8, iter.next().?, "maintainable"));
    try expect(eql(u8, iter.next().?, ""));
    try expect(iter.next() == null);
}
test "iter while" {
    const text = "robust, optimal, reusable, maintainable, ";
    var iter = std.mem.split(u8, text, ", ");
    var count : usize = 0;
    while (iter.next()) |str| {
        count += str.len;
    }
    try expectEqual(count,33);
}
// test "iter while with 'closure'" {
//     const text = [_]u16{1,4,-5,3};
//     var count : usize = 0;
//     var closure = .{
//         .t = text[0..],
//         .n = @as(isize,-1),
//         fn next(self:*@This()) u16 {self.n += 1;return if (self.n>=self.text.len) null else self.text[n];}
//     };
//     while (closure.next()) |value| {
//         count += value;
//     }
//     try expectEqual(count,3);
// }
test "point" {
    const Point = struct {
        x: i32,
        y: i32,
        const Self = @This();
        pub fn new(x: i32, y: i32) Self {
            return .{.x = x, .y = y};
        }
        pub fn abs(self: Self) Self {
            return new(if (self.x>=0) self.x else -self.x,if (self.y>=0) self.y else -self.y);
        }
    };
    const p1 = Point.new(3,-4);
    try expectEqual(p1.abs(),Point.new(3,4));
}
pub fn Point_(comptime T: type) type {
    return struct {
        x: T,
        y: T,
        const Self = @This();
        pub fn new(x: T, y: T) Self {
            return .{.x = x, .y = y,};
        }
        pub fn abs(self: Self) Self {
            return new(if (self.x>=0) self.x else -self.x,if (self.y>=0) self.y else -self.y);
        }
    };
}
fn foo(x: i32) i32 { return x; }
test "parametric point" {
    const Point_i32 = Point_(i32);
    const p1 = Point_i32.new(3,-4);
    try expectEqual(p1.abs(),Point_i32.new(3,4));
}
