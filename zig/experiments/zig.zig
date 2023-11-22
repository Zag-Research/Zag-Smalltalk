const Object = struct {
    data: u64,
    const Self = @This();
    fn init(data: u64) Self {
        return .{ .data = data };
    }
    fn drop(self: SP) SP {
        return self + 1;
    }
    fn top(self: SP) u64 {
        return self[0].data;
    }
};
const SP = [*]Object;
pub fn main() void {
    var a = [_]Object{.{ .data = 0 }} ** 5;
    var sp: SP = a[0..];
    {}
    @import("std").debug.print("sp.top() = {}\n", .{sp[0]});
}
