const std = @import("std");

noinline fn foo(i: u64) f64 {
    return @floatFromInt(i);
}
pub fn main() void {
    std.debug.print("nan = {x}", .{@as(u64,@bitCast(0.0/foo(0)))});
}
