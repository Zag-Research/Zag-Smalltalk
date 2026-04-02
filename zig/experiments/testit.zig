const std = @import("std");

noinline fn foo(i: u64) f64 {
    return @floatFromInt(i);
}
comptime {
    switch (@as(u64,@bitCast(0.0/foo(0)))) {
        0x7ff8000000000000,
        0xfff8000000000000 => {},
        else => @compileError("generated nan is unexpected"),
    }
}
pub fn main() void {
    std.debug.print("0.0/0.0 = {x}\n", .{@as(u64,@bitCast(0.0/foo(0)))});
    std.debug.print("-0.0/0.0 = {x}\n", .{@as(u64,@bitCast(-0.0/foo(0)))});
    std.debug.print("math.nan(f64) = {x}\n", .{@as(u64,@bitCast(std.math.nan(f64)))});
    std.debug.print("math.nan(f32) = {x}\n", .{@as(u32,@bitCast(std.math.nan(f32)))});
}
