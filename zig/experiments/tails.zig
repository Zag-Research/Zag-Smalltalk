const std = @import("std");
const math = std.math;
const _CO: std.builtin.CallOptions = .{.modifier = .always_tail};
const stdout = std.io.getStdOut().writer();

fn loop1(cur: u64, limit: u64) void {
    if (cur % 1000000 == 0) stdout.print("cur: {}\n",.{cur}) catch unreachable;
    return @call(_CO,loop2,.{cur,limit});
}
fn loop2(cur: u64, limit: u64) void {
    if (cur == limit) return;
//    return loop1(cur+1,limit);
    return @call(_CO,loop1,.{cur+1,limit});
}
test "looping" {
    loop1(0,10000000);
}
