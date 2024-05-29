const std = @import("std");
export fn foo(x: u64) u64 {
    return x+1;
}

extern fn bar(x: u64) u64;

pub fn main() void {
    std.debug.print("Hello {}\n",.{bar(39)});
}
