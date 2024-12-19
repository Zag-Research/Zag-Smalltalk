const main = @import("zig-separate.zig");

export fn bar(x: u64) u64 {
    return main.bar(x + 2);
}
