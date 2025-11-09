const std = @import("std");
test {
    std.testing.refAllDecls(@This());
    // std.log.debug("Hello, world!", .{});
    // std.log.info("Hello, world!", .{});
    // std.log.warn("Hello, world!", .{});
    // std.log.err("Hello, world!", .{});
    _ = .{
        @import("config.zig"),
        @import("process.zig"),
        @import("object.zig"), // don't import individual encodings, because we just want the current one
        @import("threadedFn.zig"),
    };
}
