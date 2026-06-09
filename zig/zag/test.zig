const std = @import("std");
test {
    _ = .{
        @import("config.zig"),
        @import("object.zig"), // don't import individual encodings, because we just want the current one
        @import("process.zig"),
        @import("threadedFn.zig"),
    };
    std.testing.refAllDecls(@This());
}
