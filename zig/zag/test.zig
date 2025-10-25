test {
    _ = .{
        @import("config.zig"),
        @import("process.zig"),
        //@import("utilities.zig"),
        //@import("object/inMemory.zig"),
        //@import("object/floatZag.zig"),
        //@import("object/floatSpur.zig"),
        //@import("object.zig"), // don't import individual encodings, because we just want the current one
        //@import("threadedFn.zig"),
    };
}
