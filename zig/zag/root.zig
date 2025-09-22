const std = @import("std");
//const zag = @import("zag");
test {
    //std.testing.refAllDeclsRecursive(zag);
    _ = .{
        @import("config.zig"),
        @import("utilities.zig"),
        @import("object/inMemory.zig"),
        @import("object/floatZag.zig"),
        @import("object/floatSpur.zig"),
        @import("object.zig"), // don't import individual encodings, because we just want the current one
        //@import("threadedFn.zig"),
    };
}
