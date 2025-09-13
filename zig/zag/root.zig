const std = @import("std");
//const zag = @import("zag");
test {
    //std.testing.refAllDeclsRecursive(zag);
    _ = .{
        @import("utilities.zig"),
        @import("object/inMemory.zig"),
        @import("object/floatZag.zig"),
        @import("object/floatSpur.zig"),
        //@import("threadedFn.zig"),
    };
}
