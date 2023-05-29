pub const inlines = struct {
    pub usingnamespace @import("primitives/Object.zig").inlines;
    pub usingnamespace @import("primitives/Smallinteger.zig").inlines;
    pub usingnamespace @import("primitives/Behavior.zig").inlines;
};
pub const embedded = struct {
    pub usingnamespace @import("primitives/Object.zig").embedded;
    pub usingnamespace @import("primitives/Smallinteger.zig").embedded;
    pub usingnamespace @import("primitives/Behavior.zig").embedded;
    pub usingnamespace @import("execute.zig").controlPrimitives;
};
pub const primitives = struct {
    pub usingnamespace @import("primitives/Object.zig").primitives;
    pub usingnamespace @import("primitives/Smallinteger.zig").primitives;
    pub usingnamespace @import("primitives/Behavior.zig").primitives;
};
test "prim" {
    const std = @import("std");
    std.debug.print("dup = {*}\n",.{&embedded.dup});
}
