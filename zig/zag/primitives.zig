pub const inlines = struct {
    usingnamespace @import("primitives/object.zig").inlines;
    usingnamespace @import("primitives/smallInteger.zig").inlines;
    usingnamespace @import("execute.zig").inlines;
}
pub const embedded = struct {
    usingnamespace @import("primitives/object.zig").embedded;
    usingnamespace @import("primitives/smallInteger.zig").embedded;
    usingnamespace @import("execute.zig").embedded;
}
pub const primitives = struct {
    usingnamespace @import("primitives/object.zig").primitives;
    usingnamespace @import("primitives/smallInteger.zig").primitives;
    usingnamespace @import("execute.zig").primitives;
}
