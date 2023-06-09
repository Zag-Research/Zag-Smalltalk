pub const inlines = struct {
    pub usingnamespace @import("primitives/Object.zig").inlines;
    pub usingnamespace @import("primitives/Smallinteger.zig").inlines;
    pub usingnamespace @import("primitives/Behavior.zig").inlines;
    pub usingnamespace @import("primitives/BlockClosure.zig").inines;
    pub usingnamespace @import("primitives/Boolean.zig").inlines;
};
pub const embedded = struct {
    pub usingnamespace @import("primitives/Object.zig").embedded;
    pub usingnamespace @import("primitives/Smallinteger.zig").embedded;
    pub usingnamespace @import("primitives/Behavior.zig").embedded;
    pub usingnamespace @import("primitives/BlockClosure.zig").embedded;
    pub usingnamespace @import("primitives/Boolean.zig").embedded;
    pub usingnamespace @import("execute.zig").controlPrimitives;
};
pub const primitives = struct {
    pub usingnamespace @import("primitives/Object.zig").primitives;
    pub usingnamespace @import("primitives/Smallinteger.zig").primitives;
    pub usingnamespace @import("primitives/Behavior.zig").primitives;
    pub usingnamespace @import("primitives/BlockClosure.zig").primitives;
    pub usingnamespace @import("primitives/Boolean.zig").primitives;
};
pub fn init() void{
    @import("primitives/Object.zig").init();
    @import("primitives/Smallinteger.zig").init();
    @import("primitives/Behavior.zig").init();
    @import("primitives/BlockClosure.zig").init();
    @import("primitives/Boolean.zig").init();
//    @import("execute.zig").init();
}
test "prim" {
    const std = @import("std");
    std.debug.print("dup = {*}\n",.{&embedded.dup});
}
