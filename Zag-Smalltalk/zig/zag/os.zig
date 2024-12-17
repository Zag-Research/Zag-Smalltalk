// OS specific code
pub usingnamespace switch (@import("builtin").os.tag) {
    .macos => @import("os/macos.zig"),
    //    .linux => @import("os/unix.zig"),
    //    .windows => @import("os/windows.zig"),
    else => unreachable,
};
