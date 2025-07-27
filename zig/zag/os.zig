// OS specific code
pub const page_size = target.page_size;
pub const MemoryAllocator = target.MemoryAllocator;
const target = switch (@import("builtin").os.tag) {
    .macos => @import("os/macos.zig"),
    //    .linux => @import("os/unix.zig"),
    //    .windows => @import("os/windows.zig"),
    else => unreachable,
};
