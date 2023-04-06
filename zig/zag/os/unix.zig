// Originally from https://github.com/microsoft/mimalloc/blob/master/src/prim/unix/prim.c
const std = @import("std");
const os = std.os;
const c = std.c;
//const SC = os.SC;
const SC_PAGESIZE = 29;
extern "c" fn sysconf(sc: c_int) i64;

pub fn BlockAllocation(comptime pageSize: comptime_int) type {
    return struct {
        size: u32,
        const Self = @This();
        pub fn new() Self {
            return .{.size=pageSize};
        }
    };
}
test "nothing" {
    _ = BlockAllocation(64*1024);
    const root = @import("root");
    const builtin = @import("builtin");
    try std.testing.expect(!@hasDecl(root, "os"));
    try std.testing.expect(builtin.link_libc);
    try std.testing.expect(!@hasDecl(c, "SC"));
    try std.testing.expect(@hasDecl(os, "SC"));
    try std.testing.expectEqual(sysconf(SC_PAGESIZE),std.mem.page_size);
}
