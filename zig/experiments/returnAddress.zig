// experiment to test adding a prefix to code blocks so that we could directly access them
const std = @import("std");
fn expect(sp: [*]u64) void {
    const fpc: [*]u64 = @ptrFromInt((@returnAddress() + 7) & 0xFFFF_FFFF_FFFF_FFF8);
    std.debug.print("returnAddress: 0x{x} fpc: {*}\n", .{@returnAddress(), fpc});
    for (fpc[0..10], 0..) |*u, index|
        std.debug.print("{x}: fpc[{}]: 0x{x:0>16}\n", .{@intFromPtr(u),index,u.*});
    for (sp[0..4], 0..) |u, index|
            std.debug.print("sp[{}]: 0x{x:0>16}\n", .{index,u});
    std.process.exit(1);
}
fn callExpect(sp: [*]u64) align(@alignOf(*u64)) void {
    @call(.never_inline,expect,.{sp});
}
pub fn main() void {
    const size = 4;
    var func = [_]u64{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    const funcCall: *fn([*]u64) void = @ptrCast(&func);
    var stack = [_]u64{0, 1, 2, 3, 4, 5, 6, 7, 8, 9};
    for (@as([*]u64,@constCast(@ptrCast(&callExpect)))[0..size], 0..) |*u, index|
        std.debug.print("{x}: cE[{}]: 0x{x:0>16}\n", .{@intFromPtr(u),index,u.*});
    @memcpy(func[0..size],@as([*]u64,@constCast(@ptrCast(&callExpect))));
    for (&func, 0..) |*u, index|
        std.debug.print("{x}: fpc[{}]: 0x{x:0>16}\n", .{@intFromPtr(u),index,u.*});
//    @call(.never_inline,callExpect,.{&stack});
    funcCall(&stack);
}
