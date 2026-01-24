// experiment to test adding a prefix to code blocks so that we could directly access them
const std = @import("std");
const builtin = @import("builtin");

// 1. External C declarations for macOS specific functions
// We link libc (-lc) to access these.
extern "c" fn pthread_jit_write_protect_np(enabled: c_int) void;
extern "c" fn sys_icache_invalidate(start: ?*const anyopaque, size: usize) void;
const aarch64 = struct {
    fn enable_write_permission() void {
        // 5. Enable Write Permissions (Apple Silicon only)
        // 0 = disable protection (allow write)
        if (builtin.cpu.arch == .aarch64) {
            pthread_jit_write_protect_np(0);
        }
    }
    fn disable_write_permission() void {
        // 5. Enable Write Permissions (Apple Silicon only)
        // 0 = disable protection (allow write)
        if (builtin.cpu.arch == .aarch64) {
            pthread_jit_write_protect_np(1);
        }
    }

    const call_prefix = [_]u8{ 0x50, 0x00, 0x00, 0x58, 0x00, 0x02, 0x3F, 0xD6 };
    pub const threaded_code_offset = 8;
    const header_length = call_prefix.len + @sizeOf(*const fn_type);
    fn write_call_code(mem: [*]align(@alignOf(fn_type)) u8, ptr: *const fn_type) *fn_type {
        @memcpy(mem[0..call_prefix.len], &call_prefix);
        const addr: **const fn_type = @ptrFromInt(@intFromPtr(mem) + call_prefix.len);
        addr.* = ptr;
        return @ptrCast(mem);
    }

    fn invalidate_instruction_cache(mem: []u8) void {
        sys_icache_invalidate(mem.ptr, mem.len);
    }
};
pub const arch = switch (builtin.cpu.arch) {
    .aarch64 => aarch64,
    else => unreachable,
};
pub const threaded_header_length = (@max(arch.threaded_code_offset, 0) + 7) & ~@as(usize, 7);
const fn_type = fn (sp: [*]u64) void;
fn expect(sp: [*]u64) void {
    const fpc: [*]u8 = @ptrFromInt(@returnAddress() + arch.threaded_code_offset);
    std.debug.print("returnAddress: 0x{x} fpc: {*}\n", .{ @returnAddress(), fpc });
    for (fpc[0..10], 0..) |*u, index|
        std.debug.print("{x}: fpc[{}]: 0x{x:0>2}\n", .{ @intFromPtr(u), index, u.* });
    for (sp[0..3], 0..) |u, index|
        std.debug.print("sp[{}]: 0x{x:0>16}\n", .{ index, u });
    std.process.exit(1);
}
pub fn main() !void {
    // This returns a slice []align(page_size) u8
    const mem = try std.posix.mmap(null, std.heap.page_size_min, std.posix.PROT.READ | std.posix.PROT.WRITE | std.posix.PROT.EXEC, .{ .TYPE = .PRIVATE, .ANONYMOUS = true, .JIT = true }, -1, 0);
    // Ensure we unmap when done
    defer std.posix.munmap(mem);

    arch.enable_write_permission();
    for (mem, 0..) |*m, i|
        m.* = @truncate(i);
    //const funcCall: *fn([*]u64) void = @ptrCast(mem.ptr);
    // 6. Copy code into memory
    const size = threaded_header_length;
    // x86_64: mov rax, 42; ret
    //const code_x86_prefix = [_]u8{ 0x48, 0xB8};
    //const code_x86_suffic = [_]u8{ 0xFF, 0xD0};

    const funcCall = arch.write_call_code(mem.ptr, &expect);
    arch.disable_write_permission();
    arch.invalidate_instruction_cache(mem);

    var stack = [_]u64{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    std.debug.print("expect: {*}\n", .{&expect});
    for (@as([*]u8, @constCast(@ptrCast(mem.ptr)))[0..size], 0..) |*u, index|
        std.debug.print("{x}: fpc[{}]: 0x{x:0>2}\n", .{ @intFromPtr(u), index, u.* });
    funcCall(&stack);
}
