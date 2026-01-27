// experiment to test adding a prefix to code blocks so that we could directly access them
// to compile for lldb
// zig build-exe jit.zig -O Debug -fomit-frame-pointer -fno-unwind-tables && ./jit
// on x86_64 linux you have to add the -lc
// zig build-exe jit.zig -lc -O Debug -fomit-frame-pointer -fno-unwind-tables && ./jit
const std = @import("std");
const builtin = @import("builtin");
const native_endian = builtin.target.cpu.arch.endian();
const zag = @import("zag.zig");

const common = struct {
    extern "c" fn pthread_jit_write_protect_np(enabled: c_int) void;
    fn mmap(size: usize) ![]align(std.heap.page_size_min) u8 {
        switch (builtin.os.tag) {
            .windows => {
                zag.untested();
                const MEM_COMMIT = std.os.windows.MEM_COMMIT;
                const MEM_RESERVE = std.os.windows.MEM_RESERVE;
                const PAGE_READWRITE = std.os.windows.PAGE_READWRITE;
                const ptr = std.os.windows.VirtualAlloc(
                    null,
                    size,
                    MEM_COMMIT | MEM_RESERVE,
                    PAGE_READWRITE,
                ) catch return error.OutOfMemory;
                return @as([*]u8, @ptrCast(ptr))[0..size];
            },
            .macos => {
                const prot_rwx = std.posix.PROT.READ | std.posix.PROT.WRITE | std.posix.PROT.EXEC;
                return std.posix.mmap(null, size, prot_rwx, .{ .TYPE = .PRIVATE, .ANONYMOUS = true, .JIT = true }, -1, 0);
            },
            .linux => {
                const prot_rwx = std.posix.PROT.READ | std.posix.PROT.WRITE | std.posix.PROT.EXEC;
                return std.posix.mmap(null, size, prot_rwx, .{ .TYPE = .PRIVATE, .ANONYMOUS = true }, -1, 0);
            },
            else => unreachable,
        }
    }

    fn munmap(mem: []align(std.heap.page_size_min) u8) void {
        switch (builtin.os.tag) {
            .windows => {
                zag.untested();
                _ = std.os.windows.VirtualFree(mem.ptr, 0, std.os.windows.MEM_RELEASE);
            },
            .macos, .linux => std.posix.munmap(mem),
            else => unreachable,
        }
    }

    fn enable_write_permission(mem: []align(std.heap.page_size_min) u8) !void {
        switch (builtin.os.tag) {
            .macos => pthread_jit_write_protect_np(0),
            .windows => {
                zag.untested();
                const PAGE_READ_WRITE = std.os.windows.PAGE_READ_WRITE;
                var old_prot: u32 = undefined;

                // 1. Change Permissions
                // Note: Zig's std.os.windows.VirtualProtect wrapper might be minimal,
                // so we call the kernel32 function directly if needed, but standard library usage is:
                const res = std.os.windows.VirtualProtect(
                    mem.ptr,
                    mem.len,
                    PAGE_READ_WRITE,
                    &old_prot,
                );
                if (res == 0) return error.AccessDenied;
            },
            .linux => {
                // We must remove execute permission to satisfy W^X
                const prot_rw = std.posix.PROT.READ | std.posix.PROT.WRITE;
                return std.posix.mprotect(mem, prot_rw);
            },
            else => unreachable,
        }
    }
};

const aarch64 = struct {
    const mmap = common.mmap;
    const munmap = common.munmap;
    extern "c" fn pthread_jit_write_protect_np(enabled: c_int) void;
    extern "c" fn sys_icache_invalidate(start: ?*const anyopaque, size: usize) void;
    pub const enable_write_permission = common.enable_write_permission;
    extern "c" fn __clear_cache(start: [*]u8, end: [*]u8) void;
    pub fn prepare_to_execute(mem: []align(std.heap.page_size_min) u8) !void {
        switch (builtin.os.tag) {
            .macos => {
                pthread_jit_write_protect_np(1);
                sys_icache_invalidate(mem.ptr, mem.len);
            },
            .windows => {
                zag.untested();
                var old_protection: std.os.windows.PROTECTION_TYPE = undefined;
                _ = std.os.windows.VirtualProtect(mem.ptr, mem.len, std.os.windows.PAGE_EXECUTE_READ, &old_protection);
                const process = std.os.windows.GetCurrentProcess();
                _ = std.os.windows.kernel32.FlushInstructionCache(
                    process,
                    mem.ptr,
                    mem.len,
                );
            },
            .linux => {
                try std.posix.mprotect(mem, std.posix.PROT.READ | std.posix.PROT.EXEC);
                __clear_cache(mem.ptr, mem.ptr + mem.len);
            },
            else => unreachable,
        }
    }

    const call_prefix = [_]u8{ 0x50, 0x00, 0x00, 0x58, 0x00, 0x02, 0x3F, 0xD6 };
    pub const threaded_code_offset = 8;
    const header_length = call_prefix.len + @sizeOf(usize);
    pub fn write_call_code(ptr: anytype, mem: [*]align(@alignOf(@TypeOf(ptr))) u8) @TypeOf(ptr) {
        @memcpy(mem[0..call_prefix.len], &call_prefix);
        const suffix_offset = call_prefix.len + @sizeOf(usize);
        std.mem.writeInt(u64, mem[call_prefix.len..suffix_offset], @intFromPtr(ptr), native_endian);
        return @ptrCast(mem);
    }
};
const x86_64 = struct {
    const mmap = common.mmap;
    const munmap = common.munmap;
    pub const enable_write_permission = common.enable_write_permission;
    extern "c" fn pthread_jit_write_protect_np(enabled: c_int) void;
    pub fn prepare_to_execute(mem: []align(std.heap.page_size_min) u8) !void {
        switch (builtin.os.tag) {
            .macos => pthread_jit_write_protect_np(1),
            .windows => {
                zag.untested();
                const PAGE_EXECUTE_READ = std.os.windows.PAGE_EXECUTE_READ;
                var old_prot: u32 = undefined;

                // 1. Change Permissions
                // Note: Zig's std.os.windows.VirtualProtect wrapper might be minimal,
                // so we call the kernel32 function directly if needed, but standard library usage is:
                const res = std.os.windows.VirtualProtect(
                    mem.ptr,
                    mem.len,
                    PAGE_EXECUTE_READ,
                    &old_prot,
                );
                if (res == 0) return error.AccessDenied;

                // 2. Flush Cache (Required for AARCH64, good practice for x64)
                // -1 as process handle means "current process"
                _ = std.os.windows.kernel32.FlushInstructionCache(
                    std.os.windows.GetCurrentProcess(),
                    mem.ptr,
                    mem.len,
                );
            },
            .linux => {
                // We must remove Write permission to satisfy W^X
                const prot_rx = std.posix.PROT.READ | std.posix.PROT.EXEC;
                return std.posix.mprotect(mem, prot_rx);
            },
            else => unreachable,
        }
    }

    const call_prefix = [_]u8{ 0x49, 0xBB };
    const call_suffix = [_]u8{ 0x41, 0xFF, 0xD3 };
    pub const threaded_code_offset = 3;
    const header_length = call_prefix.len + @sizeOf(usize) + call_suffix.len;
    pub fn write_call_code(ptr: anytype, mem: [*]align(@alignOf(@TypeOf(ptr))) u8) @TypeOf(ptr) {
        @memcpy(mem[0..call_prefix.len], &call_prefix);
        const suffix_offset = call_prefix.len + @sizeOf(usize);
        std.mem.writeInt(u64, mem[call_prefix.len..suffix_offset], @intFromPtr(ptr), native_endian);
        @memcpy(mem[suffix_offset .. suffix_offset + call_suffix.len], &call_suffix);
        return @ptrCast(mem);
    }
};

pub const arch = switch (builtin.cpu.arch) {
    .aarch64 => aarch64,
    .x86_64 => x86_64,
    else => unreachable,
};
pub const threaded_header_length = (@max(
    aarch64.header_length,
    x86_64.header_length,
) + 7) & ~@as(usize, 7);

//* for testing purposes
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
    const mem = try arch.mmap(std.heap.page_size_min);
    defer arch.munmap(mem);

    try arch.enable_write_permission(mem);
    for (mem, 0..) |*m, i|
        m.* = @truncate(i);
    const size = threaded_header_length;

    const funcCall = arch.write_call_code(&expect, mem.ptr);
    try arch.prepare_to_execute(mem);

    var stack = [_]u64{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 };
    std.debug.print("expect: {*}\n", .{&expect});
    for (@as([*]u8, @ptrCast(@constCast(mem.ptr)))[0..size], 0..) |*u, index|
        std.debug.print("{x}: fpc[{}]: 0x{x:0>2}\n", .{ @intFromPtr(u), index, u.* });
    funcCall(&stack);
}
