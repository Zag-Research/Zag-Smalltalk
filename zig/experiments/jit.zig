const std = @import("std");
const builtin = @import("builtin");

// 1. External C declarations for macOS specific functions
// We link libc (-lc) to access these.
extern "c" fn pthread_jit_write_protect_np(enabled: c_int) void;
extern "c" fn sys_icache_invalidate(start: ?*const anyopaque, size: usize) void;

pub fn main() !void {
    // 2. Define machine code
    // ARM64: mov w0, #42; ret
    const code_arm64 = [_]u8{ 0x40, 0x05, 0x80, 0x52, 0xc0, 0x03, 0x5f, 0xd6 };
    // x86_64: mov rax, 42; ret
    const code_x86   = [_]u8{ 0x48, 0xc7, 0xc0, 0x2a, 0x00, 0x00, 0x00, 0xc3 };

    const page_size = std.heap.page_size_min;

    // 3. Setup mmap flags
    // Zig's std.posix (or std.os in older versions) wrappers
    const PROT_RWX = std.posix.PROT.READ | std.posix.PROT.WRITE | std.posix.PROT.EXEC;

    // MAP_JIT is 0x0800 on macOS. It might not be in the Zig enum yet, so we define it manually.
    const MAP_FLAGS: std.posix.MAP = .{.TYPE=.PRIVATE, .ANONYMOUS = true, .JIT = true};

    // 4. Allocate Memory
    // This returns a slice []align(page_size) u8
    const mem = try std.posix.mmap(
        null,
        page_size,
        PROT_RWX,
        MAP_FLAGS,
        -1,
        0
    );
    // Ensure we unmap when done
    defer std.posix.munmap(mem);

    // 5. Enable Write Permissions (Apple Silicon only)
    // 0 = disable protection (allow write)
    if (builtin.cpu.arch == .aarch64) {
        pthread_jit_write_protect_np(0);
    }

    // 6. Copy code into memory
    if (builtin.cpu.arch == .aarch64) {
        @memcpy(mem[0..code_arm64.len], &code_arm64);
    } else {
        @memcpy(mem[0..code_x86.len], &code_x86);
    }

    // 7. Enable Execute Permissions (Apple Silicon only)
    // 1 = enable protection (allow execute)
    if (builtin.cpu.arch == .aarch64) {
        pthread_jit_write_protect_np(1);
    }

    // 8. Invalidate Instruction Cache
    sys_icache_invalidate(mem.ptr, page_size);

    // 9. Cast and Execute
    // We cast the pointer to a function pointer returning c_int
    const func_ptr: *const fn () callconv(.c) c_int = @ptrCast(mem.ptr);
    const result = func_ptr();

    std.debug.print("Result: {d}\n", .{result});
}
// Just like the C version,
//     - if you sign this binary (or run it as a full app bundle),
//     - you need the com.apple.security.cs.allow-jit entitlement, or mmap will fail
// XML:
//   <key>com.apple.security.cs.allow-jit</key>
//   <true/>
