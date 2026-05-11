const PAGE_SIZE = std.heap.page_size_min;
memory: []align(PAGE_SIZE) u8,
pos: usize,

const Self = @This();

const c = @cImport({
    @cInclude("pthread.h");
    @cInclude("libkern/OSCacheControl.h");
    @cInclude("sys/mman.h");
});

pub fn init(size: usize) !Self {
    const aligned_size = std.mem.alignForward(usize, size, PAGE_SIZE);

    const memory = std.posix.mmap(
        null,
        aligned_size,
        std.posix.PROT.READ | std.posix.PROT.WRITE | std.posix.PROT.EXEC,
        .{ .TYPE = .PRIVATE, .ANONYMOUS = true },
        -1,
        0,
    ) catch |err| {
        if (builtin.os.tag == .macos) {
            const ptr = c.mmap(
                null,
                aligned_size,
                c.PROT_READ | c.PROT_WRITE | c.PROT_EXEC,
                c.MAP_PRIVATE | c.MAP_ANONYMOUS | c.MAP_JIT,
                -1,
                0,
            );
            if (ptr == c.MAP_FAILED) {
                return err;
            }
            const slice: []align(PAGE_SIZE) u8 = @alignCast(@as([*]u8, @ptrCast(ptr))[0..aligned_size]);
            return Self{ .memory = slice, .pos = 0 };
        }
        return err;
    };

    return Self{ .memory = memory, .pos = 0 };
}

pub fn deinit(self: *Self) void {
    std.posix.munmap(self.memory);
}

pub fn makeWritable(self: *Self) void {
    _ = self;
    if (comptime builtin.os.tag == .macos and builtin.cpu.arch == .aarch64) {
        c.pthread_jit_write_protect_np(0);
    }
}

pub fn makeExecutable(self: *Self) void {
    if (comptime builtin.os.tag == .macos and builtin.cpu.arch == .aarch64) {
        c.sys_icache_invalidate(@ptrCast(self.memory.ptr), self.memory.len);
        c.pthread_jit_write_protect_np(1);
    }
}

pub fn currentOffset(self: *const Self) usize {
    return self.pos;
}

pub fn getAddress(self: *const Self) *u8 {
    return &self.memory[self.pos];
}

/// Copies template bytes to the JIT buffer. Relocation is handled by the
/// architecture-specific CnP layer once that decoder/encoder is wired in.
pub fn copyTemplate(self: *Self, info: anytype) usize {
    const startPos = self.pos;
    const dst = self.memory[self.pos..][0..info.size];
    @memcpy(dst, info.start[0..info.size]);
    self.pos += info.size;
    return startPos;
}

/// Copy a slice of any type into the buffer
pub fn append(self: *Self, slice: anytype) usize {
    const byteSlice = std.mem.sliceAsBytes(slice);
    const startPos = self.pos;
    const dst = self.memory[self.pos..][0..byteSlice.len];
    @memcpy(dst, byteSlice);
    self.pos += byteSlice.len;
    return startPos;
}

pub fn getEntry(self: *Self, offset: usize) ThreadedFn {
    return @ptrCast(@alignCast(self.memory.ptr + offset));
}

pub const JitBuffer = @This();

const std = @import("std");
const builtin = @import("builtin");
const zag = @import("zag");
const Process = zag.Process;
const Context = zag.Context;
const Extra = Context.Extra;
const PC = zag.execute.PC;
const SP = Process.SP;
const Result = zag.execute.Result;

pub const ThreadedFn = *const fn (PC, SP, *Process, *Context, Extra) Result;
