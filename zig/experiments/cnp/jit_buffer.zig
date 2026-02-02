const std = @import("std");
const builtin = @import("builtin");
const zag = @import("zag");
const Process = zag.Process;
const Context = zag.Context;
const Extra = Context.Extra;
const PC = zag.execute.PC;
const SP = Process.SP;
const Result = zag.execute.Result;
const Arm64 = @import("arm64.zig").Arm64;
const TemplateInfo = @import("template.zig").TemplateInfo;

pub const ThreadedFn = *const fn (PC, SP, *Process, *Context, Extra) Result;

pub const JitBuffer = struct {
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

    /// Relocates PC-relative instructions when copying to new address.
    fn relocateInst(orig_pc: usize, new_pc: usize, inst: u32) u32 {
        if (Arm64.isAdr(inst)) {
            const target = @as(i64, @intCast(orig_pc)) + Arm64.decodeAdrImm(inst);
            const new_imm = target - @as(i64, @intCast(new_pc));
            std.debug.assert((new_imm & 0x3) == 0);
            std.debug.assert(new_imm >= -(@as(i64, 1) << 20) and new_imm < (@as(i64, 1) << 20));
            return Arm64.encodeAdrImm(inst, new_imm);
        }
        if (Arm64.isAdrp(inst)) {
            const orig_base = @as(i64, @intCast(orig_pc & ~@as(usize, 0xfff)));
            const target = orig_base + (Arm64.decodeAdrImm(inst) << 12);
            const new_base = @as(i64, @intCast(new_pc & ~@as(usize, 0xfff)));
            const new_imm = target - new_base;
            std.debug.assert((new_imm & 0xfff) == 0);
            std.debug.assert(new_imm >= -(@as(i64, 1) << 32) and new_imm < (@as(i64, 1) << 32));
            return Arm64.encodeAdrImm(inst, new_imm >> 12);
        }
        if (Arm64.isLdrLiteral64(inst)) {
            const target = @as(i64, @intCast(orig_pc)) + Arm64.decodeLdrLiteralImm(inst);
            const new_imm = target - @as(i64, @intCast(new_pc));
            std.debug.assert((new_imm & 0x3) == 0);
            std.debug.assert(new_imm >= -(@as(i64, 1) << 20) and new_imm < (@as(i64, 1) << 20));
            return Arm64.encodeLdrLiteralImm(inst, new_imm);
        }
        if (Arm64.isBImm(inst) or Arm64.isBlImm(inst)) {
            const target = @as(i64, @intCast(orig_pc)) + Arm64.decodeBImm(inst);
            const new_imm = target - @as(i64, @intCast(new_pc));
            std.debug.assert((new_imm & 0x3) == 0);
            std.debug.assert(new_imm >= -(@as(i64, 1) << 27) and new_imm < (@as(i64, 1) << 27));
            return Arm64.encodeBImm(inst, new_imm);
        }
        return inst;
    }

    /// Copies template to JIT buffer, relocating PC-relative instructions.
    pub fn copyTemplate(self: *Self, info: *const TemplateInfo) usize {
        const startPos = self.pos;
        const dst = self.memory[self.pos..][0..info.size];
        @memcpy(dst, info.start[0..info.size]);
        var i: usize = 0;
        while (i + 4 <= info.size) : (i += 4) {
            const inst = std.mem.readInt(u32, dst[i..][0..4], .little);
            const orig_pc = @intFromPtr(info.start + i);
            const new_pc = @intFromPtr(self.memory.ptr + startPos + i);
            const relocated = relocateInst(orig_pc, new_pc, inst);
            if (relocated != inst) {
                std.mem.writeInt(u32, dst[i..][0..4], relocated, .little);
            }
        }
        self.pos += info.size;
        return startPos;
    }

    /// Patches indirect branch (BR Xn) to direct branch (B imm).
    pub fn patchBranch(self: *Self, templateOffset: usize, brOffset: usize, targetOffset: usize) void {
        const patchAddr = templateOffset + brOffset;
        const offset: i32 = @intCast(@as(i64, @intCast(targetOffset)) - @as(i64, @intCast(patchAddr)));
        const inst = Arm64.encodeBranch(offset);
        std.mem.writeInt(u32, self.memory[patchAddr..][0..4], inst, .little);
    }

    pub fn getEntry(self: *Self, offset: usize) ThreadedFn {
        return @ptrCast(@alignCast(self.memory.ptr + offset));
    }
};
