const std = @import("std");
const zag = @import("zag");
const Process = zag.Process;
const Context = zag.Context;
const Extra = Context.Extra;
const PC = zag.execute.PC;
const SP = Process.SP;
const Result = zag.execute.Result;
const Arm64 = @import("arm64.zig").Arm64;

pub const ThreadedFn = *const fn (PC, SP, *Process, *Context, Extra) Result;

pub const TemplateInfo = struct {
    start: [*]const u8,
    size: usize,
    br_offsets: [8]usize,
    br_count: usize,
    last_branch_offset: ?usize,

    /// Scans native code to find template boundaries and branch locations.
    /// Detects function end by looking for:
    /// 1. RET instruction
    /// 2. Next function's prologue (after seeing at least one branch)
    pub fn analyze(func: ThreadedFn) TemplateInfo {
        const ptr: [*]const u8 = @ptrCast(func);
        var info = TemplateInfo{
            .start = ptr,
            .size = 0,
            .br_offsets = undefined,
            .br_count = 0,
            .last_branch_offset = null,
        };

        var i: usize = 0;
        const maxBytes: usize = 2048;

        while (i < maxBytes) : (i += 4) {
            const inst = std.mem.readInt(u32, ptr[i..][0..4], .little);

            // If we've seen at least one branch and hit a prologue,
            // we've entered the next function - stop here
            // subject to change 
            if (info.br_count > 0 and Arm64.isLikelyPrologue(inst)) {
                info.size = i;
                break;
            }

            if (Arm64.isBranchRegister(inst)) {
                if (info.br_count < info.br_offsets.len) {
                    info.br_offsets[info.br_count] = i;
                    info.br_count += 1;
                }
                info.last_branch_offset = i;
            }

            if (Arm64.isRet(inst)) {
                info.size = i + 4;
                break;
            }
        }

        return info;
    }

    pub fn dump(self: *const TemplateInfo, name: []const u8) void {
        std.debug.print("  {s}: size={}, br_count={}\n", .{ name, self.size, self.br_count });
    }
};
