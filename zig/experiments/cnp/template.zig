const std = @import("std");
const zag = @import("zag");
const Process = zag.Process;
const Context = zag.Context;
const Extra = Context.Extra;
const PC = zag.execute.PC;
const SP = Process.SP;
const Result = zag.execute.Result;
const TemplateScan = @import("template_scan.zig");

pub const ThreadedFn = *const fn (PC, SP, *Process, *Context, Extra) Result;

pub const TemplateInfo = struct {
    start: [*]const u8,
    size: usize,
    br_offsets: [8]usize,
    br_count: usize,
    last_branch_offset: ?usize,

    fn nextKnownTemplateStart(func: ThreadedFn) ?usize {
        const start_addr = @intFromPtr(func);
        var next_addr: usize = std.math.maxInt(usize);
        var found = false;

        for (zag.threadedFn.functions) |candidate| {
            const addr = @intFromPtr(candidate);
            if (addr > start_addr and addr < next_addr) {
                next_addr = addr;
                found = true;
            }
        }

        return if (found) next_addr else null;
    }

    pub fn analyze(func: ThreadedFn) TemplateInfo {
        const ptr: [*]const u8 = @ptrCast(func);
        const start_addr = @intFromPtr(func);
        const next_addr = nextKnownTemplateStart(func);
        var info = TemplateInfo{
            .start = ptr,
            .size = 0,
            .br_offsets = undefined,
            .br_count = 0,
            .last_branch_offset = null,
        };

        const hard_max_bytes: usize = 32 * 1024;
        const max_bytes =
            if (next_addr) |next| @min(next - start_addr, hard_max_bytes) else hard_max_bytes;

        const scan = TemplateScan.scan(ptr, max_bytes) catch @panic("Template scan failed");
        info.size = scan.size;
        info.br_count = scan.br_count;
        info.br_offsets = scan.br_offsets;
        info.last_branch_offset = scan.last_branch_offset;

        return info;
    }

    pub fn dump(self: *const TemplateInfo, name: []const u8) void {
        std.debug.print("  {s}: size={}, br_count={}\n", .{ name, self.size, self.br_count });
    }
};
