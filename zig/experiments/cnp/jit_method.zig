const std = @import("std");
const zag = @import("zag");
const threadedFn = zag.threadedFn;
const tf = threadedFn.Enum;
const Process = zag.Process;
const Context = zag.Context;
const Extra = Context.Extra;
const PC = zag.execute.PC;
const SP = Process.SP;
const Result = zag.execute.Result;
const Code = zag.execute.Code;
const JitBuffer = @import("jit_buffer.zig").JitBuffer;
const TemplateInfo = @import("template.zig").TemplateInfo;

pub const ThreadedFn = *const fn (PC, SP, *Process, *Context, Extra) Result;

/// Operations that can have their tail-calls patched to direct branches.
pub fn isSupportedOp(op: tf) bool {
    return switch (op) {
        .push, .pushLiteral, .dup, .drop, .returnSelf, .returnTop => true,
        else => false,
    };
}

/// JIT-compiled method that copies and patches threaded function templates.
pub fn JitMethod(comptime ops: []const tf) type {
    return struct {
        const Self = @This();
        const num_ops = ops.len;

        jit: JitBuffer,
        entry_offset: usize,
        end_offset: usize,
        entry_offsets: [num_ops]usize,
        template_infos: [num_ops]TemplateInfo,
        end_info: TemplateInfo,

        pub fn init() !Self {
            const patch_tail_calls = true;
            var patched_branches: usize = 0;
            var template_infos: [num_ops]TemplateInfo = undefined;
            inline for (ops, 0..) |op, i| {
                template_infos[i] = TemplateInfo.analyze(threadedFn.threadedFn(op));
            }
            const end_info = TemplateInfo.analyze(Code.end);

            var total_size: usize = 0;
            for (template_infos) |info| total_size += info.size;
            total_size += end_info.size;

            var jit = try JitBuffer.init(total_size + 4096); // need to adjust accordingly
            jit.makeWritable();

            var offsets: [num_ops + 1]usize = undefined;
            for (&template_infos, 0..) |*info, i| {
                offsets[i] = jit.copyTemplate(info);
            }
            offsets[num_ops] = jit.copyTemplate(&end_info);

            if (patch_tail_calls) {
                for (0..num_ops) |i| {
                    if (!isSupportedOp(ops[i]) or (i + 1 < num_ops and !isSupportedOp(ops[i + 1]))) {
                        continue;
                    }
                    const info = &template_infos[i];
                    const curr_offset = offsets[i];
                    const next_offset = offsets[i + 1];
                    if (info.last_branch_offset) |br_offset| {
                        jit.patchBranch(curr_offset, br_offset, next_offset);
                        patched_branches += 1;
                    }
                }
            }

            jit.makeExecutable();

            if (patch_tail_calls and patched_branches == 0) {
                std.debug.print("warning: no tail-call patches applied\n", .{});
            }

            return Self{
                .jit = jit,
                .entry_offset = offsets[0],
                .end_offset = offsets[num_ops],
                .entry_offsets = offsets[0..num_ops].*,
                .template_infos = template_infos,
                .end_info = end_info,
            };
        }

        pub fn deinit(self: *Self) void {
            self.jit.deinit();
        }

        pub fn getEntry(self: *Self) ThreadedFn {
            return self.jit.getEntry(self.entry_offset);
        }

        pub fn getEntryFor(self: *Self, op_index: usize) ThreadedFn {
            return self.jit.getEntry(self.entry_offsets[op_index]);
        }

        pub fn patchOp(self: *Self, code: []Code, op_index: usize, op_position: usize) void {
            code[op_position] = Code.primOf(self.getEntryFor(op_index));
        }

        pub fn dump(self: *const Self) void {
            std.debug.print("JitMethod with {} ops:\n", .{num_ops});
            inline for (ops, 0..) |op, i| {
                self.template_infos[i].dump(@tagName(op));
            }
            self.end_info.dump("end");
        }
    };
}
