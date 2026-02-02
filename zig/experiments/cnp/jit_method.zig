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
        .branchFalse, .branchTrue, .branch => true,
        else => false,
    };
}

/// Check if an operation is a conditional branch (has two exit paths)
fn isConditionalBranch(op: tf) bool {
    return op == .branchFalse or op == .branchTrue;
}

/// Check if an operation is an unconditional branch (has one exit to target)
fn isUnconditionalBranch(op: tf) bool {
    return op == .branch;
}

/// JIT-compiled method that copies and patches threaded function templates.
/// branch_targets[i] = target op index for branch ops, 0 for non-branch ops
pub fn JitMethod(comptime ops: []const tf, comptime branch_targets: []const usize) type {
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
                    const op = ops[i];
                    const info = &template_infos[i];
                    const curr_offset = offsets[i];

                    if (isConditionalBranch(op)) {
                        // Conditional branch: two exits
                        // br_offsets[0] = taken path (when condition is true)
                        // br_offsets[1] = not-taken path (fall through)
                        const target_op_idx = branch_targets[i];
                        const taken_target = offsets[target_op_idx];
                        const not_taken_target = offsets[i + 1];

                        if (info.br_count >= 1) {
                            jit.patchBranch(curr_offset, info.br_offsets[0], taken_target);
                            patched_branches += 1;
                        }
                        if (info.br_count >= 2) {
                            jit.patchBranch(curr_offset, info.br_offsets[1], not_taken_target);
                            patched_branches += 1;
                        }
                    } else if (isUnconditionalBranch(op)) {
                        // Unconditional branch: one exit to target
                        const target_op_idx = branch_targets[i];
                        const target_offset = offsets[target_op_idx];

                        if (info.last_branch_offset) |br_offset| {
                            jit.patchBranch(curr_offset, br_offset, target_offset);
                            patched_branches += 1;
                        }
                    } else if (isSupportedOp(op)) {
                        // Regular op: patch to next instruction
                        if (i + 1 < num_ops and !isSupportedOp(ops[i + 1])) {
                            continue;
                        }
                        const next_offset = offsets[i + 1];
                        if (info.last_branch_offset) |br_offset| {
                            jit.patchBranch(curr_offset, br_offset, next_offset);
                            patched_branches += 1;
                        }
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
