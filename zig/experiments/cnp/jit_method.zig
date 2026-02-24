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

pub fn isSupportedOp(op: tf) bool {
    return switch (op) {
        .push, .pushLiteral, .dup, .drop, .returnSelf, .returnTop => true,
        .branchFalse, .branchTrue, .branch => true,
        .@"inline<=I", .@"inline+I", .@"inline-I", .@"inline*I", .@"inline<=F", .@"inline+F", .@"inline-F" => true,
        .fail => true,
        .send, .send0 => true,
        else => false,
    };
}

fn isConditionalBranch(op: tf) bool {
    return op == .branchFalse or op == .branchTrue;
}

fn isUnconditionalBranch(op: tf) bool {
    return op == .branch;
}

fn isSendOp(op: tf) bool {
    return op == .send or op == .send0;
}

fn isReturnOp(op: tf) bool {
    return op == .returnSelf or op == .returnTop;
}

fn isInlinePrimOp(op: tf) bool {
    return switch (op) {
        .@"inline<=I", .@"inline+I", .@"inline-I", .@"inline*I", .@"inline<=F", .@"inline+F", .@"inline-F" => true,
        else => false,
    };
}

fn isFailOp(op: tf) bool {
    return op == .fail;
}

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

            for (0..num_ops) |i| {
                const op = ops[i];
                const info = &template_infos[i];
                const curr_offset = offsets[i];

                if (isConditionalBranch(op)) {
                    const target_op_idx = branch_targets[i];
                    const taken_target = offsets[target_op_idx];
                    const not_taken_target = offsets[i + 1];

                    if (info.br_count >= 2) {
                        jit.patchBranch(curr_offset, info.br_offsets[0], taken_target);
                        jit.patchBranch(curr_offset, info.br_offsets[1], not_taken_target);
                        patched_branches += 2;
                    }
                } else if (isUnconditionalBranch(op)) {
                    const target_op_idx = branch_targets[i];
                    const target_offset = offsets[target_op_idx];

                    if (info.last_branch_offset) |br_offset| {
                        jit.patchBranch(curr_offset, br_offset, target_offset);
                        patched_branches += 1;
                    }
                } else if (isSendOp(op) or isReturnOp(op) or isFailOp(op)) {
                    continue;
                } else if (isInlinePrimOp(op)) {
                    if (info.br_count >= 2) {
                        jit.patchBranch(curr_offset, info.br_offsets[0], offsets[i + 3]);
                        jit.patchBranch(curr_offset, info.br_offsets[1], offsets[i + 1]);
                        patched_branches += 2;
                    }
                } else if (isSupportedOp(op)) {
                    const next_offset = offsets[i + 1];
                    if (info.last_branch_offset) |br_offset| {
                        jit.patchBranch(curr_offset, br_offset, next_offset);
                        patched_branches += 1;
                    }
                }
            }

            jit.makeExecutable();

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

        /// Returns true if the given function pointer lies within this JIT buffer.
        /// TODO: Remove once unused
        pub fn inJit(self: *const Self, fn_ptr: anytype) bool {
            const addr = @intFromPtr(fn_ptr);
            const base = @intFromPtr(self.jit.memory.ptr);
            return addr >= base and addr < base + self.jit.memory.len;
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


fn isLabelDef(comptime field: anytype) bool {
    return switch (@typeInfo(@TypeOf(field))) {
        .pointer => |ptr| switch (@typeInfo(ptr.child)) {
            .array => field[0] == ':',
            else => false,
        },
        else => false,
    };
}

fn isLabelRef(comptime field: anytype) bool {
    return switch (@typeInfo(@TypeOf(field))) {
        .pointer => |ptr| switch (@typeInfo(ptr.child)) {
            .array => field[0] != ':' and field[0] != '0' and field[0] != '1' and field[0] != '2',
            else => false,
        },
        else => false,
    };
}

fn isBranchOp(op: tf) bool {
    return op == .branchFalse or op == .branchTrue or op == .branch;
}

pub fn opsInfo(comptime tup: anytype) type {
    comptime var count: usize = 0;
    inline for (tup) |field| {
        if (!isLabelDef(field) and @TypeOf(field) == tf) count += 1;
    }

    return struct {
        pub const ops: [count]tf = blk: {
            var arr: [count]tf = undefined;
            var i: usize = 0;
            for (tup) |field| {
                if (!isLabelDef(field) and @TypeOf(field) == tf) {
                    arr[i] = field;
                    i += 1;
                }
            }
            break :blk arr;
        };

        pub const positions: [count]usize = blk: {
            var arr: [count]usize = undefined;
            var i: usize = 0;
            var pos: usize = 0;
            for (tup) |field| {
                if (isLabelDef(field)) continue;
                if (@TypeOf(field) == tf) {
                    arr[i] = pos;
                    i += 1;
                }
                pos += 1;
            }
            break :blk arr;
        };

        pub const branch_targets: [count]usize = blk: {
            var arr: [count]usize = [_]usize{0} ** count;
            var op_idx: usize = 0;
            const fields = std.meta.fields(@TypeOf(tup));

            for (fields, 0..) |fld, tup_idx| {
                const field = @field(tup, fld.name);
                if (isLabelDef(field)) continue;

                if (@TypeOf(field) == tf) {
                    if (isBranchOp(field)) {
                        if (tup_idx + 1 < fields.len) {
                            const next_fld = fields[tup_idx + 1];
                            const next_field = @field(tup, next_fld.name);
                            if (isLabelRef(next_field)) {
                                arr[op_idx] = findLabelTarget(tup, next_field);
                            }
                        }
                    }
                    op_idx += 1;
                }
            }
            break :blk arr;
        };

        fn findLabelTarget(comptime tuple: anytype, comptime label_ref: []const u8) usize {
            var target_op_idx: usize = 0;
            for (tuple) |field| {
                if (isLabelDef(field)) {
                    const label_def: []const u8 = field;
                    if (label_def.len == label_ref.len + 1 and
                        std.mem.eql(u8, label_def[1..], label_ref))
                    {
                        return target_op_idx;
                    }
                } else if (@TypeOf(field) == tf) {
                    target_op_idx += 1;
                }
            }
            @compileError("Branch target label not found: " ++ label_ref);
        }
    };
}
