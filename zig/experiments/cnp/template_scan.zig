const std = @import("std");
const Arm64 = @import("arm64.zig").Arm64;

pub const ScanResult = struct {
    size: usize,
    br_offsets: [8]usize,
    br_count: usize,
    last_branch_offset: ?usize,
};

pub const ScanError = error{
    NoCode,
    TooLarge,
};

const MaxScanBytes = 32 * 1024;
const MaxInst = MaxScanBytes / 4;

pub fn scan(start: [*]const u8, max_bytes: usize) ScanError!ScanResult {
    if (max_bytes < 4) return error.NoCode;
    if (max_bytes > MaxScanBytes) return error.TooLarge;

    const inst_count = max_bytes / 4;
    var queue: [MaxInst]u32 = undefined;
    var q_head: usize = 0;
    var q_tail: usize = 0;
    var block_seen: [MaxInst]bool = [_]bool{false} ** MaxInst;
    var inst_seen: [MaxInst]bool = [_]bool{false} ** MaxInst;

    var out = ScanResult{
        .size = 0,
        .br_offsets = undefined,
        .br_count = 0,
        .last_branch_offset = null,
    };

    enqueue(&queue, &q_tail, &block_seen, inst_count, 0);

    while (q_head < q_tail) {
        var off: usize = queue[q_head];
        q_head += 1;

        while (off + 4 <= max_bytes) {
            const idx = off / 4;
            if (idx >= inst_count) break;

            if (inst_seen[idx]) break;
            inst_seen[idx] = true;
            if (off + 4 > out.size) out.size = off + 4;

            const inst = std.mem.readInt(u32, start[off..][0..4], .little);

            if (Arm64.isBranchRegister(inst)) {
                if (out.br_count < out.br_offsets.len) {
                    out.br_offsets[out.br_count] = off;
                    out.br_count += 1;
                }
                out.last_branch_offset = off;
                break;
            }

            if (Arm64.isRet(inst)) break;

            if (Arm64.isBImm(inst)) {
                enqueueRelativeTarget(
                    &queue,
                    &q_tail,
                    &block_seen,
                    inst_count,
                    off,
                    Arm64.decodeBImm(inst),
                );
                break;
            }

            if (Arm64.isBCond(inst)) {
                enqueueRelativeTarget(
                    &queue,
                    &q_tail,
                    &block_seen,
                    inst_count,
                    off,
                    Arm64.decodeBCondImm(inst),
                );
                enqueue(&queue, &q_tail, &block_seen, inst_count, off + 4);
                break;
            }

            if (Arm64.isCbzCbnz(inst)) {
                enqueueRelativeTarget(
                    &queue,
                    &q_tail,
                    &block_seen,
                    inst_count,
                    off,
                    Arm64.decodeCbzCbnzImm(inst),
                );
                enqueue(&queue, &q_tail, &block_seen, inst_count, off + 4);
                break;
            }

            if (Arm64.isTbzTbnz(inst)) {
                enqueueRelativeTarget(
                    &queue,
                    &q_tail,
                    &block_seen,
                    inst_count,
                    off,
                    Arm64.decodeTbzTbnzImm(inst),
                );
                enqueue(&queue, &q_tail, &block_seen, inst_count, off + 4);
                break;
            }

            off += 4;
        }
    }

    if (out.size == 0) return error.NoCode;
    if (out.br_count > 1) {
        std.mem.sort(usize, out.br_offsets[0..out.br_count], {}, comptime std.sort.asc(usize));
    }
    if (out.br_count > 0) {
        out.last_branch_offset = out.br_offsets[out.br_count - 1];
    } else {
        out.last_branch_offset = null;
    }
    return out;
}

fn enqueue(
    queue: *[MaxInst]u32,
    q_tail: *usize,
    seen: *[MaxInst]bool,
    inst_count: usize,
    off: usize,
) void {
    if (off & 0x3 != 0) return;
    const idx = off / 4;
    if (idx >= inst_count) return;
    if (seen[idx]) return;
    seen[idx] = true;
    queue[q_tail.*] = @intCast(off);
    q_tail.* += 1;
}

fn enqueueRelativeTarget(
    queue: *[MaxInst]u32,
    q_tail: *usize,
    seen: *[MaxInst]bool,
    inst_count: usize,
    from_off: usize,
    rel_imm: i64,
) void {
    const target = @as(i64, @intCast(from_off)) + rel_imm;
    if (target < 0) return;
    enqueue(queue, q_tail, seen, inst_count, @intCast(target));
}
