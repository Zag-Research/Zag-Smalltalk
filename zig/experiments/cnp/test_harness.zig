const std = @import("std");
const zag = @import("zag");
const threadedFn = zag.threadedFn;
const tf = threadedFn.Enum;
const Process = zag.Process;
const Context = zag.Context;
const Extra = Context.Extra;
const PC = zag.execute.PC;
const SP = Process.SP;
const Code = zag.execute.Code;
const Object = zag.Object;
const Signature = zag.execute.Signature;
const SmallInteger = zag.primitives.primitives.SmallInteger;

pub const ThreadedFn = *const fn (PC, SP, *Process, *Context, Extra) zag.execute.Result;

fn findInlinePrimFn(comptime sig: Signature) ?ThreadedFn {
    inline for (.{
        .{ SmallInteger.@"+".inlined, &SmallInteger.@"+".inlinePrimitive },
        .{ SmallInteger.@"-".inlined, &SmallInteger.@"-".inlinePrimitive },
        .{ SmallInteger.@"<=".inlined, &SmallInteger.@"<=".inlinePrimitive },
        .{ SmallInteger.@"*".inlined, &SmallInteger.@"*".inlinePrimitive },
    }) |entry| {
        if (sig.prim == entry[0].prim) return entry[1];
    }
    return null;
}

pub fn initJitTest(method: anytype, process: *align(Process.alignment) Process, title: []const u8) !void {
    const MethodType = @TypeOf(method.*);
    method.* = try MethodType.init();
    process.init();
    std.debug.print("\n--- {s} ---\n", .{title});
    method.dump();
}

pub fn initBenchmark(method: anytype, process: *align(Process.alignment) Process) !void {
    const MethodType = @TypeOf(method.*);
    method.* = try MethodType.init();
    process.init();
}

pub fn runCompiled(
    method: anytype,
    compiled: anytype,
    process: *align(Process.alignment) Process,
    op_positions: []const usize,
    sp_opt: ?SP,
) i64 {
    const context = process.getContext();
    const sp = sp_opt orelse process.endOfStack();
    const code = compiled.code[0..];
    for (op_positions, 0..) |pos, i| {
        method.patchOp(code, i, pos);
    }
    const pc = PC.init(&code[0]).next();
    const extra = Extra.forMethod(compiled.asCompiledMethodPtr(), sp);
    const result_sp = method.getEntryFor(0)(pc, sp, process, context, extra);
    return result_sp.at(0).to(i64);
}

pub fn setLiteral(code: []Code, index: usize, value: Object) void {
    code[index] = Code.objectOf(value);
}

pub fn reportResult(got: i64, expected: i64) !void {
    std.debug.print("Got: {}, Expected: {}\n", .{ got, expected });
    if (got != expected) {
        std.debug.print("FAILED!\n", .{});
        return error.TestFailed;
    }
    std.debug.print("SUCCESS!\n", .{});
}

pub fn runTest(comptime T: type) !void {
    try T.init();
    defer T.deinit();
    try T.run();
}

pub fn mean(samples: []const u64) u64 {
    var sum: u128 = 0;
    for (samples) |s| sum += s;
    return @intCast(sum / samples.len);
}

pub fn median(samples: []u64) u64 {
    std.mem.sort(u64, samples, {}, comptime std.sort.asc(u64));
    return samples[samples.len / 2];
}

pub fn stdDev(samples: []const u64, mean_val: u64) u64 {
    var sum_sq: u128 = 0;
    for (samples) |s| {
        const diff: i128 = @as(i128, @intCast(s)) - @as(i128, @intCast(mean_val));
        sum_sq += @intCast(diff * diff);
    }
    return @intCast(std.math.sqrt(sum_sq / samples.len));
}

pub fn minMax(samples: []const u64) struct { min: u64, max: u64 } {
    var min: u64 = std.math.maxInt(u64);
    var max: u64 = 0;
    for (samples) |s| {
        if (s < min) min = s;
        if (s > max) max = s;
    }
    return .{ .min = min, .max = max };
}

pub fn printRow(comptime N: usize, row_name: []const u8, samples: *[N]u64) void {
    const med_ns = median(samples);
    const mean_ns = mean(samples);
    const sd_ns = stdDev(samples, mean_ns);
    const med_ms = @as(f64, @floatFromInt(med_ns)) / 1_000_000.0;
    const mean_ms = @as(f64, @floatFromInt(mean_ns)) / 1_000_000.0;
    const sd_ms = @as(f64, @floatFromInt(sd_ns)) / 1_000_000.0;
    std.debug.print("{s:>9}{d:5.0}ms {d:5.0}ms {d:6.2}ms", .{ row_name, med_ms, mean_ms, sd_ms });
    std.debug.print(" {d:5.1}%", .{sd_ms / mean_ms * 100.0});
    std.debug.print("\n", .{});
}

pub fn measureIter(
    comptime N: usize,
    entry: anytype,
    pc: PC,
    sp: SP,
    process: *Process,
    ctx: *Context,
    extra: Extra,
) u64 {
    var timer = std.time.Timer.start() catch unreachable;
    for (0..N) |_| {
        _ = @call(.never_inline, entry, .{ pc, sp, process, ctx, extra });
    }
    return timer.read() / N;
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

/// Extracts threaded function ops, their code positions, and branch targets from a tuple.
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

        pub const prim_fns: [count]?ThreadedFn = blk: {
            var arr: [count]?ThreadedFn = [_]?ThreadedFn{null} ** count;
            var op_idx: usize = 0;
            var prev_was_inline_prim = false;
            const fields = std.meta.fields(@TypeOf(tup));

            for (fields) |fld| {
                const field = @field(tup, fld.name);
                if (isLabelDef(field)) continue;

                if (@TypeOf(field) == tf) {
                    prev_was_inline_prim = (field == .inlinePrimitive);
                    op_idx += 1;
                } else if (prev_was_inline_prim and @TypeOf(field) == Signature) {
                    arr[op_idx - 1] = findInlinePrimFn(field);
                    prev_was_inline_prim = false;
                } else {
                    prev_was_inline_prim = false;
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
