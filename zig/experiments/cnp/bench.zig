const std = @import("std");
const zag = @import("zag");
const threadedFn = zag.threadedFn;
const tf = threadedFn.Enum;
const Process = zag.Process;
const Context = zag.Context;
const Extra = Context.Extra;
const Object = zag.Object;
const PC = zag.execute.PC;
const Code = zag.execute.Code;
const compileMethod = zag.execute.compileMethod;
const Sym = zag.symbol.symbols;

const JitMethod = @import("jit_method.zig").JitMethod;
const harness = @import("test_harness.zig");
const opsInfo = harness.opsInfo;
const setLiteral = harness.setLiteral;

const print = std.debug.print;

const rounds = 7;
const iterations: usize = 500_000;
const warmup: usize = 50_000;

pub fn main() !void {
    print("\n" ++ "=" ** 70 ++ "\n", .{});
    print("CNP JIT Scaling Benchmark\n", .{});
    print("=" ** 70 ++ "\n", .{});
    print("Rounds: {}, Iterations: {}, Warmup: {}\n\n", .{ rounds, iterations, warmup });

    print("{s:>6} | {s:>8} | {s:>8} | {s:>7} | {s:>14}\n", .{
        "Ops", "JIT", "Thr", "Ratio", "Range (JIT/Thr)",
    });
    print("-" ** 6 ++ "-+-" ++ "-" ** 8 ++ "-+-" ++ "-" ** 8 ++ "-+-" ++ "-" ** 7 ++ "-+-" ++ "-" ** 14 ++ "\n", .{});

    try bench2();
    try bench10();
    try bench20();
    try bench50();
}

fn bench2() !void {
    const tup = .{ tf.pushLiteral, "0const", tf.returnTop };
    try runBench("2", tup, &[_]usize{1});
}

fn bench10() !void {
    const tup = .{
        tf.pushLiteral, "0const",
        tf.dup,         tf.dup,
        tf.drop,        tf.drop,
        tf.dup,         tf.dup,
        tf.drop,        tf.drop,
        tf.returnTop,
    };
    try runBench("10", tup, &[_]usize{1});
}

fn bench20() !void {
    const tup = .{
        tf.pushLiteral, "0const",
        tf.dup,         tf.dup,
        tf.drop,        tf.drop,
        tf.dup,         tf.dup,
        tf.drop,        tf.drop,
        tf.dup,         tf.dup,
        tf.drop,        tf.drop,
        tf.dup,         tf.dup,
        tf.drop,        tf.drop,
        tf.dup,         tf.dup,
        tf.drop,        tf.returnTop,
    };
    try runBench("20", tup, &[_]usize{1});
}

fn bench50() !void {
    const tup = .{
        tf.pushLiteral, "0const",
        tf.dup,         tf.dup,        tf.drop,        tf.drop,
        tf.dup,         tf.dup,        tf.drop,        tf.drop,
        tf.dup,         tf.dup,        tf.drop,        tf.drop,
        tf.dup,         tf.dup,        tf.drop,        tf.drop,
        tf.dup,         tf.dup,        tf.drop,        tf.drop,
        tf.dup,         tf.dup,        tf.drop,        tf.drop,
        tf.dup,         tf.dup,        tf.drop,        tf.drop,
        tf.dup,         tf.dup,        tf.drop,        tf.drop,
        tf.dup,         tf.dup,        tf.drop,        tf.drop,
        tf.dup,         tf.dup,        tf.drop,        tf.drop,
        tf.dup,         tf.dup,        tf.drop,        tf.drop,
        tf.dup,         tf.dup,        tf.drop,        tf.drop,
        tf.dup,         tf.returnTop,
    };
    try runBench("50", tup, &[_]usize{1});
}

fn runBench(name: []const u8, comptime tup: anytype, comptime lit_pos: []const usize) !void {
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops, &info.branch_targets, &info.prim_fns);

    var method = try Method.init();
    defer method.deinit();

    var process: Process align(Process.alignment) = undefined;
    process.init();

    var compiled_jit align(64) = compileMethod(Sym.value, 0, .Object, tup);
    var compiled_thr align(64) = compileMethod(Sym.value, 0, .Object, tup);

    var lits_jit: [lit_pos.len]Object.StaticObject = undefined;
    var lits_thr: [lit_pos.len]Object.StaticObject = undefined;
    var resolve_jit: [lit_pos.len]Object = undefined;
    var resolve_thr: [lit_pos.len]Object = undefined;

    inline for (0..lit_pos.len) |i| {
        resolve_jit[i] = lits_jit[i].init(0);
        resolve_thr[i] = lits_thr[i].init(0);
    }

    compiled_jit.resolve(&resolve_jit) catch unreachable;
    compiled_thr.resolve(&resolve_thr) catch unreachable;

    const ctx = process.getContext();
    const sp = process.endOfStack();

    inline for (lit_pos) |pos| {
        setLiteral(compiled_jit.code[0..], pos, Object.from(42, sp, ctx));
        setLiteral(compiled_thr.code[0..], pos, Object.from(42, sp, ctx));
    }

    for (info.positions, 0..) |pos, i| method.patchOp(compiled_jit.code[0..], i, pos);
    compiled_thr.initExecute();

    const jit_fn = method.getEntryFor(0);
    const thr_fn = compiled_thr.executeFn;
    const jit_pc = PC.init(&compiled_jit.code[0]).next();
    const thr_pc = PC.init(&compiled_thr.code[0]).next();
    const jit_extra = Extra.forMethod(compiled_jit.asCompiledMethodPtr(), sp);
    const thr_extra = Extra.forMethod(compiled_thr.asCompiledMethodPtr(), sp);

    var jit_samples: [rounds]u64 = undefined;
    var thr_samples: [rounds]u64 = undefined;

    // warmup
    for (0..warmup) |_| {
        _ = @call(.never_inline, jit_fn, .{ jit_pc, sp, &process, ctx, jit_extra });
        _ = @call(.never_inline, thr_fn, .{ thr_pc, sp, &process, ctx, thr_extra });
    }

    // alternating order
    for (0..rounds) |r| {
        if (r % 2 == 0) {
            jit_samples[r] = measure(jit_fn, jit_pc, sp, &process, ctx, jit_extra);
            thr_samples[r] = measure(thr_fn, thr_pc, sp, &process, ctx, thr_extra);
        } else {
            thr_samples[r] = measure(thr_fn, thr_pc, sp, &process, ctx, thr_extra);
            jit_samples[r] = measure(jit_fn, jit_pc, sp, &process, ctx, jit_extra);
        }
    }

    const jit_med = median(&jit_samples);
    const thr_med = median(&thr_samples);
    const jit_mm = minMax(&jit_samples);
    const thr_mm = minMax(&thr_samples);
    const ratio = @as(f64, @floatFromInt(jit_med)) / @as(f64, @floatFromInt(@max(1, thr_med)));

    print("{s:>6} | {d:>5}ns | {d:>5}ns | {d:>6.2}x | {d}-{d}/{d}-{d}\n", .{
        name, jit_med, thr_med, ratio,
        jit_mm.min, jit_mm.max, thr_mm.min, thr_mm.max,
    });
}

fn measure(entry: anytype, pc: PC, sp: Process.SP, process: *Process, ctx: *Context, extra: Extra) u64 {
    var timer = std.time.Timer.start() catch unreachable;
    for (0..iterations) |_| {
        _ = @call(.never_inline, entry, .{ pc, sp, process, ctx, extra });
    }
    return timer.read() / iterations;
}

fn median(samples: []u64) u64 {
    std.mem.sort(u64, samples, {}, std.sort.asc(u64));
    return samples[samples.len / 2];
}

fn minMax(samples: []const u64) struct { min: u64, max: u64 } {
    var min: u64 = std.math.maxInt(u64);
    var max: u64 = 0;
    for (samples) |s| {
        if (s < min) min = s;
        if (s > max) max = s;
    }
    return .{ .min = min, .max = max };
}
