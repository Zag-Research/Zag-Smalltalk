const std = @import("std");
const builtin = @import("builtin");
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
const dispatch = zag.dispatch;
const Sym = zag.symbol.symbols;
const SmallInteger = zag.primitives.primitives.SmallInteger;
const symbol = zag.symbol;
const MainExecutor = zag.execute.Execution.MainExecutor;

const JitMethod = @import("jit_method.zig").JitMethod;
const harness = @import("test_harness.zig");
const opsInfo = harness.opsInfo;
const setLiteral = harness.setLiteral;

const print = std.debug.print;

noinline fn nativeFib(n: i64) i64 {
    if (n <= 2) return n;
    return nativeFib(n - 1) + nativeFib(n - 2);
}

const FibSetup = struct {
    const self = Context.makeVariable(0, 1, .Parameter, &.{});
    const leq = SmallInteger.@"<=".inlined;
    const plus = SmallInteger.@"+".inlined;
    const minus = SmallInteger.@"-".inlined;
    const nullMethod = dispatch.nullMethod;
    const sig = symbol.signature;

    const tup = .{
        tf.push,            self,
        tf.pushLiteral,     "1const",
        tf.inlinePrimitive, leq,
        tf.branchFalse,     "false",
        tf.returnSelf,      ":false",
        tf.push,            self,
        tf.pushLiteral,     "0const",
        tf.inlinePrimitive, minus,
        tf.send,            sig(.fibonacci, 0),
        &nullMethod,        tf.push,
        self,               tf.pushLiteral,
        "1const",           tf.inlinePrimitive,
        minus,              tf.send,
        sig(.fibonacci, 0), &nullMethod,
        tf.inlinePrimitive, plus,
        tf.returnTop,
    };
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops, &info.branch_targets, &info.prim_fns);
};

const fib_rounds = 7;

const FibCase = struct { n: i64, iters: usize, warmup_iters: usize };
const fib_cases = [_]FibCase{
    .{ .n = 1, .iters = 500_000, .warmup_iters = 50_000 },
    .{ .n = 2, .iters = 500_000, .warmup_iters = 50_000 },
    .{ .n = 5, .iters = 200_000, .warmup_iters = 20_000 },
    .{ .n = 8, .iters = 50_000, .warmup_iters = 5_000 },
    .{ .n = 10, .iters = 10_000, .warmup_iters = 1_000 },
    .{ .n = 15, .iters = 1_000, .warmup_iters = 100 },
    .{ .n = 20, .iters = 100, .warmup_iters = 10 },
    .{ .n = 25, .iters = 10, .warmup_iters = 2 },
};

pub fn main() !void {
    var method = try FibSetup.Method.init();
    defer method.deinit();

    var compiled_jit align(64) = compileMethod(Sym.fibonacci, 0, .SmallInteger, FibSetup.tup);
    var compiled_thr align(64) = compileMethod(Sym.fibonacci, 0, .SmallInteger, FibSetup.tup);

    var one_j: Object.StaticObject = undefined;
    var two_j: Object.StaticObject = undefined;
    var one_t: Object.StaticObject = undefined;
    var two_t: Object.StaticObject = undefined;

    compiled_jit.resolve(&[_]Object{ one_j.init(1), two_j.init(2) }) catch @panic("resolve jit");
    compiled_thr.resolve(&[_]Object{ one_t.init(1), two_t.init(2) }) catch @panic("resolve thr");

    for (FibSetup.info.positions, 0..) |pos, i| {
        if (method.template_infos[i].size == 0) continue;
        method.patchOp(compiled_jit.code[0..], i, pos);
    }

    compiled_jit.initExecute();
    compiled_thr.initExecute();

    print("Fibonacci: CnP JIT vs Threaded vs Native\n", .{});
    print("Mode: {s} - Arch: {s}\n", .{ @tagName(builtin.mode), @tagName(builtin.cpu.arch) });
    print("Rounds: {}\n", .{fib_rounds});

    // Verify correctness
    {
        var exe: MainExecutor = undefined;
        exe = MainExecutor.new();
        dispatch.addMethod(compiled_jit.asCompiledMethodPtr());
        for (fib_cases) |tc| {
            const obj = exe.sendTo(
                Sym.fibonacci.asObject(),
                exe.object(@as(i56, @intCast(tc.n))),
            ) catch return error.TestFailed;
            const got = obj.nativeI() orelse return error.TestFailed;
            const expected = nativeFib(tc.n);
            if (got != expected) {
                print("FAIL: fib({}) = {}, expected {}\n", .{ tc.n, got, expected });
                return error.TestFailed;
            }
        }
    }

    print("{s:>6} | {s:>10} | {s:>10} | {s:>10} | {s:>7}\n", .{
        "fib(n)", "JIT", "Threaded", "Native", "JIT/Thr",
    });
    print("-" ** 6 ++ "-+-" ++ "-" ** 10 ++ "-+-" ++ "-" ** 10 ++ "-+-" ++ "-" ** 10 ++
        "-+-" ++ "-" ** 7 ++ "-+-" ++ "-" ** 14 ++ "\n", .{});

    for (fib_cases) |tc| {
        var jit_samples: [fib_rounds]u64 = undefined;
        var thr_samples: [fib_rounds]u64 = undefined;
        var nat_samples: [fib_rounds]u64 = undefined;

        // warmup
        dispatch.addMethod(compiled_jit.asCompiledMethodPtr());
        {
            var exe_w: MainExecutor = undefined;
            exe_w = MainExecutor.new();
            for (0..tc.warmup_iters) |_| {
                _ = exe_w.sendTo(
                    Sym.fibonacci.asObject(),
                    exe_w.object(@as(i56, @intCast(tc.n))),
                ) catch unreachable;
            }
        }
        // warmup threaded
        dispatch.addMethod(compiled_thr.asCompiledMethodPtr());
        {
            var exe_w: MainExecutor = undefined;
            exe_w = MainExecutor.new();
            for (0..tc.warmup_iters) |_| {
                _ = exe_w.sendTo(
                    Sym.fibonacci.asObject(),
                    exe_w.object(@as(i56, @intCast(tc.n))),
                ) catch unreachable;
            }
        }

        // timed rounds, alternating
        for (0..fib_rounds) |r| {
            if (r % 2 == 0) {
                jit_samples[r] = measureFib(&compiled_jit, tc.n, tc.iters);
                thr_samples[r] = measureFib(&compiled_thr, tc.n, tc.iters);
            } else {
                thr_samples[r] = measureFib(&compiled_thr, tc.n, tc.iters);
                jit_samples[r] = measureFib(&compiled_jit, tc.n, tc.iters);
            }
            nat_samples[r] = measureNativeFib(tc.n, tc.iters);
        }

        const jit_med = median(&jit_samples);
        const thr_med = median(&thr_samples);
        const nat_med = median(&nat_samples);
        const ratio = @as(f64, @floatFromInt(jit_med)) / @as(f64, @floatFromInt(@max(1, thr_med)));

        print("{d:>6} | {s:>10} | {s:>10} | {s:>10} | {d:>6.2}x\n", .{ @as(u64, @intCast(tc.n)), fmtTime(jit_med), fmtTime(thr_med), fmtTime(nat_med), ratio });
    }
    print("\n", .{});
}

fn measureFib(compiled: anytype, n: i64, iters: usize) u64 {
    dispatch.addMethod(compiled.asCompiledMethodPtr());
    var exe: MainExecutor = undefined;
    exe = MainExecutor.new();
    var timer = std.time.Timer.start() catch unreachable;
    for (0..iters) |_| {
        _ = exe.sendTo(
            Sym.fibonacci.asObject(),
            exe.object(@as(i56, @intCast(n))),
        ) catch unreachable;
    }
    return timer.read() / iters;
}

fn measureNativeFib(n: i64, iters: usize) u64 {
    var timer = std.time.Timer.start() catch unreachable;
    for (0..iters) |_| {
        const r = nativeFib(n);
        std.mem.doNotOptimizeAway(r);
    }
    return timer.read() / iters;
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

const FmtBuf = [32]u8;

fn fmtTime(ns: u64) []const u8 {
    const S = struct {
        var bufs: [8]FmtBuf = undefined;
        var idx: usize = 0;
    };
    const slot = S.idx % 8;
    S.idx += 1;
    var buf = &S.bufs[slot];
    buf.* = [_]u8{0} ** 32;
    const len = if (ns < 1_000)
        (std.fmt.bufPrint(buf, "{d}ns", .{ns}) catch buf).len
    else if (ns < 1_000_000)
        (std.fmt.bufPrint(buf, "{d:.1}us", .{@as(f64, @floatFromInt(ns)) / 1_000.0}) catch buf).len
    else
        (std.fmt.bufPrint(buf, "{d:.2}ms", .{@as(f64, @floatFromInt(ns)) / 1_000_000.0}) catch buf).len;
    return buf[0..len];
}
