const std = @import("std");
const zag = @import("zag");
const threadedFn = zag.threadedFn;
const tf = threadedFn.Enum;
const Process = zag.Process;
const Context = zag.Context;
const Extra = Context.Extra;
const Object = zag.Object;
const PC = zag.execute.PC;
const compileMethod = zag.execute.compileMethod;
const Sym = zag.symbol.symbols;
const SmallInteger = zag.primitives.primitives.SmallInteger;

const jitmethod = @import("jit_method.zig");
const JitMethod = jitmethod.JitMethod;
const opsInfo = jitmethod.opsInfo;
const harness = @import("test_harness.zig");

const print = std.debug.print;

const rounds = 11;
const warmup_rounds = 3;
const iterations: usize = 10_000;

const Mode = enum { jit, threaded };

fn parseMode(args: []const []const u8) Mode {
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--mode=threaded")) return .threaded;
    }
    return .jit;
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    const mode = parseMode(args[1..]);

    print("Scaling Benchmark:\n", .{});
    print("Mode:{s} Rounds: {} ({} warmup), {} iterations/round\n\n", .{ @tagName(mode), rounds - warmup_rounds, warmup_rounds, iterations });

    print("{s:>6} | {s:>8} | {s:>14}\n", .{ "Ops", "ns/iter", "Range" });
    print("-" ** 6 ++ "-+-" ++ "-" ** 8 ++ "-+-" ++ "-" ** 14 ++ "\n", .{});

    try benchLoop(mode);
    try benchArith(mode);
    try bench2(mode);
    try bench10(mode);
    try bench20(mode);
    try bench50(mode);
}

fn bench2(mode: Mode) !void {
    const tup = .{ tf.pushLiteral, "0const", tf.returnTop };
    try runBench("2", tup, &[_]usize{1}, mode);
}

fn bench10(mode: Mode) !void {
    const tup = .{
        tf.pushLiteral, "0const",
        tf.dup,         tf.dup,
        tf.drop,        tf.drop,
        tf.dup,         tf.dup,
        tf.drop,        tf.drop,
        tf.returnTop,
    };
    try runBench("10", tup, &[_]usize{1}, mode);
}

fn bench20(mode: Mode) !void {
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
    try runBench("20", tup, &[_]usize{1}, mode);
}

fn bench50(mode: Mode) !void {
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
        tf.drop,        tf.drop,
        tf.dup,         tf.dup,
        tf.drop,        tf.drop,
        tf.dup,         tf.dup,
        tf.drop,        tf.drop,
        tf.dup,         tf.dup,
        tf.drop,        tf.drop,
        tf.dup,         tf.dup,
        tf.drop,        tf.drop,
        tf.dup,         tf.dup,
        tf.drop,        tf.drop,
        tf.dup,         tf.dup,
        tf.drop,        tf.drop,
        tf.dup,         tf.dup,
        tf.drop,        tf.drop,
        tf.dup,         tf.returnTop,
    };
    try runBench("50", tup, &[_]usize{1}, mode);
}

fn benchArith(mode: Mode) !void {
    const tup = .{
        tf.pushLiteral, "0const", // base = 0
        tf.pushLiteral, "1const", tf.@"inline+I", tf.fail,tf.fail, // + 1
        tf.pushLiteral, "2const", tf.@"inline+I", tf.fail,tf.fail, // + 2
        tf.pushLiteral, "3const", tf.@"inline+I", tf.fail,tf.fail, // + 3
        tf.pushLiteral, "4const", tf.@"inline+I", tf.fail,tf.fail, // + 4
        tf.pushLiteral, "5const", tf.@"inline+I", tf.fail,tf.fail, // + 5
        tf.pushLiteral, "6const", tf.@"inline+I", tf.fail,tf.fail, // + 6
        tf.pushLiteral, "7const", tf.@"inline+I", tf.fail,tf.fail, // + 7
        tf.pushLiteral, "8const", tf.@"inline+I", tf.fail,tf.fail, // + 8
        tf.pushLiteral, "9const", tf.@"inline+I", tf.fail,tf.fail, // + 9
        tf.returnTop,
    };
    try runBench("arith", tup, &[_]usize{ 0, 1, 2, 3, 4, 5, 6, 7, 8, 9 }, mode);
}

fn benchLoop(mode: Mode) !void {
    const tup = .{
        tf.pushLiteral,  "0const",
        ":loop",         tf.dup,
        tf.pushLiteral,  "1const",
        tf.@"inline<=I", tf.fail,tf.fail,
        tf.branchFalse,  "cont",
        tf.returnTop,    ":cont",
        tf.pushLiteral,  "2const",
        tf.@"inline-I",  tf.fail,tf.fail,
        tf.branch,       "loop",
    };
    try runBench("loop", tup, &[_]usize{ 50000, 0, 1 }, mode);
}

fn runBench(name: []const u8, comptime tup: anytype, comptime lit_pos: []const usize, mode: Mode) !void {
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops, &info.branch_targets);

    var method = try Method.init();
    defer method.deinit();

    var process: Process align(Process.alignment) = undefined;
    process.init();

    var compiled align(64) = compileMethod(Sym.value, 0, .Object, tup);

    var lits: [lit_pos.len]Object.StaticObject = undefined;
    var resolve: [lit_pos.len]Object = undefined;
    inline for (0..lit_pos.len) |i| resolve[i] = lits[i].init(lit_pos[i]);
    compiled.resolve(&resolve) catch unreachable;

    const ctx = process.getContext();
    const sp = process.endOfStack();

    var samples: [rounds]u64 = undefined;

    switch (mode) {
        .jit => {
            for (info.positions, 0..) |pos, i| method.patchOp(compiled.code[0..], i, pos);
            compiled.initExecute();
            const fn_ = method.getEntryFor(0);
            const pc = PC.init(&compiled.code[0]).next();
            const extra = Extra.forMethod(compiled.asCompiledMethodPtr(), sp);
            for (0..warmup_rounds) |_|
                _ = harness.measureIter(iterations, fn_, pc, sp, &process, ctx, extra);
            for (0..rounds) |r|
                samples[r] = harness.measureIter(iterations, fn_, pc, sp, &process, ctx, extra);
        },
        .threaded => {
            compiled.initExecute();
            const fn_ = compiled.executeFn;
            const pc = PC.init(&compiled.code[0]).next();
            const extra = Extra.forMethod(compiled.asCompiledMethodPtr(), sp);
            for (0..warmup_rounds) |_|
                _ = harness.measureIter(iterations, fn_, pc, sp, &process, ctx, extra);
            for (0..rounds) |r|
                samples[r] = harness.measureIter(iterations, fn_, pc, sp, &process, ctx, extra);
        },
    }

    const med = harness.median(&samples);
    const mm = harness.minMax(&samples);
    print("{s:>6} | {d:>5}ns | {d}-{d}\n", .{ name, med, mm.min, mm.max });
}
