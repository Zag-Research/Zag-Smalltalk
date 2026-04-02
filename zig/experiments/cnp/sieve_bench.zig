const std = @import("std");
const zag = @import("zag");
const threadedFn = zag.threadedFn;
const tf = threadedFn.Enum;
const Object = zag.Object;
const MainExecutor = zag.execute.Execution.MainExecutor;
const compileMethod = zag.execute.compileMethod;
const Context = zag.Context;
const symbol = zag.symbol;
const Sym = zag.symbol.Symbols;
const dispatch = zag.dispatch;

const jitmethod = @import("jit_method.zig");
const JitMethod = jitmethod.JitMethod;
const opsInfo = jitmethod.opsInfo;
const harness = @import("test_harness.zig");
const print = std.debug.print;

const start_n: i64 = 19;
const step_iterations: usize = 100_000;
const rounds = 5;
const warmup_rounds = 3;

const Mode = enum { jit, threaded };

fn parseMode(args: []const []const u8) Mode {
    for (args) |arg| {
        if (std.mem.eql(u8, arg, "--mode=threaded")) return .threaded;
    }
    return .jit;
}

fn nativeCollatzStep(n: i64) i64 {
    if (n <= 1) return 1;
    if (@mod(n, 2) == 0) return @divTrunc(n, 2);
    return n * 3 + 1;
}

fn measureNative() u64 {
    var timer = std.time.Timer.start() catch unreachable;
    var n = start_n;
    for (0..step_iterations) |_| {
        n = nativeCollatzStep(n);
        if (n <= 1) n = start_n;
    }
    std.mem.doNotOptimizeAway(n);
    return timer.read();
}

const CollatzSetup = struct {
    const self = Context.makeVariable(0, 1, .Parameter, &.{});
    const nullMethod = dispatch.nullMethod;
    const sig = symbol.signature;

    // returns 1 if even, else 0
    const is_even_tup = .{
        tf.push,             self,
        tf.pushLiteral,      "1const",
        tf.@"inline<=I",    tf.fail, tf.fail,
        tf.branchFalse,      "gt1",
        tf.pushLiteral,      "0const",
        tf.returnTop,        ":gt1",
        tf.push,             self,
        tf.pushLiteral,      "2const",
        tf.@"inline<=I",    tf.fail, tf.fail,
        tf.branchFalse,      "recur",
        tf.pushLiteral,      "1const",
        tf.returnTop,        ":recur",
        tf.push,             self,
        tf.pushLiteral,      "2const",
        tf.@"inline-I",     tf.fail, tf.fail,
        tf.send,             sig(.Behavior, 0),
        &nullMethod,
        tf.returnTop,
    };

    // integer half via repeated subtraction: half(n)
    const half_tup = .{
        tf.push,             self,
        tf.pushLiteral,      "1const",
        tf.@"inline<=I",    tf.fail, tf.fail,
        tf.branchFalse,      "recur",
        tf.pushLiteral,      "0const",
        tf.returnTop,        ":recur",
        tf.push,             self,
        tf.pushLiteral,      "2const",
        tf.@"inline-I",     tf.fail, tf.fail,
        tf.send,             sig(.ClassDescription, 0),
        &nullMethod,
        tf.pushLiteral,      "1const",
        tf.@"inline+I",     tf.fail, tf.fail,
        tf.returnTop,
    };

    // collatz step for SmallInteger receiver
    // if n <= 1 -> self
    // if odd -> 3n + 1
    // if even -> half(n)
    const collatz_tup = .{
        tf.push,             self,
        tf.pushLiteral,      "1const",
        tf.@"inline<=I",    tf.fail, tf.fail,
        tf.branchFalse,      "gt1",
        tf.returnSelf,       ":gt1",
        tf.push,             self,
        tf.send,             sig(.Behavior, 0),
        &nullMethod,
        tf.pushLiteral,      "0const",
        tf.@"inline<=I",    tf.fail, tf.fail,
        tf.branchFalse,      "even",
        tf.push,             self,
        tf.pushLiteral,      "3const",
        tf.@"inline*I",     tf.fail, tf.fail,
        tf.pushLiteral,      "1const",
        tf.@"inline+I",     tf.fail, tf.fail,
        tf.returnTop,        ":even",
        tf.push,             self,
        tf.send,             sig(.ClassDescription, 0),
        &nullMethod,
        tf.returnTop,
    };

    const even_info = opsInfo(is_even_tup);
    const half_info = opsInfo(half_tup);
    const collatz_info = opsInfo(collatz_tup);

    const EvenMethod = JitMethod(&even_info.ops, &even_info.branch_targets);
    const HalfMethod = JitMethod(&half_info.ops, &half_info.branch_targets);
    const CollatzMethod = JitMethod(&collatz_info.ops, &collatz_info.branch_targets);
};

fn patchAndSetEntry(comptime info: type, method: anytype, compiled: anytype) void {
    for (info.positions, 0..) |pos, i| method.patchOp(compiled.code[0..], i, pos);
    compiled.executeFn = method.getEntryFor(0);
}

fn measureCollatzMode(exe: *MainExecutor, keep_alive: anytype) u64 {
    var timer = std.time.Timer.start() catch unreachable;
    var n: i64 = start_n;

    for (0..step_iterations) |_| {
        const result_obj = @call(.never_inline, MainExecutor.sendTo, .{
            exe,
            Sym.fibonacci.asObject(),
            exe.object(@as(i56, @intCast(n))),
        }) catch unreachable;

        if (result_obj.nativeI()) |next_n| {
            n = next_n;
        } else {
            @panic("collatz result not SmallInteger");
        }
        if (n <= 1) n = start_n;
    }

    _ = keep_alive;
    std.mem.doNotOptimizeAway(n);
    return timer.read();
}

pub fn main() !void {
    const allocator = std.heap.page_allocator;
    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);
    const mode = parseMode(args[1..]);

    var even_method = try CollatzSetup.EvenMethod.init();
    defer even_method.deinit();
    var half_method = try CollatzSetup.HalfMethod.init();
    defer half_method.deinit();
    var collatz_method = try CollatzSetup.CollatzMethod.init();
    defer collatz_method.deinit();

    var compiled_even align(64) = compileMethod(Sym.Behavior, 0, .SmallInteger, CollatzSetup.is_even_tup);
    var compiled_half align(64) = compileMethod(Sym.ClassDescription, 0, .SmallInteger, CollatzSetup.half_tup);
    var compiled_collatz align(64) = compileMethod(Sym.fibonacci, 0, .SmallInteger, CollatzSetup.collatz_tup);

    var lit0: Object.StaticObject = undefined;
    var lit1: Object.StaticObject = undefined;
    var lit2: Object.StaticObject = undefined;
    var lit3: Object.StaticObject = undefined;

    const l0 = lit0.init(0);
    const l1 = lit1.init(1);
    const l2 = lit2.init(2);
    const l3 = lit3.init(3);

    compiled_even.resolve(&[_]Object{ l0, l1, l2 }) catch @panic("resolve even");
    compiled_half.resolve(&[_]Object{ l0, l1, l2 }) catch @panic("resolve half");
    compiled_collatz.resolve(&[_]Object{ l0, l1, l2, l3 }) catch @panic("resolve collatz");

    switch (mode) {
        .jit => {
            patchAndSetEntry(CollatzSetup.even_info, &even_method, &compiled_even);
            patchAndSetEntry(CollatzSetup.half_info, &half_method, &compiled_half);
            patchAndSetEntry(CollatzSetup.collatz_info, &collatz_method, &compiled_collatz);
        },
        .threaded => {
            compiled_even.initExecute();
            compiled_half.initExecute();
            compiled_collatz.initExecute();
        },
    }

    dispatch.addMethod(compiled_even.asCompiledMethodPtr());
    dispatch.addMethod(compiled_half.asCompiledMethodPtr());
    dispatch.addMethod(compiled_collatz.asCompiledMethodPtr());

    var exe = MainExecutor.new();

    print("for '{}' collatz steps, {} iterations (mode={s})\n", .{ start_n, step_iterations, @tagName(mode) });
    print("          Median   Mean   StdDev  SD/Mean ({} run{s}, {} warmup{s})\n", .{
        rounds,        if (rounds != 1) "s" else "",
        warmup_rounds, if (warmup_rounds != 1) "s" else "",
    });

    for (0..warmup_rounds) |_| {
        _ = measureCollatzMode(&exe, &compiled_collatz);
        _ = measureNative();
    }

    var jit_samples: [rounds]u64 = undefined;
    var nat_samples: [rounds]u64 = undefined;

    for (0..rounds) |r| {
        jit_samples[r] = measureCollatzMode(&exe, &compiled_collatz);
        nat_samples[r] = measureNative();
    }

    harness.printRow(rounds, "Native", &nat_samples);
    harness.printRow(rounds, "IntegerCnP", &jit_samples);
}
