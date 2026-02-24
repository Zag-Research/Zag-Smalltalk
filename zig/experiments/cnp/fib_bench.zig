const std = @import("std");
const zag = @import("zag");
const threadedFn = zag.threadedFn;
const tf = threadedFn.Enum;
const Object = zag.Object;
const PC = zag.execute.PC;
const MainExecutor = zag.execute.Execution.MainExecutor;
const compileMethod = zag.execute.compileMethod;
const Context = zag.Context;
const symbol = zag.symbol;
const Sym = zag.symbol.symbols;
const dispatch = zag.dispatch;

const jitmethod = @import("jit_method.zig");
const JitMethod = jitmethod.JitMethod;
const opsInfo = jitmethod.opsInfo;
const harness = @import("test_harness.zig");
const print = std.debug.print;

const fib_n = 40;
const fib_rounds = 5;
const warmup_rounds = 3;

noinline fn nativeFib(n: i64) i64 {
    if (n <= 2) return n;
    return nativeFib(n - 1) + nativeFib(n - 2);
}

fn measureNativeFib(n: i64) u64 {
    var timer = std.time.Timer.start() catch unreachable;
    const r = nativeFib(n);
    std.mem.doNotOptimizeAway(r);
    return timer.read();
}

const FibSetup = struct {
    const self = Context.makeVariable(0, 1, .Parameter, &.{});
    const nullMethod = dispatch.nullMethod;
    const sig = symbol.signature;

    const tup = .{
        tf.push,             self,
        tf.pushLiteral,      "1const",
        tf.@"inline<=I",     tf.fail, tf.fail,
        tf.branchFalse,      "false",
        tf.returnSelf,       ":false",
        tf.push,             self,
        tf.pushLiteral,      "0const",
        tf.@"inline-I",      tf.fail, tf.fail,
        tf.send,             sig(.fibonacci, 0),
        &nullMethod,         tf.push,
        self,                tf.pushLiteral,
        "1const",
        tf.@"inline-I",      tf.fail, tf.fail,
        tf.send,             sig(.fibonacci, 0),
        &nullMethod,
        tf.@"inline+I",      tf.fail, tf.fail,
        tf.returnTop,
    };
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops, &info.branch_targets);
};

fn measureSendTo(exe: *MainExecutor, compiled: anytype) u64 {
    var timer = std.time.Timer.start() catch unreachable;
    _ = @call(.never_inline, MainExecutor.sendTo, .{ exe, Sym.fibonacci.asObject(), exe.object(@as(i56, fib_n)) }) catch unreachable;
    _ = compiled; // keep compiled alive
    return timer.read();
}

pub fn main() !void {
    var method = try FibSetup.Method.init();
    defer method.deinit();

    var compiled_jit align(64) = compileMethod(Sym.fibonacci, 0, .SmallInteger, FibSetup.tup);
    var one_j: Object.StaticObject = undefined;
    var two_j: Object.StaticObject = undefined;
    compiled_jit.resolve(&[_]Object{ one_j.init(1), two_j.init(2) }) catch @panic("resolve jit");
    for (FibSetup.info.positions, 0..) |pos, i| method.patchOp(compiled_jit.code[0..], i, pos);

    compiled_jit.executeFn = method.getEntryFor(0);
    dispatch.addMethod(compiled_jit.asCompiledMethodPtr());

    var exe = MainExecutor.new();

    print("for '{} fibonacci'\n", .{fib_n});
    print("          Median   Mean   StdDev  SD/Mean ({} run{s}, {} warmup{s})\n", .{
        fib_rounds,    if (fib_rounds != 1) "s" else "",
        warmup_rounds, if (warmup_rounds != 1) "s" else "",
    });
    
    for (0..warmup_rounds) |_| {
        _ = measureSendTo(&exe, &compiled_jit);
        _ = measureNativeFib(fib_n);
    }

    var jit_samples: [fib_rounds]u64 = undefined;
    var nat_samples: [fib_rounds]u64 = undefined;

    for (0..fib_rounds) |r| {
        jit_samples[r] = measureSendTo(&exe, &compiled_jit);
        nat_samples[r] = measureNativeFib(fib_n);
    }

    harness.printRow(fib_rounds, "Native", &nat_samples);
    harness.printRow(fib_rounds, "IntegerCnP", &jit_samples);
}
