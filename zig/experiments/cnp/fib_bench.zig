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

const JitMethod = @import("jit_method.zig").JitMethod;
const harness = @import("test_harness.zig");
const opsInfo = harness.opsInfo;

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

const fib_n = 30;
const fib_rounds = 5;

pub fn main() !void {
    var method = try FibSetup.Method.init();
    defer method.deinit();

    var compiled_jit align(64) = compileMethod(Sym.fibonacci, 0, .SmallInteger, FibSetup.tup);

    var one_j: Object.StaticObject = undefined;
    var two_j: Object.StaticObject = undefined;

    compiled_jit.resolve(&[_]Object{ one_j.init(1), two_j.init(2) }) catch @panic("resolve jit");

    for (FibSetup.info.positions, 0..) |pos, i| {
        method.patchOp(compiled_jit.code[0..], i, pos);
    }

    compiled_jit.initExecute();

    const jit_fn = method.getEntryFor(0);
    const jit_pc = PC.init(&compiled_jit.code[0]).next();

    var process_j: Process align(Process.alignment) = undefined;
    process_j.init();
    const tmp_sp = process_j.endOfStack();
    const receiver_j = Object.from(@as(i56, @intCast(fib_n)), tmp_sp, process_j.getContext());

    var jit_samples: [fib_rounds]u64 = undefined;
    var nat_samples: [fib_rounds]u64 = undefined;

    for (0..fib_rounds) |r| {
        jit_samples[r] = measureDirect(jit_fn, jit_pc, receiver_j, &process_j, &compiled_jit);
        nat_samples[r] = measureNativeFib(fib_n);
    }

    print("for '{} fibonacci'\n", .{fib_n});
    print("          Median   Mean   StdDev  SD/Mean ({} runs)\n", .{fib_rounds});
    printRow("Native", &nat_samples);
    printRow("IntegerCnP", &jit_samples);
}

fn printRow(row_name: []const u8, samples_ns: *[fib_rounds]u64) void {
    const med_ns = median(samples_ns);
    const mean_ns = mean(samples_ns);
    const sd_ns = stdDev(samples_ns, mean_ns);

    const med_ms = @as(f64, @floatFromInt(med_ns)) / 1_000_000.0;
    const mean_ms = @as(f64, @floatFromInt(mean_ns)) / 1_000_000.0;
    const sd_ms = @as(f64, @floatFromInt(sd_ns)) / 1_000_000.0;

    print("{s:>9}", .{row_name});
    print("{d:5.0}ms {d:5.0}ms {d:6.2}ms", .{ med_ms, mean_ms, sd_ms });
    if (mean_ns > 0) {
        const pct = sd_ms / mean_ms * 100.0;
        print(" {d:5.1}%", .{pct});
    }
    print("\n", .{});
}

fn measureDirect(
    entry: anytype,
    pc: PC,
    receiver: Object,
    process: *align(Process.alignment) Process,
    compiled: anytype,
) u64 {
    dispatch.addMethod(compiled.asCompiledMethodPtr());
    const sp = process.endOfStack().safeReserve(1);
    sp.top = receiver;
    const ctx = process.getContext();
    const extra = Extra.forMethod(compiled.asCompiledMethodPtr(), sp);
    var timer = std.time.Timer.start() catch unreachable;
    _ = @call(.never_inline, entry, .{ pc, sp, process, ctx, extra });
    return timer.read();
}

fn measureNativeFib(n: i64) u64 {
    var timer = std.time.Timer.start() catch unreachable;
    const r = nativeFib(n);
    std.mem.doNotOptimizeAway(r);
    return timer.read();
}

fn mean(samples: []const u64) u64 {
    var sum: u128 = 0;
    for (samples) |s| sum += s;
    return @intCast(sum / samples.len);
}

fn median(samples: []u64) u64 {
    std.mem.sort(u64, samples, {}, comptime std.sort.asc(u64));
    return samples[samples.len / 2];
}

fn stdDev(samples: []const u64, mean_val: u64) u64 {
    var sum_sq: u128 = 0;
    for (samples) |s| {
        const diff: i128 = @as(i128, @intCast(s)) - @as(i128, @intCast(mean_val));
        sum_sq += @intCast(diff * diff);
    }
    const variance = sum_sq / samples.len;
    return @intCast(std.math.sqrt(variance));
}
