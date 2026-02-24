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
pub const ThreadedFn = *const fn (PC, SP, *Process, *Context, Extra) zag.execute.Result;

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
