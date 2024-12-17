const std = @import("std");
const debug = std.debug;
const math = std.math;
const tailCall: std.builtin.CallModifier = .always_tail;
const stdout = std.io.getStdOut().writer();

pub fn timing(runs: isize) !void {
    const ts = std.time.nanoTimestamp;
    const warmup = 1_000_000;
    try stdout.print("for {} runs\n", .{runs});
    if (runs > warmup) _ = R.run(warmup);
    var start = ts();
    _ = R.run(runs);
    var base = ts() - start;
    try stdout.print("R: {d:8.3}s {d:8.3}ns\n", .{ @as(f64, @floatFromInt(base)) / 1000000000, @as(f64, @floatFromInt(base)) / @as(f64, @floatFromInt(runs)) });
    if (runs > warmup) _ = Q.run(warmup);
    start = ts();
    _ = Q.run(runs);
    var time = ts() - start;
    try stdout.print("Q: {d:8.3}s {d:8.3}ns +{d:6.2}%\n", .{ @as(f64, @floatFromInt(time)) / 1000000000, @as(f64, @floatFromInt(time)) / @as(f64, @floatFromInt(runs)), @as(f64, @floatFromInt(time - base)) * 100.0 / @as(f64, @floatFromInt(base)) });
    if (runs > warmup) _ = P.run(warmup);
    start = ts();
    _ = P.run(runs);
    time = ts() - start;
    try stdout.print("P: {d:8.3}s {d:8.3}ns +{d:6.2}%\n", .{ @as(f64, @floatFromInt(time)) / 1000000000, @as(f64, @floatFromInt(time)) / @as(f64, @floatFromInt(runs)), @as(f64, @floatFromInt(time - base)) * 100.0 / @as(f64, @floatFromInt(base)) });
    if (runs > warmup) _ = N.run(warmup);
    start = ts();
    _ = N.run(runs);
    time = ts() - start;
    try stdout.print("N: {d:8.3}s {d:8.3}ns  {d:6.2}%\n", .{ @as(f64, @floatFromInt(time)) / 1000000000, @as(f64, @floatFromInt(time)) / @as(f64, @floatFromInt(runs)), @as(f64, @floatFromInt(time)) * 100.0 / @as(f64, @floatFromInt(base)) });
}
pub fn main() !void {
    try timing(4);
}
