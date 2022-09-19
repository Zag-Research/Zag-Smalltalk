const std = @import("std");
const debug = std.debug;
const math = std.math;
const tailCall: std.builtin.CallOptions = .{.modifier = .always_tail};
const noInlineCall: std.builtin.CallOptions = .{.modifier = .never_inline};
const stdout = std.io.getStdOut().writer();

pub fn timing(runs: isize) !void {
    const ts=std.time.nanoTimestamp;
    const warmup = 1_000_000;
    try stdout.print("for {} runs\n",.{runs});
    if (runs > warmup) _ = R.run(warmup);
    var start=ts();
    _ = R.run(runs);
    var base = ts()-start;
    try stdout.print("R: {d:8.3}s {d:8.3}ns\n",.{@intToFloat(f64,base)/1000000000,@intToFloat(f64,base)/@intToFloat(f64,runs)});
    if (runs > warmup) _ = Q.run(warmup);
    start=ts();
    _ = Q.run(runs);
    var time = ts()-start;
    try stdout.print("Q: {d:8.3}s {d:8.3}ns +{d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time)/@intToFloat(f64,runs),@intToFloat(f64,time-base)*100.0/@intToFloat(f64,base)});
    if (runs > warmup) _ = P.run(warmup);
    start=ts();
    _ = P.run(runs);
    time = ts()-start;
    try stdout.print("P: {d:8.3}s {d:8.3}ns +{d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time)/@intToFloat(f64,runs),@intToFloat(f64,time-base)*100.0/@intToFloat(f64,base)});
    if (runs > warmup) _ = N.run(warmup);
    start=ts();
    _ = N.run(runs);
    time = ts()-start;
    try stdout.print("N: {d:8.3}s {d:8.3}ns  {d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time)/@intToFloat(f64,runs),@intToFloat(f64,time)*100.0/@intToFloat(f64,base)});
}
pub fn main() !void {
    try timing(4);
}
