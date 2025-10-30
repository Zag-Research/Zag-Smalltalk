const std = @import("std");
// zig run -Doptimize=ReleaseFast -fomit-frame-pointer floatStats.zig
pub fn main() void {
    const iterations = 1_000_000_000;
    const ns = 1.0 / @as(f64, @floatFromInt(iterations));
    var timer = std.time.Timer.start() catch unreachable;

    const zag = @import("floatZag.zig");
    _ = timer.lap();
    zag.encode_valid(iterations);
    const zag_valid_time = timer.lap();
    zag.encode_invalid(iterations);
    const zag_invalid_time = timer.lap();
    zag.decode_valid(iterations);
    const zag_decode_time = timer.lap();
    std.log.err("zag time: {d:.3}ns {d:.3}ns {d:.3}ns\n", .{ @as(f64, @floatFromInt(zag_valid_time))*ns, @as(f64, @floatFromInt(zag_invalid_time))*ns, @as(f64, @floatFromInt(zag_decode_time))*ns });

    const spur = @import("floatSpur.zig");
    _ = timer.lap();
    spur.encode_valid(iterations);
    const spur_valid_time = timer.lap();
    spur.encode_invalid(iterations);
    const spur_invalid_time = timer.lap();
    spur.decode_valid(iterations);
    const spur_decode_time = timer.lap();
    std.log.err("Spur time: {d:.3}ns {d:.3}ns {d:.3}ns\n", .{ @as(f64, @floatFromInt(spur_valid_time))*ns, @as(f64, @floatFromInt(spur_invalid_time))*ns, @as(f64, @floatFromInt(spur_decode_time))*ns });

    std.log.err("Zag is {d:.2}x {d:.2}x {d:.2}x faster\n",
        .{ delta(zag_valid_time, spur_valid_time),
            delta(zag_invalid_time, spur_invalid_time),
            delta(zag_decode_time, spur_decode_time) });
}
fn delta(zag: u64, spur: u64) f64 {
    return @as(f64, @floatFromInt(spur)) / @as(f64, @floatFromInt(zag));
}
