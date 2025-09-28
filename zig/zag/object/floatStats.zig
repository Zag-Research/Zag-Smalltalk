const std = @import("std");
// zig run -Doptimize=ReleaseFast -fomit-frame-pointer floatStats.zig
pub fn main() void {
    const iterations = 1000000000;
    var timer = std.time.Timer.start() catch unreachable;

    const zag = @import("floatZag.zig");
    _ = timer.lap();
    zag.encode_valid(iterations);
    const zag_valid_time = timer.lap();
    zag.encode_invalid(iterations);
    const zag_invalid_time = timer.lap();
    zag.decode_valid(iterations);
    const zag_decode_time = timer.lap();
    std.debug.print("zag time: {}ns {}ns {}ns\n", .{ zag_valid_time, zag_invalid_time, zag_decode_time });

    const spur = @import("floatSpur.zig");
    _ = timer.lap();
    spur.encode_valid(iterations);
    const spur_valid_time = timer.lap();
    spur.encode_invalid(iterations);
    const spur_invalid_time = timer.lap();
    spur.decode_valid(iterations);
    const spur_decode_time = timer.lap();
    std.debug.print("Spur time: {}ns {}ns {}ns\n", .{ spur_valid_time, spur_invalid_time, spur_decode_time });

    std.debug.print("Zag is {d:.2}x {d:.2}x {d:.2}x faster\n",
        .{ delta(zag_valid_time, spur_valid_time),
            delta(zag_invalid_time, spur_invalid_time),
            delta(zag_decode_time, spur_decode_time) });
}
fn delta(zag: u64, spur: u64) f64 {
    return @as(f64, @floatFromInt(spur)) / @as(f64, @floatFromInt(zag));
}
