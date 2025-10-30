const std = @import("std");
const builtin = @import("builtin");

pub fn main() !void {
    //const out = std.io.getStdOut().writer();
    // The ANSI escape codes for red and reset
    const on_terminal = true;
    const red = if (on_terminal) "\x1b[31m" else "";
    const green = if (on_terminal) "\x1b[32m" else "";
    const yellow = if (on_terminal) "\x1b[33m" else "";
    const reset = if (on_terminal) "\x1b[0m" else "";

    for (builtin.test_functions) |t| {
        const start = std.time.milliTimestamp();
        const result = t.func();
        const elapsed = std.time.milliTimestamp() - start;

        const name = t.name; //extractName(t);
        if (result) |_| {
            if (elapsed > 100) {
                //try std.fmt.format(out,
                std.debug.print("{s}{s} passed - ({d}ms){s}\n", .{ name, green, elapsed, reset });
            } else {
                //try std.fmt.format(out,
                std.debug.print("{s}{s} passed{s}\n", .{ name, green, reset });
            }
        } else |err| {
            if (err == error.SkipZigTest) {
                //try std.fmt.format(out,
                std.debug.print("{s}{s} skipped{s}\n", .{ name, yellow, reset });
            } else {
                //try std.fmt.format(out,
                std.debug.print("{s}{s} failed - {}{s}\n", .{ name, red, err, reset });
            }
        }
    }
}

fn extractName(t: std.builtin.TestFn) []const u8 {
    const marker = std.mem.lastIndexOf(u8, t.name, ".test.") orelse return t.name;
    return t.name[marker + 6 ..];
}

//pub const panic = std.debug.FullPanic(myPanic);

fn myPanic(msg: []const u8, first_trace_addr: ?usize) noreturn {
    _ = first_trace_addr;
    std.debug.print("Panic! {s}\n", .{msg});
    std.process.exit(1);
}

//pub const std_options: std.Options = .{.logFn = myLogFn, };

fn myLogFn(
    comptime level: std.log.Level,
    comptime scope: @TypeOf(.EnumLiteral),
    comptime format: []const u8,
    args: anytype,
) void {
    const allocator = std.heap.page_allocator;
    const home = std.os.getenv("HOME") orelse {
        std.debug.print("Failed to read $HOME.\n", .{});
        return;
    };
    const path = std.fmt.allocPrint(allocator, "{s}/{s}", .{ home, ".local/share/my-app.log" }) catch |err| {
        std.debug.print("Failed to create log file path: {}\n", .{err});
        return;
    };
    std.debug.print("\n******path: {s}\n", .{path});
    defer allocator.free(path);

    const file = std.fs.openFileAbsolute(path, .{ .mode = .read_write }) catch |err| {
        std.debug.print("Failed to open log file: {}\n", .{err});
        return;
    };
    defer file.close();
    // const file = std.fs.openFile("logfile", .{ .mode = .read_write }) catch |err| {
    //     std.debug.print("Failed to open log file: {}\n", .{err});
    //     return;
    // };
    // defer file.close();

    // const stat = file.stat() catch |err| {
    //     std.debug.print("Failed to get stat of log file: {}\n", .{err});
    //     return;
    // };
    // file.seekTo(stat.size) catch |err| {
    //     std.debug.print("Failed to seek log file: {}\n", .{err});
    //     return;
    // };

    const prefix = "[" ++ comptime level.asText() ++ "] " ++ "(" ++ @tagName(scope) ++ ") ";

    var buffer: [256]u8 = undefined;
    const message = std.fmt.bufPrint(buffer[0..], prefix ++ format ++ "\n", args) catch |err| {
        std.debug.print("Failed to format log message with args: {}\n", .{err});
        return;
    };
    file.writeAll(message) catch |err| {
        std.debug.print("Failed to write to log file: {}\n", .{err});
    };
}
