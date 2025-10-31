const std = @import("std");
const testing = std.testing;
const builtin = @import("builtin");

var test_name: []const u8 = "no test active";
var test_file: ?[]const u8 = null;
var test_line: usize = 0;

pub fn main() !void {
    @disableInstrumentation();
    const test_fn_list = builtin.test_functions;
    const root_node = std.Progress.start(.{
        .root_name = "Test",
        .estimated_total_items = test_fn_list.len,
    });
    // The ANSI escape codes for red and reset
    const on_terminal = std.fs.File.stderr().isTty();
    const red = if (on_terminal) "\x1b[1;31m" else "";
    const green = if (on_terminal) "\x1b[1;32m" else "";
    const yellow = if (on_terminal) "\x1b[1;33m" else "";
    const bold = if (on_terminal) "\x1b[1m" else "";
    const reset = if (on_terminal) "\x1b[0m" else "";

    var fail_count: usize = 0;
    var leaks: usize = 0;
    for (test_fn_list) |test_fn| {
        testing.allocator_instance = .{};
        defer {
            if (testing.allocator_instance.deinit() == .leak) {
                leaks += 1;
            }
        }
        testing.log_level = .warn;
        test_name = extractName(test_fn);
        test_file = filenameOf(test_fn);
        test_line = lineOf(test_fn);
        const test_node = root_node.start(test_fn.name, 0);
        const start = std.time.milliTimestamp();
        const result = test_fn.func();
        const elapsed = std.time.milliTimestamp() - start;

        if (result) |_| {
            if (elapsed > 100) {
                std.debug.print("{s}{s} passed - ({d}ms){s}\n", .{ test_name, green, elapsed, reset });
            } else {
                std.debug.print("{s}{s} passed{s}\n", .{ test_name, green, reset });
            }
            test_node.end();
        } else |err| switch (err) {
            error.SkipZigTest => {
                std.debug.print("{s}{s} skipped{s}\n", .{ test_name, yellow, reset });
                test_node.end();
            },
            else => {
                fail_count += 1;
                if (test_file) |filename|
                    std.debug.print("{s}{s}:{d}{s} ", .{ bold, filename, test_line, reset });
                std.debug.print("{s}{s} failed - {s}{s}\n", .{ test_name, red, @errorName(err), reset });
                if (@errorReturnTrace()) |trace| std.debug.dumpStackTrace(trace.*);
                test_node.end();
            },
        }
    }
    root_node.end();
    if (leaks != 0) {
        std.debug.print("{d} tests leaked memory.\n", .{leaks});
    }
    if (leaks != 0 or fail_count != 0) {
        std.process.exit(1);
    }
}
var file_space: [4096]u8 = undefined;
fn filenameOf(t: std.builtin.TestFn) ?[]const u8 {
    @disableInstrumentation();
    if (std.mem.lastIndexOf(u8, t.name, ".test.")) |marker| {
        var len = (std.posix.getcwd(&file_space) catch "").len;
        var path = t.name[0 .. marker];
        while (std.mem.indexOf(u8, path, ".")) |index| {
            len += (std.fmt.bufPrint(file_space[len ..],"/{s}", .{path[0..index]}) catch "").len;
            path = path[index + 1 ..];
        }
        len += (std.fmt.bufPrint(file_space[len ..],".zig", .{}) catch "").len;
        return file_space[0..len];
    }
    return null;
}
fn lineOf(_: std.builtin.TestFn) usize {
    @disableInstrumentation();
    return 1;
}
fn extractName(t: std.builtin.TestFn) []const u8 {
    @disableInstrumentation();
    const marker = std.mem.lastIndexOf(u8, t.name, ".test.") orelse return t.name;
    return t.name[marker + 6 ..];
}

pub const panic = std.debug.FullPanic(std.debug.defaultPanic);//myPanic);

fn myPanic(msg: []const u8, first_trace_addr: ?usize) noreturn {
    @branchHint(.cold);
    const on_terminal = std.fs.File.stderr().isTty();
    const red = if (on_terminal) "\x1b[1;31m" else "";
    const bold = if (on_terminal) "\x1b[1m" else "";
    const reset = if (on_terminal) "\x1b[0m" else "";
    //try std.fmt.format(out,
    if (test_file) |filename|
        std.debug.print("{s}{s}:{d}{s} ", .{ bold, filename, test_line, reset });
    std.debug.print("{s}{s} panic{s}\n", .{ test_name, red, reset });
    std.debug.defaultPanic(msg, first_trace_addr);
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
