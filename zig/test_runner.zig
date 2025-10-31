const std = @import("std");
const testing = std.testing;
const builtin = @import("builtin");

var test_name: []const u8 = "no test active";
var test_file: ?[]const u8 = null;
var test_line: usize = 0;

pub fn main() !void {
    @disableInstrumentation();
    const test_fn_list = builtin.test_functions;
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
        testing.log_level = .warn;
        test_name = extractName(test_fn);
        test_file = filenameOf(test_fn);
        test_line = lineOf(test_fn);

        const start = std.time.milliTimestamp();
        const result = test_fn.func();
        const elapsed = std.time.milliTimestamp() - start;

        if (testing.allocator_instance.deinit() == .leak) {
            leaks += 1;
        }

        if (result) |_| {
            if (elapsed > 100) {
                std.debug.print("{s}{s} passed - ({d}ms){s}\n", .{ test_name, green, elapsed, reset });
            } else {
                std.debug.print("{s}{s} passed{s}\n", .{ test_name, green, reset });
            }
        } else |err| switch (err) {
            error.SkipZigTest => {
                std.debug.print("{s}{s} skipped{s}\n", .{ test_name, yellow, reset });
                handleTraces(false);
            },
            else => {
                fail_count += 1;
                if (test_file) |filename|
                    std.debug.print("{s}{s}:{d}{s} ", .{ bold, filename, test_line, reset });
                std.debug.print("{s}{s} failed - {s}{s}\n", .{ test_name, red, @errorName(err), reset });
                handleTraces(true);
            },
        }
    }

    if (leaks != 0 or fail_count != 0) {
        std.process.exit(1);
    }
}
fn handleTraces(print: bool) void {
    if (@errorReturnTrace()) |trace| {
        if (print) std.debug.dumpStackTrace(trace.*);
        trace.index = 0;
        trace.instruction_addresses[0] = 0;
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

pub const panic = std.debug.FullPanic(myPanic);

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
