const std = @import("std");
const testing = std.testing;
const builtin = @import("builtin");

var test_name: []const u8 = "no test active";
var cwd_buffer: [std.fs.max_path_bytes]u8 = undefined;
var cwd: ?[]const u8 = null;
var red: []const u8 = "";
var green: []const u8 = "";
var yellow: []const u8 = "";
var blue: []const u8 = "";
var cyan: []const u8 = "";
var reset: []const u8 = "";

pub fn main() !void {
    @disableInstrumentation();
    const test_fn_list = builtin.test_functions;
    // The ANSI escape codes for red and reset
    if (std.fs.File.stderr().isTty()) {
        red = "\x1b[1;31m";
        green = "\x1b[1;32m";
        yellow = "\x1b[1;33m";
        blue = "\x1b[1;34m";
        cyan = "\x1b[1;96m";
        reset = "\x1b[0m";
    }
    if (std.fs.cwd().realpath(".", &cwd_buffer)) |dir_name| {
        cwd = dir_name;
    } else |_| {}
    var fail_count: usize = 0;
    var leaks: usize = 0;
    for (test_fn_list) |test_fn| {
        testing.allocator_instance = .{};
        testing.log_level = .warn;
        test_name = extractName(test_fn);
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
                std.debug.print("{s}{s} failed - {s}{s}\n", .{ test_name, red, @errorName(err), reset });
                handleTraces(true);
            },
        }
    }

    if (leaks != 0 or fail_count != 0) {
        std.process.exit(1);
    }
}
const Trace_Option = enum {
    none,
    short,
    full,
};
fn handleTraces(print: bool) void {
    if (@errorReturnTrace()) |trace| {
        if (print) dumpStackTrace(trace.*);
        trace.index = 0;
        trace.instruction_addresses[0] = 0;
    }
}
const native_os = builtin.os.tag;
const io = std.io;
const fs = std.fs;
const mem = std.mem;
const Writer = std.io.Writer;
const lockStderrWriter = std.debug.lockStderrWriter;
const unlockStderrWriter = std.debug.unlockStderrWriter;
const getSelfDebugInfo = std.debug.getSelfDebugInfo;
const SelfInfo = std.debug.SelfInfo;
const SourceLocation = std.debug.SourceLocation;
/// Tries to print a stack trace to stderr, unbuffered, and ignores any error returned.
/// TODO multithreaded awareness
fn dumpStackTrace(stack_trace: std.builtin.StackTrace) void {
    nosuspend {
        if (builtin.target.cpu.arch.isWasm()) {
            if (native_os == .wasi) {
                const stderr = lockStderrWriter(&.{});
                defer unlockStderrWriter();
                stderr.writeAll("Unable to dump stack trace: not implemented for Wasm\n") catch return;
            }
            return;
        }
        const stderr = lockStderrWriter(&.{});
        defer unlockStderrWriter();
        if (builtin.strip_debug_info) {
            stderr.writeAll("Unable to dump stack trace: debug info stripped\n") catch return;
            return;
        }
        const debug_info = getSelfDebugInfo() catch |err| {
            stderr.print("Unable to dump stack trace: Unable to open debug info: {s}\n", .{@errorName(err)}) catch return;
            return;
        };
        writeStackTrace(stack_trace, stderr, debug_info, io.tty.detectConfig(.stderr())) catch |err| {
            stderr.print("Unable to dump stack trace: {s}\n", .{@errorName(err)}) catch return;
            return;
        };
    }
}
fn writeStackTrace(
    stack_trace: std.builtin.StackTrace,
    writer: *Writer,
    debug_info: *SelfInfo,
    tty_config: io.tty.Config,
) !void {
    if (builtin.strip_debug_info) return error.MissingDebugInfo;
    var frame_index: usize = 0;
    var frames_left: usize = @min(stack_trace.index, stack_trace.instruction_addresses.len);

    while (frames_left != 0) : ({
        frames_left -= 1;
        frame_index = (frame_index + 1) % stack_trace.instruction_addresses.len;
    }) {
        const return_address = stack_trace.instruction_addresses[frame_index];
        try printSourceAtAddress(debug_info, writer, return_address - 1, tty_config);
    }

    if (stack_trace.index > stack_trace.instruction_addresses.len) {
        const dropped_frames = stack_trace.index - stack_trace.instruction_addresses.len;

        tty_config.setColor(writer, .bold) catch {};
        try writer.print("({d} additional stack frames skipped...)\n", .{dropped_frames});
        tty_config.setColor(writer, .reset) catch {};
    }
}
fn printUnknownSource(debug_info: *SelfInfo, writer: *Writer, address: usize, tty_config: io.tty.Config) !void {
    const module_name = debug_info.getModuleNameForAddress(address);
    return printLineInfo(
        writer,
        null,
        address,
        "???",
        module_name orelse "???",
        tty_config,
        printLineFromFileAnyOs,
    );
}
fn printSourceAtAddress(debug_info: *SelfInfo, writer: *Writer, address: usize, tty_config: io.tty.Config) !void {
    const module = debug_info.getModuleForAddress(address) catch |err| switch (err) {
        error.MissingDebugInfo, error.InvalidDebugInfo => return printUnknownSource(debug_info, writer, address, tty_config),
        else => return err,
    };

    const symbol_info = module.getSymbolAtAddress(debug_info.allocator, address) catch |err| switch (err) {
        error.MissingDebugInfo, error.InvalidDebugInfo => return printUnknownSource(debug_info, writer, address, tty_config),
        else => return err,
    };
    defer if (symbol_info.source_location) |sl| debug_info.allocator.free(sl.file_name);

    if (symbol_info.source_location) |*sl| {
        if (cwd) |directory_name| {
            if (!std.mem.startsWith(u8, sl.file_name, directory_name)) {
                return;
            }
        }
    }

    return printLineInfo(
        writer,
        symbol_info.source_location,
        address,
        symbol_info.name,
        symbol_info.compile_unit_name,
        tty_config,
        printLineFromFileAnyOs,
    );
}
fn printLineInfo(
    writer: *Writer,
    source_location: ?SourceLocation,
    address: usize,
    symbol_name: []const u8,
    compile_unit_name: []const u8,
    tty_config: io.tty.Config,
    comptime printLineFromFile: anytype,
) !void {
    nosuspend {
        try tty_config.setColor(writer, .bold);

        if (source_location) |*sl| {
            try writer.print("{s}:{d}:{d}", .{ sl.file_name, sl.line, sl.column });
        } else {
            try writer.writeAll("???:?:?");
        }

        try tty_config.setColor(writer, .reset);
        try writer.writeAll(": ");
        try tty_config.setColor(writer, .dim);
        try writer.print("0x{x} in {s} ({s})", .{ address, symbol_name, compile_unit_name });
        try tty_config.setColor(writer, .reset);
        try writer.writeAll("\n");

        // Show the matching source code line if possible
        if (source_location) |sl| {
            if (printLineFromFile(writer, sl)) {
                if (sl.column > 0) {
                    // The caret already takes one char
                    const space_needed = @as(usize, @intCast(sl.column - 1));

                    try writer.splatByteAll(' ', space_needed);
                    try tty_config.setColor(writer, .green);
                    try writer.writeAll("^");
                    try tty_config.setColor(writer, .reset);
                }
                try writer.writeAll("\n");
            } else |err| switch (err) {
                error.EndOfFile, error.FileNotFound => {},
                error.BadPathName => {},
                error.AccessDenied => {},
                else => return err,
            }
        }
    }
}
fn printLineFromFileAnyOs(writer: *Writer, source_location: SourceLocation) !void {
    // Need this to always block even in async I/O mode, because this could potentially
    // be called from e.g. the event loop code crashing.
    var f = try fs.cwd().openFile(source_location.file_name, .{});
    defer f.close();
    // TODO fstat and make sure that the file has the correct size

    var buf: [4096]u8 = undefined;
    var amt_read = try f.read(buf[0..]);
    const line_start = seek: {
        var current_line_start: usize = 0;
        var next_line: usize = 1;
        while (next_line != source_location.line) {
            const slice = buf[current_line_start..amt_read];
            if (mem.indexOfScalar(u8, slice, '\n')) |pos| {
                next_line += 1;
                if (pos == slice.len - 1) {
                    amt_read = try f.read(buf[0..]);
                    current_line_start = 0;
                } else current_line_start += pos + 1;
            } else if (amt_read < buf.len) {
                return error.EndOfFile;
            } else {
                amt_read = try f.read(buf[0..]);
                current_line_start = 0;
            }
        }
        break :seek current_line_start;
    };
    const slice = buf[line_start..amt_read];
    if (mem.indexOfScalar(u8, slice, '\n')) |pos| {
        const line = slice[0 .. pos + 1];
        mem.replaceScalar(u8, line, '\t', ' ');
        return writer.writeAll(line);
    } else { // Line is the last inside the buffer, and requires another read to find delimiter. Alternatively the file ends.
        mem.replaceScalar(u8, slice, '\t', ' ');
        try writer.writeAll(slice);
        while (amt_read == buf.len) {
            amt_read = try f.read(buf[0..]);
            if (mem.indexOfScalar(u8, buf[0..amt_read], '\n')) |pos| {
                const line = buf[0 .. pos + 1];
                mem.replaceScalar(u8, line, '\t', ' ');
                return writer.writeAll(line);
            } else {
                const line = buf[0..amt_read];
                mem.replaceScalar(u8, line, '\t', ' ');
                try writer.writeAll(line);
            }
        }
        // Make sure printing last line of file inserts extra newline
        try writer.writeByte('\n');
    }
}

fn extractName(t: std.builtin.TestFn) []const u8 {
    @disableInstrumentation();
    const marker = std.mem.lastIndexOf(u8, t.name, ".test.") orelse return t.name;
    return t.name[marker + 6 ..];
}

pub const panic = std.debug.FullPanic(myPanic);

fn myPanic(msg: []const u8, first_trace_addr: ?usize) noreturn {
    @branchHint(.cold);
    std.debug.print("{s}panic in test: {s}{s}\n", .{ red, reset, test_name });
    std.debug.defaultPanic(msg, first_trace_addr);
}

pub const std_options: std.Options = .{
    // Set the log level to warning
    .log_level = .debug,

    // Define logFn to override the std implementation
    .logFn = myLogFn,
};

pub fn myLogFn(
    comptime level: std.log.Level,
    comptime _: @Type(.enum_literal),
    comptime format: []const u8,
    args: anytype,
) void {
    std.debug.lockStdErr();
    defer std.debug.unlockStdErr();
    const stderr = std.fs.File.stderr().deprecatedWriter();
    const color = switch (level) {
        .debug => cyan,
        .info => green,
        .warn => yellow,
        .err => red,
    };
    nosuspend stderr.print("{s}" ++ comptime level.asText() ++ "{s}: ", .{ color, reset}) catch return;
    nosuspend stderr.print(format ++ "\n", args) catch return;
}
