const std = @import("std");
const builtin = @import("builtin");
const Process = @import("process.zig");
pub const is_test = builtin.is_test;
pub const testRun = debugMode or show_trace;
pub const debugMode = builtin.mode == .Debug;
pub const native_endian = builtin.target.cpu.arch.endian();
pub const tailCall: std.builtin.CallModifier = if (show_error_stack) .never_inline else .always_tail;
pub fn trace(comptime format: anytype, values: anytype) void {
    if (show_trace) std.debug.print(format, values);
}
const options = @import("options");
pub const includeLLVM = options.includeLLVM;
pub const git_version = options.git_version;
pub const compile_date = options.compile_date;
pub const objectEncoding = options.objectEncoding;
pub const max_classes = options.maxClasses;
// must be more than HeapObject.maxLength*8 so externally allocated
pub const process_total_size: usize = if (is_test) 1024 else if (testRun) 2048 else 64 * 1024;

pub const debugging = false;
pub const logThreadExecution = debugging;
const show_error_stack = debugging;
pub const show_trace = debugging or options.trace;

pub const immediateIntegers = switch (objectEncoding) {
    .zag, .nan, .spur, .zagAlt => true,
    else => false,
};
pub const immediateSymbols = switch (objectEncoding) {
    .zag, .nan, .zagAlt => true,
    else => false,
};
pub const notZag = objectEncoding != .zag;
pub fn skipNotZag() !void {
    if (notZag) return error.SkipZigTest;
}
pub fn printConfig() void {
    std.debug.print(
        \\Config:
        \\  compile_date   = {s}
        \\  git_version    = {s}
        \\  objectEncoding = {}
        \\  max_classes    = {}
        \\  native_endian  = {}
        \\  stack_size     = {d}w
        \\  nursery_size   = {d}w
        \\
    , .{
        compile_date,
        git_version,
        objectEncoding,
        max_classes,
        native_endian,
        Process.process_stack_size,
        Process.process_nursery_size,
    });
    if (show_trace) {
        std.debug.print("  Trace enabled\n", .{});
    }
    if (show_error_stack) {
        std.debug.print("  Error stack enabled\n", .{});
    }
}
comptime {
    @setEvalBranchQuota(100000);
}
