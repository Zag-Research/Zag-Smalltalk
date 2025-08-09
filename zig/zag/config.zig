const std = @import("std");
const builtin = @import("builtin");
const Process = @import("process.zig");
pub const is_test = builtin.is_test;
pub const native_endian = builtin.target.cpu.arch.endian();
pub const tailCall: std.builtin.CallModifier = if (show_error_stack) .never_inline else .always_tail;
pub fn trace(comptime format: anytype, values: anytype) void {
    if (show_trace) std.debug.print(format, values);
}
const accept_options = false;
const options = if (accept_options) @import("options") else struct {
    const includeLLVM = false;
    const git_version = "no options flags";
};
pub const includeLLVM = options.includeLLVM;
pub const git_version = options.git_version;
pub const objectEncoding: Encoding = .zag;
pub const max_classes = 256;
// must be more than HeapObject.maxLength*8 so externally allocated
pub const process_total_size = if (is_test) 2048 else 64 * 1024;

pub const debugging = false;
pub const logThreadExecution = debugging;
const show_error_stack = debugging;
const show_trace = debugging;

pub const immediateIntegers = switch (objectEncoding) {
    .zag, .nan, .spur => true,
    else => false,
};
pub const immediateSymbols = switch (objectEncoding) {
    .zag, .nan => true,
    else => false,
};
pub const notZag = objectEncoding != .zag;
const Encoding = enum {
    zag,
    nan,
    spur,
    taggedPtr,
    cachedPtr,
    ptr,
};
pub fn skipNotZag() !void {
    if (notZag) return error.SkipZigTest;
}
pub fn printConfig() void {
    std.debug.print(
        \\Config:
        \\  objectEncoding = {}
        \\  max_classes    = {}
        \\  native_endian  = {}
        \\  git_version    = {s}
        \\  stack_size     = {d}w
        \\  nursery_size   = {d}w
        \\
    , .{
        objectEncoding,
        max_classes,
        native_endian,
        git_version,
        Process.process_stack_size,
        Process.process_nursery_size,
    });
}
test "printConfig" {
    printConfig();
}
comptime {
    @setEvalBranchQuota(100000);
}
