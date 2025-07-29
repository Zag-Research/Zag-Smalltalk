const std = @import("std");
const builtin = @import("builtin");
const options = @import("options");
pub const is_test = builtin.is_test;
pub const native_endian = builtin.target.cpu.arch.endian();
pub const tailCall: std.builtin.CallModifier = if (show_error_stack) .never_inline else .always_tail;
pub fn trace(comptime format: anytype, values: anytype) void {
    if (show_trace) std.debug.print(format, values);
}
pub const includeLLVM = options.includeLLVM;
pub const git_version = options.git_version;
pub const objectEncoding: Encoding = .zag;

pub const debugging = false;
pub const logThreadExecution = debugging;
const show_error_stack = debugging;
const show_trace = debugging;
pub const picSize = 0;
pub const max_classes = 256;

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

test "config" {
    std.debug.print(
        \\Config:
        \\  objectEncoding = {}
        \\  max_classes    = {}
        \\  native_endian  = {}
        \\  git_version    = {s}
        \\
    , .{
        objectEncoding,
        max_classes,
        native_endian,
        git_version,
    });
}
comptime {
    @setEvalBranchQuota(100000);
}
