const std = @import("std");
const builtin = @import("builtin");
pub const is_test = builtin.is_test;
pub const native_endian = builtin.target.cpu.arch.endian();
pub const tailCall: std.builtin.CallModifier = if (show_error_stack) .never_inline else .always_tail;
pub fn trace(comptime format: anytype, values: anytype) void {
    if (show_trace) std.debug.print(format, values);
}
pub const objectEncoding = Encoding.tag;

const Encoding = enum {
    nan,
    tag,
};

pub const debugging = false;
const show_error_stack = debugging;
const show_trace = debugging;
pub const picSize = 0;
pub const max_classes = 100;
