const std = @import("std");
const debugging = true;
const show_error_stack = debugging;
const show_trace = debugging;
pub const tailCall: std.builtin.CallModifier = if (show_error_stack) .never_inline else .always_tail;
pub const trace = if (show_trace) std.debug.print else trace_;
inline fn trace_(_: anytype, _: anytype) void {}
