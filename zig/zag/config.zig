const std = @import("std");
const debugging = true;
const show_error_stack = debugging;
const show_trace = debugging;
pub const tailCall: std.builtin.CallModifier = if (show_error_stack) .never_inline else .always_tail;
pub inline fn trace(format: anytype, values: anytype) void {
    if (show_trace) std.debug.print(format,values);
}
pub const dispatchCache = false;
