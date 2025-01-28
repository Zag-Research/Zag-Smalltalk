const std = @import("std");
const builtin = @import("builtin");
pub const native_endian = builtin.target.cpu.arch.endian();
pub const tailCall: std.builtin.CallModifier = if (show_error_stack) .never_inline else .always_tail;
pub inline fn trace(format: anytype, values: anytype) void {
    if (show_trace) std.debug.print(format, values);
}
pub const stdCall: std.builtin.CallingConvention = .auto; //if (builtin.cpu.arch == .x86) .Stdcall else .C; //.AAPCS;
pub const objectEncoding = Encoding.tag;

const Encoding = enum {
    nan,
    tag,
};

const debugging = false;
const show_error_stack = debugging;
const show_trace = true;
pub const picSize = 0;
pub const max_classes = 100;
