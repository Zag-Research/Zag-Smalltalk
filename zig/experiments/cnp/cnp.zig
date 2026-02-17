const std = @import("std");
const builtin = @import("builtin");
const harness = @import("test_harness.zig");
const tests = @import("tests.zig");

pub fn main() !void {
    std.debug.print("Running CnP Tests...\n", .{});
    std.debug.print("arch: {s}\n", .{@tagName(builtin.cpu.arch)});
    std.debug.print("opt: {s}\n", .{@tagName(builtin.mode)});

    try harness.runTest(tests.PushTest);
    try harness.runTest(tests.PushLiteralTest);
    try harness.runTest(tests.TailCallPatchTest);
    try harness.runTest(tests.ReturnSelfTest);
    try harness.runTest(tests.BranchFalseTest);
    try harness.runTest(tests.InlinePrimitiveAddTest);
    try harness.runTest(tests.InlinePrimitiveChainedTest);
    try harness.runTest(tests.Send0Test);
    try harness.runTest(tests.SendTest);
}
