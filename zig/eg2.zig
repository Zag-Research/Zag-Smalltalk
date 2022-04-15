const std = @import("std");
const expect = std.testing.expect;
const assert = @import("std").debug.assert;
const stdout = @import("std").io.getStdOut().writer();
const O = @import("object.zig");
const Nil = O.Nil;
const Object = O.Object;
const Dispatch = @import("dispatch.zig");
const returnE = Dispatch.returnE;
const Thread = @import("thread.zig");
const Symbol = @import("symbol.zig");

test "try a thread" {
    var thread = try Thread.Thread.initForTest();
    defer thread.deinit();
    try Symbol.init(&thread,250);
    const _start = Symbol.internLiteral(&thread,"start");
    const _System = Symbol.internLiteral(&thread,"start");
    _ = _start;
    _ = _System;
    try expect(true);
}
