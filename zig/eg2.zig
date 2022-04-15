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

test "try a thread" {
    var thread = try Thread.Thread.initForTest();
    defer thread.deinit();
    const Symbol = struct {
        usingnamespace @import("symbol.zig");
        @This.init(thread,250);
        const start = @This().symbol_of(42,0);
        const add_ = @This().symbol_of(43,1);
    };
    try expect(true);
}
