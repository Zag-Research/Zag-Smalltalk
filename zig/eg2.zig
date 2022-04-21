const std = @import("std");
const expect = std.testing.expect;
const assert = @import("std").debug.assert;
const stdout = @import("std").io.getStdOut().writer();
const O = @import("object.zig");
const Nil = O.Nil;
const Object = O.Object;
const Class = @import("class.zig");
const Dispatch = @import("dispatch.zig");
const MethodReturns = Dispatch.MethodReturns;
const Thread = @import("thread.zig");
const Symbol = @import("symbol.zig");

fn startMethod(thread : *Thread, self: Object) MethodReturns {
    _ = thread;
    _ = self;
    return MethodReturns.Normal;
}
test "try a thread" {
    var thread = try Thread.Thread.initForTest();
    defer thread.deinit();
    try Symbol.init(&thread,250);
    const _start = Symbol.internLiteral(&thread,"start");
    const _System = Symbol.internLiteral(&thread,"System");
    const System = Class.getClass(_System);
    System.addMethod(_start,startMethod);
    const result = System.dispatch(&thread,_start);
    try expect(result.is_nil());
}
