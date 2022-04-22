const std = @import("std");
const expect = std.testing.expect;
const assert = @import("std").debug.assert;
const stdout = @import("std").io.getStdOut().writer();
const O = @import("object.zig");
const Nil = O.Nil;
const Object = O.Object;
const Class = @import("class.zig");
const Class_S = Class.Class_S;
const Dispatch = @import("dispatch.zig");
const MethodReturns = Dispatch.MethodReturns;
const Normal = MethodReturns.Normal;
const Thread = @import("thread.zig");
const Symbol = @import("symbol.zig");

fn startMethod(thread : *Thread.Thread, self: Object) MethodReturns {
    _ = thread;
    _ = self;
    return Normal;
}
test "try a thread" {
    var thread = try Thread.Thread.initForTest();
    defer thread.deinit();
    try Symbol.init(&thread,250);
    const _start = Symbol.internLiteral(&thread,"start");
    const _System = Symbol.internLiteral(&thread,"System");
    const System = Class.getClass(_System);
    System.to(Class_S).addMethod(_start,startMethod);
    thread.push(System);
    switch (System.dispatch(&thread,_start)) {
        .Normal => {
            try expect(thread.stack()[0].is_nil());
        },
        else => unreachable,
    }
}
