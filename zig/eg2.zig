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
const _Symbol = @import("symbol.zig");
fn make_symbols (thread : *Thread.Thread) type {
    return struct {
        usingnamespace _Symbol.symbol;
        const start = _Symbol.internLiteral(&thread,"start");
        const System = _Symbol.internLiteral(&thread,"System");
    };
}
test "try a thread" {
    var thread = try Thread.Thread.initForTest();
    defer thread.deinit();
    try _Symbol.init(&thread,250);
    //const _sym = make_symbols(&thread);
    //_ = _sym;
    // const System = Class.getClass(_sym.System);
    // System.to(Class_S).addMethod(_sym.start,startMethod);
    // thread.push(System);
    // switch (System.dispatch(&thread,_sym.start)) {
    //     .Normal => {
    //         try expect(thread.stack()[0].is_nil());
    //     },
    //     else => unreachable,
    // }
}
fn startMethod(thread : *Thread.Thread, self: Object) MethodReturns {
    _ = thread;
    _ = self;
    return Normal;
}
