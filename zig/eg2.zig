const std = @import("std");
const expect = std.testing.expect;
const assert = @import("std").debug.assert;
const stdout = @import("std").io.getStdOut().writer();
const O = @import("object.zig");
const Nil = O.Nil;
const Object = O.Object;
const _O = Object;
const Class = @import("class.zig");
const Class_S = Class.Class_S;
const Dispatch = @import("dispatch.zig");
const MethodReturns = Dispatch.MethodReturns;
const Normal = MethodReturns.Normal;
const Thread = @import("thread.zig");
const _Symbol = @import("symbol.zig");
const _s0 = _Symbol.symbol0;
const _s = struct {
    const start = _s0(49);
    usingnamespace _Symbol.symbols;
};

test "try a thread" {
    var thread = try Thread.Thread.initForTest();
    defer thread.deinit();
    const System_methods = struct {
        fn startMethod(_thread : *Thread.Thread, self: Object) MethodReturns {
            _ = _thread;
            _ = self;
            return Normal;
        }
    };
    const System_dispatch = [_]Dispatch.SymbolMethod{
        .{.selector=_s.start,.method=System_methods.startMethod},
    };
    try _Symbol.init(&thread,250,
\\ start
                     );
    try std.io.getStdOut().writer().print("before getClass\n",.{});
    const System = Class.getClass(_s.System);
    try std.io.getStdOut().writer().print("before addClass\n",.{});
    try Dispatch.addClass(&thread,_s.System,System_dispatch[0..],Dispatch.noMethods[0..]);
    thread.push(System);
    try std.io.getStdOut().writer().print("before dispatch\n",.{});
    switch (System.dispatch(&thread,_s.start)) {
        .Normal => {
            try expect(thread.stack()[0].is_nil());
        },
        else => |result| {
            try std.io.getStdOut().writer().print("result={}\n",.{result});
            unreachable;
        },
    }
}
