const _std = @import("std");
const _expect = _std.testing.expect;
const _assert = _std.debug.assert;
const _stdout = _std.io.getStdOut();
const _object = @import("object.zig");
const _class = @import("class.zig");
const _dispatch = @import("dispatch.zig");
const _thread = @import("thread.zig");
const _symbol = @import("symbol.zig");
const _O = _object.Object;
const _c = struct {
    const _class_S = _class.Class_S;
};
const _MR = _dispatch.MethodReturns;
const _DP = _dispatch.DispatchPtr;
const _CXT = _dispatch.Context;
const _dnu = _dispatch.dnu;
const _s = struct {
    const nil = _object.Nil;
    const @"true" = _object.True;
    const @"false" = _object.False;
    const _s0 = _symbol.symbol0;
    const start = _s0(49);
    usingnamespace _symbol.symbols;
};
fn _init_symbolTable(thread: *_thread.Thread) void {
    _symbol.init(thread,250,
\\ start
                 ) catch @panic("_init_symbolTable failed");
}
const System_defs = struct {
    fn startMethod(selector: _O, self: _O, other: _O, _cc: *_CXT, _dp: _DP) _MR {
        if (!selector.equals(_s.start)) return _dnu(selector,self,other,_cc,_dp);
        return _MR{.Normal=self};
    }
    const instance_methods = ([_]_dispatch.SymbolMethod{
        .{.selector=_s.start,.method=startMethod},
    })[0..];
    const class_methods = ([0]_dispatch.SymbolMethod{
    })[0..];
    fn init(t:*_thread.Thread) !_O {
        try _stdout.writer().print("before getClass\n",.{});
        const class = _class.getClass(_s.System);
        try _stdout.writer().print("before addClass\n",.{});
        try _dispatch.addClass(t,_s.System,instance_methods,class_methods);
        return class;
    }
};
test "try a thread" {
    var thread = try _thread.Thread.initForTest();
    defer thread.deinit();
    _init_symbolTable(&thread);
    var cxt: [4]_O=undefined;
    var context = _dispatch.make_init_cxt(cxt[0..],&thread);
    const System = try System_defs.init(&thread);
    thread.push(System);
    try _stdout.writer().print("before dispatch\n",.{});
    switch (System.send(_s.start,_s.nil,context)) {
        .Normal => {
            try _expect(thread.stack()[0].is_nil());
        },
        else => |result| {
            try _stdout.writer().print("result={}\n",.{result});
            unreachable;
        },
    }
}
