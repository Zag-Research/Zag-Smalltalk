const _std = @import("std");
const _expect = _std.testing.expect;
const _assert = _std.debug.assert;
const _stdout = _std.io.getStdOut();
const _object = @import("object.zig");
const _class = @import("class.zig");
const _init = _class.init_class;
const _dispatch = @import("dispatch.zig");
const _thread = @import("thread.zig");
const _symbol = @import("symbol.zig");
const _prim = @import("primitives.zig");
const _O = _object.Object;
const _nil = _object.Nil;
const _c = struct {
    const _class_S = _class.Class_S;
};
const _MR = _dispatch.MethodReturns;
const _DP = _dispatch.DispatchPtr;
const _CXT = _dispatch.Context;
const _dnu = _dispatch.dnu;
const _SM = _dispatch.SymbolMethod;
const _DO = _dispatch.DNUOption;
const _s = struct {
    const nil = _nil;
    const @"true" = _object.True;
    const @"false" = _object.False;
    const _s0 = _symbol.symbol0;
    const _n = _symbol.predefinedSymbols;
    const start = _s0(_n+0);
    const main = _s0(_n+2);
    usingnamespace _symbol.symbols;
};
fn _init_symbolTable(thread: *_thread.Thread) void {
    _symbol.init(thread,250,
\\ start main
                 ) catch @panic("_init_symbolTable failed");
}
const Object_defs = struct {
    fn class_MI(selector: _O, self: _O, other: _O, _cc: *_CXT, _dp: _DP, _do: _DO) _MR {
        if (!selector.equals(_s.class)) return _dnu(selector,self,other,_cc,_dp,_do,_s.class);
        return _MR{.Normal=_prim.prim_111_class(self,other,_cc) catch unreachable};
    }
    const instance_methods = ([_]_SM{
    })[0..];
    const class_methods = ([_]_SM{
        .{.selector=_s.class,.method=class_MI},
    })[0..];
    inline fn init(t:*_thread.Thread) !_O { return _init(t,_s.System,instance_methods,class_methods);}
};
const System_defs = struct {
    fn start_MC(selector: _O, self: _O, other: _O, _cc: *_CXT, _dp: _DP, _do: _DO) _MR {
        if (!selector.equals(_s.start)) return _dnu(selector,self,other,_cc,_dp,_s.start,_do);
        return _MR{.Normal=self};
    }
    const instance_methods = ([_]_dispatch.SymbolMethod{
    })[0..];
    const class_methods = ([_]_dispatch.SymbolMethod{
        .{.selector=_s.start,.method=start_MC},
    })[0..];
    fn init(t:*_thread.Thread) !_O { return _init(t,_s.System,instance_methods,class_methods);}
};
test "try a thread" {
    var thread = try _thread.Thread.initForTest();
    defer thread.deinit();
    _init_symbolTable(&thread);
    var cxt = [_]_O{_nil,_s.main}++[_]_O{_nil}**3;
    var context = _dispatch.make_init_cxt(cxt[0..],&thread);
    const System = try System_defs.init(&thread);
    try _stdout.writer().print("before dispatch\n",.{});
    switch (System.send(_s.start,_s.nil,context)) {
        .Normal => {
            try _expect(thread.stack()[0].is_nil());
        },
        else => |result| {
            try _stdout.writer().print("result={}\n",.{result});
            return error.Fail;
        },
    }
}
