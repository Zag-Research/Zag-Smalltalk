const std = @import("std");
const config = @import("config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const checkEqual = @import("utilities.zig").checkEqual;
const object = @import("zobject.zig");
const Object = object.Object;
const ClassIndex = object.ClassIndex;
const Nil = object.Nil;
const NotAnObject = object.NotAnObject;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const indexSymbol0 = object.Object.indexSymbol0;
const indexSymbol1 = object.Object.indexSymbol1;
pub const Context = @import("context.zig").Context;

//const class = @import("class.zig");
const symbol = @import("symbol.zig");
const Sym = symbol.symbols;
const phi32 = @import("utilities.zig").inversePhi(u32);

const execute = @import("execute.zig");

pub const newLabel = struct {
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
        _ = .{ pc, sp, process, context, extra };
        return @call(tailCall, process.check(context.nPc()), .{ context.tPc(), sp, process, context, undefined });
    }
};
pub const @"literalToRegister:" = struct {
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
        const valueToPush = sp.top;
        _ = .{ valueToPush, pc, sp, process, context, extra };
        return @call(tailCall, process.check(context.nPc()), .{ context.tPc(), sp, process, context, undefined });
    }
};
pub const @"add:to:" = struct {
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
        _ = .{ pc, sp, process, context, extra };
        return @call(tailCall, process.check(context.nPc()), .{ context.tPc(), newSp, process, context, undefined });
    }
};

test "primitives" {}

extern fn llvmPL(_: *anyopaque, _: *anyopaque, _: *anyopaque, _: *anyopaque, c_int) *anyopaque;
const llvmPLCM = CompiledMethod.init(Sym.yourself, @ptrCast(&llvmPL));
test "simple llvm" {
    std.debug.print("Test: simple llvm\n", .{});
    const expectEqual = std.testing.expectEqual;
    Process.resetForTest();
    const pl = if (true) &p.pushLiteral else llvmPLCM;
    var method = compileMethod(Sym.yourself, 0, 0, .none, .{
        pl,                    0,
        &p.returnTopNoContext,
    });
    var te = Execution.new();
    te.init(null);
    var objs = [_]Object{ Nil, True };
    const result = te.run(objs[0..], &method);
    try expectEqual(result.len, 3);
    try expectEqual(result[0], Object.from(0));
    try expectEqual(result[1], Nil);
    try expectEqual(result[2], True);
}
test "llvm external" {
    std.debug.print("Test: llvm external\n", .{});
    const expectEqual = std.testing.expectEqual;
    Process.resetForTest();
    const pl = if (true) &p.pushLiteral else llvmPLCM;
    var method = compileMethod(Sym.yourself, 1, 0, .none, .{
        &p.pushContext,   "^",
        ":label1",        &p.pushLiteral,
        42,               &p.popLocal,
        0,                &p.pushLocal,
        0,                pl,
        0,                &p.pushLiteral,
        true,             &p.classCase,
        ClassIndex.False, "label3",
        &p.branch,        "label2",
        ":label3",        &p.pushLocal,
        0,                ":label4",
        &p.returnTop,     ":label2",
        pl,               0,
        &p.branch,        "label4",
    });
    method.resolve();
    const debugging = false;
    if (debugging) {
        @setRuntimeSafety(false);
        for (&method.code, 0..) |*tv, idx|
            trace("\nt[{}]=0x{x:0>8}: 0x{x:0>16}", .{ idx, @intFromPtr(tv), tv.object.rawU() });
    }
    var objs = [_]Object{ Nil, Nil, Nil };
    var te = Execution.new();
    te.init(null);
    const result = te.run(objs[0..], &method);
    try expectEqual(result.len, 1);
    try expectEqual(result[0], Object.from(0));
}
