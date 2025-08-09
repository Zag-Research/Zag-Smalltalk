const std = @import("std");
const zag = @import("../zag.zig");
const config = zag.config;
const tailCall = config.tailCall;
const trace = config.trace;
const execute = zag.execute;
const Context = zag.Context;
const Code = execute.Code;
const PC = execute.PC;
const SP = Process.SP;
const Extra = Context.Extra;
const Result = execute.Result;
const Execution = execute.Execution;
const CompiledMethod = execute.CompiledMethod;
const Process = zag.Process;
const object = zag.object;
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const Sym = zag.symbol.symbols;
const rawSymbol = zag.symbol.rawSymbol;
const heap = zag.heap;
const empty = &[0]Object{};
const tf = zag.threadedFn.Enum;
const blockClosure = @import("BlockClosure.zig");

pub fn init() void {}
pub const inlines = struct {
    pub inline fn pxxxBoolean(self: Object, other: Object) !Object { // at:
        _ = self;
        _ = other;
        return error.primitiveError;
    }
};
pub const embedded = struct {
    const fallback = execute.fallback;
    pub fn @"ifTrue:"(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
        const v = sp[1];
        if (True.equals(v)) {
            sp[1] = sp[0];
            return @call(tailCall, process.check(blockClosure.embedded.value), .{ pc, sp + 1, process, context, undefined, undefined });
        }
        if (False.equals(v)) {
            sp[1] = Nil;
            return @call(tailCall, process.check(pc[0].prim), .{ pc + 1, sp + 1, process, context, undefined });
        }
        return @call(tailCall, process.check(fallback), .{ pc, sp, process, context, Sym.@"ifTrue:", undefined });
    }
    pub fn @"ifFalse:"(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
        const v = sp[1];
        if (False.equals(v)) {
            sp[1] = sp[0];
            return @call(tailCall, process.check(blockClosure.embedded.value), .{ pc, sp + 1, process, context, undefined });
        }
        if (True.equals(v)) {
            sp[1] = Nil;
            return @call(tailCall, process.check(pc[0].prim), .{ pc, sp + 1, process, context });
        }
        return @call(tailCall, process.check(fallback), .{ pc, sp, process, context });
    }
    pub fn @"ifTrue:ifFalse:"(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const v = sp[2];
        if (True.equals(v)) {
            sp[2] = sp[1];
            return @call(tailCall, process.check(blockClosure.embedded.value), .{ pc, sp + 2, process, context });
        }
        if (False.equals(v)) {
            sp[2] = sp[0];
            return @call(tailCall, process.check(blockClosure.embedded.value), .{ pc, sp + 2, process, context });
        }
        return @call(tailCall, process.check(fallback), .{ pc, sp, process, context, extra });
    }
    pub fn @"ifFalse:ifTrue:"(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const v = sp[2];
        if (False.equals(v)) {
            sp[2] = sp[1];
            return @call(tailCall, process.check(blockClosure.embedded.value), .{ pc, sp + 2, process, context, extra });
        }
        if (True.equals(v)) {
            sp[2] = sp[0];
            return @call(tailCall, process.check(blockClosure.embedded.value), .{ pc, sp + 2, process, context, extra });
        }
        return @call(tailCall, process.check(fallback), .{ pc, sp, process, context, extra });
    }
};
const dnu = execute.controlPrimitives.dnu;
pub const primitives = struct {
    pub fn p60(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // at:
        _ = .{ pc, sp, process, context, extra };
        unreachable;
    }
    pub fn p61(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // at:
        _ = .{ pc, sp, process, context, extra };
        unreachable;
    }
    pub fn p71(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // at:
        _ = .{ pc, sp, process, context, extra };
        unreachable;
    }
};
// test "ifTrue:" {
//     const expectEqual = std.testing.expectEqual;
//     var prog = compileMethod(sym.value,0,0,.{
//         &e.pushLiteral,Object.from(3),
//         &e.pushLiteral,Object.from(4),
//         &e.p169,
//         &e.ifTrue,"true",
//         &e.pushLiteral,Object.from(17),
//         &e.branch,"common",
//         ":true",
//         &e.pushLiteral,Object.from(42),
//         ":common", &e.returnNoContext,
//     });
//     try expectEqual(testExecute(prog.asCompiledMethodPtr())[0].toInt(),42);
// }
