const std = @import("std");
const execute = @import("../execute.zig");
const trace = execute.trace;
const Context = execute.Context;
const ContextPtr = *Context;
const Code = execute.Code;
const tailCall = execute.tailCall;
const compileMethod = execute.compileMethod;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const Process = @import("../process.zig").Process;
const object = @import("../zobject.zig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const Sym = @import("../symbol.zig").symbols;
const heap = @import("../heap.zig");
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
    pub fn @"ifTrue:"(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const v = sp[1];
        if (True.equals(v)) {
            sp[1] = sp[0];
            return @call(tailCall, blockClosure.embedded.value, .{ pc, sp + 1, process, context, selector });
        }
        if (False.equals(v)) return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, selector });
        return @call(tailCall, fallback, .{ pc + 1, sp, process, context, Sym.@"mustBeBoolean:" });
    }
    pub fn @"ifFalse:"(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const v = sp[1];
        if (False.equals(v)) {
            sp[1] = sp[0];
            return @call(tailCall, blockClosure.embedded.value, .{ pc, sp + 1, process, context, selector });
        }
        if (True.equals(v)) return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, selector });
        return @call(tailCall, fallback, .{ pc + 1, sp, process, context, Sym.@"mustBeBoolean:" });
    }
    pub fn @"ifTrue:ifFalse:"(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const v = sp[2];
        if (True.equals(v)) {
            sp[2] = sp[1];
            return @call(tailCall, blockClosure.embedded.value, .{ pc, sp + 2, process, context, selector });
        }
        if (False.equals(v)) {
            sp[2] = sp[0];
            return @call(tailCall, blockClosure.embedded.value, .{ pc, sp + 2, process, context, selector });
        }
        return @call(tailCall, fallback, .{ pc + 1, sp, process, context, Sym.@"mustBeBoolean:with:" });
    }
    pub fn @"ifFalse:ifTrue:"(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const v = sp[2];
        if (False.equals(v)) {
            sp[2] = sp[1];
            return @call(tailCall, blockClosure.embedded.value, .{ pc, sp + 2, process, context, selector });
        }
        if (True.equals(v)) {
            sp[2] = sp[0];
            return @call(tailCall, blockClosure.embedded.value, .{ pc, sp + 2, process, context, selector });
        }
        return @call(tailCall, fallback, .{ pc + 1, sp, process, context, Sym.@"mustBeBoolean:with:" });
    }
};
const dnu = execute.controlPrimitives.dnu;
pub const primitives = struct {
    pub fn p60(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // at:
        _ = .{ pc, sp, process, context, selector };
        unreachable;
    }
    pub fn p61(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // at:
        _ = .{ pc, sp, process, context, selector };
        unreachable;
    }
    pub fn p71(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // at:
        _ = .{ pc, sp, process, context, selector };
        unreachable;
    }
    pub fn p110(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // ProtoObject>>#==
        if (!Sym.@"==".hashEquals(selector)) return @call(tailCall, dnu, .{ pc, sp, process, context, selector });
        sp[1] = Object.from(inlines.p110(sp[1], sp[0]));
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, selector });
    }
    pub fn p145(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // atAllPut:
        if (!Sym.@"atAllPut:".hashEquals(selector)) return @call(tailCall, dnu, .{ pc, sp, process, context, selector });
        inlines.p1(sp[0]) catch
            return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector });
        return @call(tailCall, context.npc, .{ context.tpc, sp + 1, process, context, selector });
    }
    pub fn p169(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // ProtoObject>>#~~
        if (!Sym.@"~~".hashEquals(selector)) return @call(tailCall, dnu, .{ pc, sp, process, context, selector });
        sp[1] = Object.from(inlines.p169(sp[1], sp[0]));
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, selector });
    }
    // pub inline fn p111(pc: [*]const Code, sp: [*]Object, heap: Hp, rpc: [*]const Code, process: *Process, caller: Context) Object { // ProtoObject>>class
};
const e = struct {
    usingnamespace execute.controlPrimitives;
    usingnamespace embedded;
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
