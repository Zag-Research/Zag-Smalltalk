const std = @import("std");
const config = @import("../config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const execute = @import("../execute.zig");
const SendCache = execute.SendCache;
const Context = execute.Context;
const ContextPtr = *Context;
const Code = execute.Code;
const PC = execute.PC;
const SP = execute.SP;
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
const MinSmallInteger: i64 = object.MinSmallInteger;
const MaxSmallInteger: i64 = object.MaxSmallInteger;

pub fn init() void {}

pub const inlines = struct {
    pub inline fn p60(self: Object, other: Object) !Object { // basicAt:
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub inline fn p61(self: Object, other: Object) !Object { // basicAt:put:
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub inline fn p70(self: Object, other: Object) !Object { // basicNew
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub inline fn p71(self: Object, other: Object) !Object { // basicNew:
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub inline fn p110(self: Object, other: Object) bool { // Identical - can't fail
        return self.equals(other);
    }
    pub inline fn p145(self: Object, other: Object) !Object { // atAllPut:
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub inline fn p169(self: Object, other: Object) bool { // NotIdentical - can't fail
        return !self.equals(other);
    }
};
const fallback = execute.fallback;
pub const embedded = struct {
    pub fn @"basicAt:"(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP {
        sp[1] = inlines.p60(sp[1], sp[0]) catch return @call(tailCall, fallback, .{ pc, sp, process, context, Sym.@"basicAt:" });
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, selector, cache });
    }
    pub fn @"basicAt:put:"(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP {
        sp[1] = inlines.p61(sp[1], sp[0]) catch return @call(tailCall, fallback, .{ pc, sp, process, context, Sym.@"basicAt:put:" });
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, selector, cache });
    }
    pub fn @"=="(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP {
        return @call(tailCall, pc.prim, .{ pc.next(), sp.dropPut(Object.from(inlines.p110(sp.next, sp.top))), process, context, selector, cache });
    }
    pub fn @"~~"(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP {
        return @call(tailCall, pc.prim, .{ pc.next(), sp.dropPut(Object.from(inlines.p169(sp.next, sp.top))), process, context, selector, cache });
    }
    pub fn @"atAllPut:"(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP {
        const newSp = sp.dropPut(inlines.p145(sp[1], sp[0]) catch return @call(tailCall, fallback, .{ pc, sp, process, context, Sym.@"atAllPut:" }));
        return @call(tailCall, pc.prim, .{ pc.next(), newSp, process, context, selector, cache });
    }
};
const dnu = execute.controlPrimitives.dnu;
pub const primitives = struct {
    pub fn p60(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP { // basicAt:
        _ = .{ pc, sp, process, context, selector, cache };
        unreachable;
    }
    pub fn p61(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP { // basicAt:put:
        _ = .{ pc, sp, process, context, selector, cache };
        unreachable;
    }
    pub fn p70(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP { // basicNew
        _ = .{ pc, sp, process, context, selector, cache };
        unreachable;
    }
    pub fn p71(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP { // basicNew:
        _ = .{ pc, sp, process, context, selector, cache };
        unreachable;
    }
    pub fn p83(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP { // perform: perform:with: perform:with:with: perform:with:with:with:
        _ = .{ pc, sp, process, context, selector, cache };
        unreachable;
    }
    pub fn p84(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP { // perform:withArguments:
        _ = .{ pc, sp, process, context, selector, cache };
        unreachable;
    }
    pub fn p100(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP { // perform:withArguments:inSuperclass:
        _ = .{ pc, sp, process, context, selector, cache };
        unreachable;
    }
    pub fn p110(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP { // ProtoObject>>#==
        if (!Sym.@"==".hashEquals(selector)) return @call(tailCall, dnu, .{ pc, sp, process, context, selector, cache });
        sp[1] = Object.from(inlines.p110(sp[1], sp[0]));
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, selector, cache });
    }
    pub fn p145(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP { // atAllPut:
        if (!Sym.@"atAllPut:".hashEquals(selector)) return @call(tailCall, dnu, .{ pc, sp, process, context, selector, cache });
        inlines.p1(sp[0]) catch
            return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
        return @call(tailCall, context.npc, .{ context.tpc, sp + 1, process, context, selector, cache });
    }
    pub fn p169(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP { // ProtoObject>>#~~
        if (!Sym.@"~~".hashEquals(selector)) return @call(tailCall, dnu, .{ pc, sp, process, context, selector, cache });
        sp[1] = Object.from(inlines.p169(sp[1], sp[0]));
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, selector, cache });
    }
    // pub inline fn p111(pc: PC, sp: SP, heap: Hp, rpc: PC, process: *Process, caller: Context) Object { // ProtoObject>>class
};
const e = struct {
    usingnamespace execute.controlPrimitives;
    usingnamespace embedded;
};
test "simple ==" {
    const expect = std.testing.expect;
    var prog = compileMethod(Sym.value, 0, 0, .{
        &e.pushLiteral, Object.from(4),
        &e.pushLiteral, Object.from(4),
        &e.@"==",       &e.returnNoContext,
    });
    const result = testExecute(prog.asCompiledMethodPtr());
    try expect(result[0].to(bool));
}
fn testExecute(method: CompiledMethodPtr) []Object {
    var te = execute.TestExecution.new();
    te.init();
    var objs = [_]Object{};
    var result = te.run(objs[0..], method);
    return result;
}
test "simple compare" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(Sym.value, 0, 0, .{
        &e.pushLiteral, Object.from(3),
        &e.pushLiteral, Object.from(4),
        &e.@"==",       &e.returnNoContext,
    });
    try expectEqual(testExecute(prog.asCompiledMethodPtr())[0], False);
}
test "simple compare and don't branch" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(Sym.value, 0, 0, .{
        &e.pushLiteral,  Object.from(3),
        &e.pushLiteral,  Object.from(4),
        &e.@"==",        &e.ifTrue,
        "true",          &e.pushLiteral,
        Object.from(17), &e.branch,
        "common",        ":true",
        &e.pushLiteral,  Object.from(42),
        ":common",       &e.returnNoContext,
    });
    try expectEqual(testExecute(prog.asCompiledMethodPtr())[0].toInt(), 17);
}
test "simple compare and branch" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(Sym.value, 0, 0, .{
        &e.pushLiteral,  Object.from(3),
        &e.pushLiteral,  Object.from(4),
        &e.@"~~",        &e.ifTrue,
        "true",          &e.pushLiteral,
        Object.from(17), &e.branch,
        "common",        ":true",
        &e.pushLiteral,  Object.from(42),
        ":common",       &e.returnNoContext,
    });
    try expectEqual(testExecute(prog.asCompiledMethodPtr())[0].toInt(), 42);
}

test "dispatch3" {}
