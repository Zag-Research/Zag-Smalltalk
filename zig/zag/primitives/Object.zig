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
    pub fn @"basicAt:"(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        sp[1] = inlines.p60(sp[1], sp[0]) catch return @call(tailCall, fallback, .{ pc, sp, process, context, Sym.@"basicAt:" });
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, selector });
    }
    pub fn @"basicAt:put:"(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        sp[1] = inlines.p61(sp[1], sp[0]) catch return @call(tailCall, fallback, .{ pc, sp, process, context, Sym.@"basicAt:put:" });
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, selector });
    }
    pub fn @"=="(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        sp[1] = Object.from(inlines.p110(sp[1], sp[0]));
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, selector });
    }
    pub fn @"~~"(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        sp[1] = Object.from(inlines.p169(sp[1], sp[0]));
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, selector });
    }
    pub fn @"atAllPut:"(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        sp[1] = inlines.p145(sp[1], sp[0]) catch return @call(tailCall, fallback, .{ pc, sp, process, context, Sym.@"atAllPut:" });
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, selector });
    }
};
const dnu = execute.controlPrimitives.dnu;
pub const primitives = struct {
    pub fn p60(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // basicAt:
        _ = .{ pc, sp, process, context, selector };
        unreachable;
    }
    pub fn p61(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // basicAt:put:
        _ = .{ pc, sp, process, context, selector };
        unreachable;
    }
    pub fn p70(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // basicNew
        _ = .{ pc, sp, process, context, selector };
        unreachable;
    }
    pub fn p71(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // basicNew:
        _ = .{ pc, sp, process, context, selector };
        unreachable;
    }
    pub fn p83(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // perform: perform:with: perform:with:with: perform:with:with:with:
        _ = .{ pc, sp, process, context, selector };
        unreachable;
    }
    pub fn p84(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // perform:withArguments:
        _ = .{ pc, sp, process, context, selector };
        unreachable;
    }
    pub fn p100(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // perform:withArguments:inSuperclass:
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
    std.debug.print("result = {any}\n", .{result});
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
