const std = @import("std");
const config = @import("../config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const execute = @import("../execute.zig");
const TFProcess = execute.TFProcess;
const TFContext = execute.TFContext;
const MethodSignature = execute.MethodSignature;
const Code = execute.Code;
const PC = execute.PC;
const SP = execute.SP;
const compileMethod = execute.compileMethod;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const Process = @import("../process.zig").Process;
const object = @import("../zobject.zig");
const Object = object.Object;
const empty = Object.empty;
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
    pub fn @"basicAt:"(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        sp[1] = inlines.p60(sp[1], sp[0]) catch return @call(tailCall, fallback, .{ pc, sp, process, context, Sym.@"basicAt:" });
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, undefined, undefined });
    }
    pub fn @"basicAt:put:"(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        sp[1] = inlines.p61(sp[1], sp[0]) catch return @call(tailCall, fallback, .{ pc, sp, process, context, Sym.@"basicAt:put:" });
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, undefined, undefined });
    }
    pub fn p110(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP { // ProtoObject>>#==
        return @call(tailCall, pc.prim(), .{ pc.next(), sp.dropPut(Object.from(inlines.p110(sp.next, sp.top))), process, context, undefined });
    }
    pub fn p169(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP { // ProtoObject>>#~~
        return @call(tailCall, pc.prim(), .{ pc.next(), sp.dropPut(Object.from(inlines.p169(sp.next, sp.top))), process, context, undefined });
    }
    // pub inline fn p111(pc: PC, sp: SP, heap: Hp, rpc: PC, process: TFProcess, caller: Context) Object { // ProtoObject>>class
    pub fn p145(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP { // atAllPut:
        const newSp = sp.dropPut(inlines.p145(sp[1], sp[0]) catch return @call(tailCall, fallback, .{ pc, sp, process, context, Sym.@"atAllPut:" }));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
};
pub const primitives = struct {
    pub fn p60(pc: PC, sp: SP, process: TFProcess, context: TFContext, selector: MethodSignature) callconv(stdCall) SP { // basicAt:
        if (pc.verifyMethod(selector)) return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector });
        unreachable;
    }
    pub fn p61(pc: PC, sp: SP, process: TFProcess, context: TFContext, selector: MethodSignature) callconv(stdCall) SP { // basicAt:put:
        if (pc.verifyMethod(selector)) return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector });
        unreachable;
    }
    pub fn p83(pc: PC, sp: SP, process: TFProcess, context: TFContext, selector: MethodSignature) callconv(stdCall) SP { // perform: perform:with: perform:with:with: perform:with:with:with:
        if (pc.verifyMethod(selector)) return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector });
        unreachable;
    }
    pub fn p84(pc: PC, sp: SP, process: TFProcess, context: TFContext, selector: MethodSignature) callconv(stdCall) SP { // perform:withArguments:
        if (pc.verifyMethod(selector)) return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector });
        unreachable;
    }
    pub fn p100(pc: PC, sp: SP, process: TFProcess, context: TFContext, selector: MethodSignature) callconv(stdCall) SP { // perform:withArguments:inSuperclass:
        if (pc.verifyMethod(selector)) return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector });
        unreachable;
    }
    pub fn p110(pc: PC, sp: SP, process: TFProcess, context: TFContext, selector: MethodSignature) callconv(stdCall) SP { // ProtoObject>>#==
        if (pc.verifyMethod(selector)) return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector });
        sp[1] = Object.from(inlines.p110(sp[1], sp[0]));
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, undefined });
    }
    pub fn p145(pc: PC, sp: SP, process: TFProcess, context: TFContext, selector: MethodSignature) callconv(stdCall) SP { // atAllPut:
        if (pc.verifyMethod(selector)) return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector });
        inlines.p1(sp[0]) catch
            return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, undefined });
        return @call(tailCall, context.npc, .{ context.tpc, sp + 1, process, context, undefined });
    }
    pub fn p169(pc: PC, sp: SP, process: TFProcess, context: TFContext, selector: MethodSignature) callconv(stdCall) SP { // ProtoObject>>#~~
        if (pc.verifyMethod(selector)) return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector });
        sp[1] = Object.from(inlines.p169(sp[1], sp[0]));
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, undefined });
    }
    // pub inline fn p111(pc: PC, sp: SP, heap: Hp, rpc: PC, process: TFProcess, caller: Context) Object { // ProtoObject>>class
};
const e = struct {
    usingnamespace execute.controlPrimitives;
    usingnamespace embedded;
};
test "simple ==" {
    const expect = std.testing.expect;
    var prog = compileMethod(Sym.value, 0, 0, .Object, .{
        &e.pushLiteral, Object.from(4),
        &e.pushLiteral, Object.from(4),
        &e.p110,        &e.returnNoContext,
    });
    const result = testExecute(&prog);
    try expect(result[0].to(bool));
}
fn testExecute(ptr: anytype) []Object {
    const method: CompiledMethodPtr = @ptrCast(ptr);
    var te = execute.Execution.new();
    te.init();
    var objs = [_]Object{};
    const result = te.run(objs[0..], method);
    return result;
}
test "simple compare" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(Sym.value, 0, 0, .Object, .{
        &e.pushLiteral, Object.from(3),
        &e.pushLiteral, Object.from(4),
        &e.p110,        &e.returnNoContext,
    });
    try expectEqual(testExecute(&prog)[0], False);
}
test "simple compare and don't branch" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(Sym.value, 0, 0, .Object, .{
        &e.pushLiteral,  Object.from(3),
        &e.pushLiteral,  Object.from(4),
        &e.p110,         &e.ifTrue,
        "true",          &e.pushLiteral,
        Object.from(17), &e.branch,
        "common",        ":true",
        &e.pushLiteral,  Object.from(42),
        ":common",       &e.returnNoContext,
    });
    try expectEqual(testExecute(&prog)[0].toInt(), 17);
}
test "simple compare and branch" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(Sym.value, 0, 0, .Object, .{
        &e.pushLiteral,  Object.from(3),
        &e.pushLiteral,  Object.from(4),
        &e.p169,         &e.ifTrue,
        "true",          &e.pushLiteral,
        Object.from(17), &e.branch,
        "common",        ":true",
        &e.pushLiteral,  Object.from(42),
        ":common",       &e.returnNoContext,
    });
    try expectEqual(testExecute(&prog)[0].toInt(), 42);
}

test "dispatch3" {}
