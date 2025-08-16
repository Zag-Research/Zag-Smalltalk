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

pub const moduleName = "SmallInteger";
pub fn init() void {}
pub const inlines = struct {
    pub fn @"+"(self: Object, other: Object, maybeProcess: ?*Process) !Object { // INLINED - Add
        if (other.untaggedI()) |untagged| {
            const result, const overflow = @addWithOverflow(self.taggedI_noCheck(), untagged);
            if (overflow == 0) return Object.fromTaggedI(result, maybeProcess);
        }
        return error.primitiveError;
    }
    pub inline fn negated(self: Object, maybeProcess: ?*Process) !Object { // Negate
        const result, const overflow = @subWithOverflow(Object.tagged0, self.untaggedI_noCheck());
        if (overflow == 0) return Object.fromTaggedI(result, maybeProcess);
        return error.primitiveError;
    }
    pub inline fn @"-"(self: Object, other: Object, maybeProcess: ?*Process) !Object { // Subtract
        if (other.untaggedI()) |untagged| {
            const result, const overflow = @subWithOverflow(self.taggedI_noCheck(), untagged);
            if (overflow == 0) return Object.fromTaggedI(result, maybeProcess);
        }
        return error.primitiveError;
    }
    pub inline fn @"<"(self: Object, other: Object) !bool { // LessThan
        if (other.taggedI()) |tagged|
            return self.taggedI_noCheck() < tagged;
        return error.primitiveError;
    }
    pub inline fn @">"(self: Object, other: Object) !bool { // GreaterThan
        if (other.taggedI()) |tagged|
            return self.taggedI_noCheck() > tagged;
        return error.primitiveError;
    }
    pub inline fn @"<="(self: Object, other: Object) !bool { // LessOrEqual
        if (other.taggedI()) |tagged|
            return self.taggedI_noCheck() <= tagged;
        return error.primitiveError;
    }
    pub inline fn @">="(self: Object, other: Object) !bool { // GreaterOrEqual
        if (other.taggedI()) |tagged|
            return self.taggedI_noCheck() >= tagged;
        return error.primitiveError;
    }
    pub inline fn @"="(self: Object, other: Object) !bool { // Equal
        if (other.taggedI()) |tagged|
            return self.taggedI_noCheck() == tagged;
        return error.primitiveError;
    }
    pub inline fn @"<>"(self: Object, other: Object) !bool { // NotEqual
        if (other.taggedI()) |tagged|
            return self.taggedI_noCheck() != tagged;
        return error.primitiveError;
    }
    pub inline fn @"*"(self: Object, other: Object, maybeProcess: ?*Process) !Object { // Multiply
        if (other.nativeI()) |untagged| {
            const result, const overflow = @mulWithOverflow(self.untaggedI_noCheck(), untagged);
            if (overflow == 0) return Object.fromUntaggedI(result, maybeProcess);
        }
        return error.primitiveError;
    }
};

test "inline primitives" {
    try config.skipNotZag();
    const expectEqual = std.testing.expectEqual;
    try expectEqual(Object.from(12, null), inlines.@"*"(Object.from(3, null), Object.from(4, null), null));
    try expectEqual(error.primitiveError, inlines.@"*"(Object.from(0x1_0000_0000, null), Object.from(0x100_0000, null), null));
    try expectEqual(error.primitiveError, inlines.@"*"(Object.from(0x1_0000_0000, null), Object.from(0x80_0000, null), null));
    try expectEqual(Object.from(-0x80_0000_0000_0000, null), inlines.@"*"(Object.from(0x1_0000_0000, null), Object.from(-0x80_0000, null), null));
    try expectEqual(Object.from(0x20_0000_0000_0000, null), inlines.@"*"(Object.from(0x1_0000_0000, null), Object.from(0x20_0000, null), null));
    try expectEqual(Object.from(0x3f_ffff_0000_0000, null), inlines.@"*"(Object.from(0x1_0000_0000, null), Object.from(0x3f_ffff, null), null));
    try expectEqual(Object.from(0, null), inlines.negated(Object.from(0, null), null));
    try expectEqual(Object.from(-42, null), inlines.negated(Object.from(42, null), null));
    try expectEqual(Object.from(0x7f_ffff_ffff_ffff, null), inlines.negated(Object.from(-0x7f_ffff_ffff_ffff, null), null));
    try expectEqual(Object.from(-0x7f_ffff_ffff_ffff, null), inlines.negated(Object.from(0x7f_ffff_ffff_ffff, null), null));
    try expectEqual(error.primitiveError, inlines.negated(Object.from(-0x80_0000_0000_0000, null), null));
    try expectEqual(true, try inlines.@"<="(Object.from(0, null), Object.from(0, null)));
    try expectEqual(true, try inlines.@"<="(Object.from(0, null), Object.from(1, null)));
    try expectEqual(false, try inlines.@"<="(Object.from(1, null), Object.from(0, null)));
    try expectEqual(true, try inlines.@">="(Object.from(0, null), Object.from(0, null)));
    try expectEqual(false, try inlines.@">="(Object.from(0, null), Object.from(1, null)));
    try expectEqual(true, try inlines.@">="(Object.from(1, null), Object.from(0, null)));
}
pub const @"+" = struct {
    pub const number = 1;
    pub const inlined = rawSymbol(.@"+", number);
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#+
        const newSp = sp.dropPut(inlines.@"+"(sp.next, sp.top, process) catch
            return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
        return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, unreachable });
    }
    test "simple add" {
        try Execution.runTest(
            "simple add",
            .{ tf.primitive, 1 },
            &[_]Object{
                Object.from(25, null),
                Object.from(17, null),
            },
            &[_]Object{
                Object.from(42, null),
            },
        );
    }
    test "simple add with overflow" {
        try Execution.runTest(
            "simple add with overflow",
            .{ tf.primitive, 1, tf.pushLiteral, 42 },
            &[_]Object{
                Object.from(4, null),
                Object.from(Object.maxInt, null),
            },
            &[_]Object{
                Object.from(42, null),
                Object.from(4, null),
                Object.from(Object.maxInt, null),
            },
        );
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const receiver = sp.next;
        if (!receiver.isInt()) {
            trace("SmallInteger>>#inlinePrimitive: {f}", .{ receiver });
            if (true) unreachable;
            return @call(tailCall, PC.inlinePrimitiveFailed, .{ pc, sp, process, context, extra });
        }
        const newSp = sp.dropPut(inlines.@"+"(receiver, sp.top, process) catch
            return @call(tailCall, PC.inlinePrimitiveFailed, .{ pc, sp, process, context, extra }));
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
    }
};
pub const @"-" = struct {
    pub const number = 2;
    pub const inlined = rawSymbol(.@"-", number);
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#-
        const newSp = sp.dropPut(inlines.@"-"(sp.next, sp.top, process) catch
            return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
        return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, unreachable });
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const receiver = sp.next;
        if (!receiver.isInt()) {
            return @call(tailCall, PC.inlinePrimitiveFailed, .{ pc, sp, process, context, extra });
        }
        const newSp = sp.dropPut(inlines.@"-"(receiver, sp.top, process) catch
            return @call(tailCall, PC.inlinePrimitiveFailed, .{ pc, sp, process, context, extra }));
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
    }
};
pub const @"<=" = struct {
    pub const number = 5;
    pub const inlined = rawSymbol(.@"<=", number);
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#<=
        const newSp = sp.dropPut(Object.from(inlines.@"<="(sp.next, sp.top) catch
            return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }), null));
        return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, unreachable });
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const receiver = sp.next;
        if (!receiver.isInt()) {
            trace("SmallInteger>>#inlinePrimitive: <= {f}\n", .{ receiver });
            if (true) unreachable;
            return @call(tailCall, PC.inlinePrimitiveFailed, .{ pc, sp, process, context, extra });
        }
        const newSp = sp.dropPut(Object.from(inlines.@"<="(receiver, sp.top) catch
            return @call(tailCall, PC.inlinePrimitiveFailed, .{ pc, sp, process, context, extra }), null));
        trace("Inline <= called, {*} {f}\n", .{ newSp, extra });
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
    }
};
pub const @"*" = struct {
    pub const number = 9;
    pub const inlined = rawSymbol(.@"*", number);
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#*
        const newSp = sp.dropPut(Object.from(inlines.@"*"(sp.next, sp.top, process) catch
            return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }), null));
        return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, unreachable });
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const receiver = sp.next;
        if (!receiver.isInt()) {
            return @call(tailCall, PC.inlinePrimitiveFailed, .{ pc, sp, process, context, extra });
        }
        const newSp = sp.dropPut(inlines.@"*"(receiver, sp.top, process) catch
            return @call(tailCall, PC.inlinePrimitiveFailed, .{ pc, sp, process, context, extra }));
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
    }
};
