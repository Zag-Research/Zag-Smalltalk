const std = @import("std");
const zag = @import("../zag.zig");
const config = zag.config;
const tailCall = config.tailCall;
const trace = config.trace;
const execute = zag.execute;
const Context = zag.Context;
const Code = execute.Code;
const PC = execute.PC;
const SP = execute.SP;
const Extra = execute.Extra;
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
const heap = zag.heap;
const empty = &[0]Object{};
const tf = zag.threadedFn.Enum;

pub const moduleName = "SmallInteger";
pub fn init() void {}
pub const inlines = struct {
    pub fn p1(self: Object, other: Object) !Object { // INLINED - Add
        if (other.untaggedI()) |untagged| {
            const result, const overflow = @addWithOverflow(self.taggedI_noCheck(), untagged);
            if (overflow == 0) return Object.fromTaggedI(result);
        }
        return error.primitiveError;
    }
    pub inline fn p_negated(self: Object) !Object { // Negate
        const result, const overflow = @subWithOverflow(Object.tagged0, self.untaggedI_noCheck());
        if (overflow == 0) return Object.fromTaggedI(result);
        return error.primitiveError;
    }
    pub inline fn p2(self: Object, other: Object) !Object { // Subtract
        if (other.untaggedI()) |untagged| {
            const result, const overflow = @subWithOverflow(self.taggedI_noCheck(), untagged);
            if (overflow == 0) return Object.fromTaggedI(result);
        }
        return error.primitiveError;
    }
    pub inline fn p3(self: Object, other: Object) !bool { // LessThan
        if (other.taggedI()) |tagged|
            return self.taggedI_noCheck() < tagged;
        return error.primitiveError;
    }
    pub inline fn p4(self: Object, other: Object) !bool { // GreaterThan
        if (other.taggedI()) |tagged|
            return self.taggedI_noCheck() > tagged;
        return error.primitiveError;
    }
    pub inline fn p5(self: Object, other: Object) !bool { // LessOrEqual
        if (other.taggedI()) |tagged|
            return self.taggedI_noCheck() <= tagged;
        return error.primitiveError;
    }
    pub fn p5N(self: Object, other: Object) bool { // INLINED - LessOrEqual when both known SmallIntegers
        return self.taggedI_noCheck() <= other.taggedI_noCheck();
    }
    pub inline fn p6(self: Object, other: Object) !bool { // GreaterOrEqual
        if (other.taggedI()) |tagged|
            return self.taggedI_noCheck() >= tagged;
        return error.primitiveError;
    }
    pub inline fn p7(self: Object, other: Object) !bool { // Equal
        if (other.taggedI()) |tagged|
            return self.taggedI_noCheck() == tagged;
        return error.primitiveError;
    }
    pub inline fn p8(self: Object, other: Object) !bool { // NotEqual
        if (other.taggedI()) |tagged|
            return self.taggedI_noCheck() != tagged;
        return error.primitiveError;
    }
    inline fn unsafeAbs(x: i64) u64 {
        @setRuntimeSafety(false);
        return @as(u64, @intCast(if (x < 0) -x else x));
    }
    pub inline fn p9(self: Object, other: Object) !Object { // Multiply
        if (other.nativeI()) |untagged| {
            const result, const overflow = @mulWithOverflow(self.untaggedI_noCheck(), untagged);
            if (overflow == 0) return Object.fromUntaggedI(result);
        }
        return error.primitiveError;
    }
};

test "inline primitives" {
    if (config.objectEncoding != .zag) return error.SkipZigTest;
    const expectEqual = std.testing.expectEqual;
    try expectEqual(Object.from(12, null), inlines.p9(Object.from(3, null), Object.from(4, null)));
    try expectEqual(error.primitiveError, inlines.p9(Object.from(0x1_0000_0000, null), Object.from(0x100_0000, null)));
    try expectEqual(error.primitiveError, inlines.p9(Object.from(0x1_0000_0000, null), Object.from(0x80_0000, null)));
    try expectEqual(Object.from(-0x80_0000_0000_0000, null), inlines.p9(Object.from(0x1_0000_0000, null), Object.from(-0x80_0000, null)));
    try expectEqual(Object.from(0x20_0000_0000_0000, null), inlines.p9(Object.from(0x1_0000_0000, null), Object.from(0x20_0000, null)));
    try expectEqual(Object.from(0x3f_ffff_0000_0000, null), inlines.p9(Object.from(0x1_0000_0000, null), Object.from(0x3f_ffff, null)));
    try expectEqual(Object.from(0, null), inlines.p_negated(Object.from(0, null)));
    try expectEqual(Object.from(-42, null), inlines.p_negated(Object.from(42, null)));
    try expectEqual(Object.from(0x7f_ffff_ffff_ffff, null), inlines.p_negated(Object.from(-0x7f_ffff_ffff_ffff, null)));
    try expectEqual(Object.from(-0x7f_ffff_ffff_ffff, null), inlines.p_negated(Object.from(0x7f_ffff_ffff_ffff, null)));
    try expectEqual(error.primitiveError, inlines.p_negated(Object.from(-0x80_0000_0000_0000, null)));
    try expectEqual(true, try inlines.p5(Object.from(0, null), Object.from(0, null)));
    try expectEqual(true, try inlines.p5(Object.from(0, null), Object.from(1, null)));
    try expectEqual(false, try inlines.p5(Object.from(1, null), Object.from(0, null)));
    try expectEqual(true, inlines.p5N(Object.from(0, null), Object.from(0, null)));
    try expectEqual(true, inlines.p5N(Object.from(0, null), Object.from(1, null)));
    try expectEqual(false, inlines.p5N(Object.from(1, null), Object.from(0, null)));
    try expectEqual(true, try inlines.p6(Object.from(0, null), Object.from(0, null)));
    try expectEqual(false, try inlines.p6(Object.from(0, null), Object.from(1, null)));
    try expectEqual(true, try inlines.p6(Object.from(1, null), Object.from(0, null)));
}
pub const @"+" = struct {
    pub const number = 1;
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#+
        const newSp = sp.dropPut(inlines.p1(sp.next, sp.top) catch
            return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
        return @call(tailCall, process.check(context.npc.f), .{ context.tpc, newSp, process, context, undefined });
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
                Object.from(0x7f_ffff_ffff_ffff, null),
            },
            &[_]Object{
                Object.from(42, null),
                Object.from(4, null),
                Object.from(0x7f_ffff_ffff_ffff, null),
            },
        );
    }
};
pub const @"-" = struct {
    pub const number = 2;
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#-
        const newSp = sp.dropPut(inlines.p2(sp.next, sp.top) catch
            return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
        return @call(tailCall, process.check(context.npc.f), .{ context.tpc, newSp, process, context, undefined });
    }
};
pub fn p7(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // at:
    _ = .{ pc, sp, process, context, extra, unreachable };
}
pub const @"<=" = struct {
    pub const number = 5;
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#<=
        const newSp = sp.dropPut(Object.from(inlines.p5(sp.next, sp.top) catch
            return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }), null));
        return @call(tailCall, process.check(context.npc.f), .{ pc, newSp, process, context, undefined });
    }
};
pub const @"*" = struct {
    pub const number = 9;
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#*
        const newSp = sp.dropPut(Object.from(inlines.p9(sp.next, sp.top) catch
            return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }), null));
        return @call(tailCall, process.check(context.npc.f), .{ pc, newSp, process, context, undefined });
    }
};
