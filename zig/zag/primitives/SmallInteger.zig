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
const fromPrimitive = execute.Signature.fromPrimitive;
const Process = zag.Process;
const object = zag.object;
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const Sym = zag.symbol.symbols;
const signature = zag.symbol.signature;
const heap = zag.heap;
const primitives = zag.primitives;
const empty = &[0]Object{};
const tf = zag.threadedFn.Enum;

pub const moduleName = "SmallInteger";
pub fn init() void {}
pub const inlines = struct {
    pub inline fn negated(self: Object, sp: SP, context: *Context) !Object { // Negate
        const result, const overflow = @subWithOverflow(Object.tagged0, self.untaggedI_noCheck());
        if (overflow == 0) return Object.fromTaggedI(result, sp, context);
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
    pub inline fn @"*"(self: Object, other: Object, sp: SP, context: *Context) !Object { // Multiply
        if (other.nativeI()) |untagged| {
            const result, const overflow = @mulWithOverflow(self.untaggedI_noCheck(), untagged);
            if (overflow == 0) return Object.fromUntaggedI(result, sp, context);
        }
        return error.primitiveError;
    }
};

const expectEqual = std.testing.expectEqual;
test "inline primitives" {
    var process: Process align(Process.alignment) = undefined;
    process.init();
    const sp = process.getSp();
    const context = process.getContext();
    try expectEqual(Object.from(12, sp, context), inlines.@"*"(Object.from(3, sp, context), Object.from(4, sp, context), sp, context));
    try expectEqual(error.primitiveError, inlines.@"*"(Object.from(0x1_0000_0000, sp, context), Object.from(0x100_0000, sp, context), sp, context));
    try expectEqual(error.primitiveError, inlines.@"*"(Object.from(0x1_0000_0000, sp, context), Object.from(0x80_0000, sp, context), sp, context));
//    try expectEqual(Object.from(-0x80_0000_0000_0000, sp, context), inlines.@"*"(Object.from(0x1_0000_0000, sp, context), Object.from(-0x80_0000, sp, context), sp, context));
//    try expectEqual(Object.from(0x20_0000_0000_0000, sp, context), inlines.@"*"(Object.from(0x1_0000_0000, sp, context), Object.from(0x20_0000, sp, context), sp, context));
//    try expectEqual(Object.from(0x3f_ffff_0000_0000, sp, context), inlines.@"*"(Object.from(0x1_0000_0000, sp, context), Object.from(0x3f_ffff, sp, context), sp, context));
    try expectEqual(Object.from(0, sp, context), inlines.negated(Object.from(0, sp, context), sp, context));
    try expectEqual(Object.from(-42, sp, context), inlines.negated(Object.from(42, sp, context), sp, context));
//    try expectEqual(Object.from(0x7f_ffff_ffff_ffff, sp, context), inlines.negated(Object.from(-0x7f_ffff_ffff_ffff, sp, context), sp, context));
//    try expectEqual(Object.from(-0x7f_ffff_ffff_ffff, sp, context), inlines.negated(Object.from(0x7f_ffff_ffff_ffff, sp, context), sp, context));
//    try expectEqual(error.primitiveError, inlines.negated(Object.from(-0x80_0000_0000_0000, sp, context), sp, context));
    try expectEqual(true, try inlines.@">="(Object.from(0, sp, context), Object.from(0, sp, context)));
    try expectEqual(false, try inlines.@">="(Object.from(0, sp, context), Object.from(1, sp, context)));
    try expectEqual(true, try inlines.@">="(Object.from(1, sp, context), Object.from(0, sp, context)));
}
pub const @"+" = struct {
    pub const number = 1;
    pub const inlined = signature(.@"+", number);
    inline fn with(self: Object, other: Object, sp: SP, context: *Context) !Object { // INLINED - Add
        if (other.untaggedI()) |untagged| {
            const result, const overflow = @addWithOverflow(self.taggedI_noCheck(), untagged);
            if (overflow == 0) return Object.fromTaggedI(result, sp, context);
        }
        return error.primitiveError;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#+
        const newSp = sp.dropPut(with(sp.next, sp.top, sp, context) catch
            return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
        return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
    }
    test "simple add" {
        var exe = Execution.initTest("simple add", .{ tf.primitive, comptime fromPrimitive(1) });
        try exe.runTest(
            &[_]Object{
                exe.object(25),
                exe.object(17),
            },
            &[_]Object{
                exe.object(42),
            },
        );
    }
    test "simple add with overflow" {
        var exe = Execution.initTest("simple add with overflow", .{ tf.primitive, comptime fromPrimitive(1), tf.pushLiteral, object.testObjects[0] });
        try exe.runTest(
            &[_]Object{
                exe.object(4),
                exe.object(Object.maxInt),
            },
            &[_]Object{
                object.testObjects[0],
                exe.object(4),
                exe.object(Object.maxInt),
            },
        );
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        sp.traceStack("+", context, extra);
        const receiver = sp.next;
        if (!receiver.isInt()) {
            trace("SmallInteger>>#inlinePrimitive: + {f}", .{receiver});
            if (true) @panic("unreachable");
            return @call(tailCall, primitives.inlinePrimitiveFailed, .{ pc, sp, process, context, extra });
        }
        const newSp = sp.dropPut(with(receiver, sp.top, sp, context) catch
            return @call(tailCall, primitives.inlinePrimitiveFailed, .{ pc, sp, process, context, extra }));
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
    }
};
pub const @"-" = struct {
    pub const number = 2;
    pub const inlined = signature(.@"-", number);
    inline fn with(self: Object, other: Object, sp: SP, context: *Context) !Object { // Subtract
        if (other.untaggedI()) |untagged| {
            const result, const overflow = @subWithOverflow(self.taggedI_noCheck(), untagged);
            if (overflow == 0) return Object.fromTaggedI(result, sp, context);
        }
        return error.primitiveError;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#-
        const newSp = sp.dropPut(with(sp.next, sp.top, sp, context) catch
            return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
        return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        sp.traceStack("-", context, extra);
        const receiver = sp.next;
        if (!receiver.isInt()) {
            std.log.err("SmallInteger>>#inlinePrimitive: -y {f}", .{sp});
            return @call(tailCall, primitives.inlinePrimitiveFailed, .{ pc, sp, process, context, extra });
        }
        const newSp = sp.dropPut(with(receiver, sp.top, sp, context) catch {
            std.log.err("SmallInteger>>#inlinePrimitive: -x {f}", .{sp});
            return @call(tailCall, primitives.inlinePrimitiveFailed, .{ pc, sp, process, context, extra });
        });
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
    }
};
pub const @"<=" = struct {
    pub const number = 5;
    pub const inlined = signature(.@"<=", number);
    pub inline fn with(self: Object, other: Object) !bool { // LessOrEqual
        if (other.taggedI()) |tagged|
            return self.taggedI_noCheck() <= tagged;
        return error.primitiveError;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#<=
        const newSp = sp.dropPut(Object.from(with(sp.next, sp.top) catch
            return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }), sp, context));
        return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        sp.traceStack("<=", context, extra);
        const receiver = sp.next;
        if (!receiver.isInt()) {
            trace("SmallInteger>>#inlinePrimitive: <= {f}", .{receiver});
            if (true) @panic("unreachable");
            return @call(tailCall, primitives.inlinePrimitiveFailed, .{ pc, sp, process, context, extra });
        }
        const newSp = sp.dropPut(Object.from(with(receiver, sp.top) catch
            return @call(tailCall, primitives.inlinePrimitiveFailed, .{ pc, sp, process, context, extra }), sp, context));
        trace("Inline <= called, {*} {f}", .{ newSp, extra });
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
    }
    test "inline primitives" {
        var process: Process align(Process.alignment) = undefined;
        process.init();
        const sp = process.getSp();
        const context = process.getContext();
        try expectEqual(true, try with(Object.from(0, sp, context), Object.from(0, sp, context)));
        try expectEqual(true, try with(Object.from(0, sp, context), Object.from(1, sp, context)));
        try expectEqual(false, try with(Object.from(1, sp, context), Object.from(0, sp, context)));
    }
};
pub const @"*" = struct {
    pub const number = 9;
    pub const inlined = signature(.@"*", number);
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#*
        const newSp = sp.dropPut(Object.from(inlines.@"*"(sp.next, sp.top, sp, context) catch
            return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }), sp, context));
        return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const receiver = sp.next;
        if (!receiver.isInt()) {
            trace("SmallInteger>>#inlinePrimitive: * {f}", .{receiver});
            return @call(tailCall, primitives.inlinePrimitiveFailed, .{ pc, sp, process, context, extra });
        }
        const newSp = sp.dropPut(inlines.@"*"(receiver, sp.top, sp, context) catch
            return @call(tailCall, primitives.inlinePrimitiveFailed, .{ pc, sp, process, context, extra }));
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
    }
};
pub const threadedFns = struct {
    pub const countDown = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        var result = True();
        if (sp.top.untaggedI()) |value| {
            const sum, const overflow = @addWithOverflow(Object.asUntaggedI(-1), value);
            if (overflow == 0) {
                sp.top = Object.fromUnTaggedI(sum, sp, context);
                if (sum > 0) result = False();
            }
        }
        if (sp.push(result)) |newSp| {
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, context, extra });
        } else {
            const newSp, const newContext, const newExtra = sp.spillStackAndPush(result, context, extra);
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, newContext, newExtra });
        }
    }
    test "countDown" {
        var exe = Execution.initTest("countDown", .{ tf.countDown, tf.pushLiteral, "0One", tf.countDown , tf.pushLiteral, "1Neg", tf.countDown , tf.countDown });
        try exe.resolve(&[_]Object{Object.fromNativeI(1, null, null), Object.fromNativeI(-5, null, null)});
        try exe.runTest(
            &[_]Object{
                exe.object(42),
            },
            &[_]Object{
                exe.object(true),
                exe.object(0),
                exe.object(false),
                exe.object(41),
            },
        );
        return error.TestFailed;
    }
};
};
