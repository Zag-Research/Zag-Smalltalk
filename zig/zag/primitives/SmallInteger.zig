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
const expectEqual = std.testing.expectEqual;
pub const @"+" = struct {
    pub const number = 1;
    pub const inlined = signature(.@"+", number);
    inline fn with(self: i64, other: Object, sp: SP, context: *Context) !Object { // INLINED - Add
        if (other.untaggedI()) |untagged| {
            const result, const overflow = @addWithOverflow(self, untagged);
            if (overflow == 0) return Object.fromTaggedI(result, sp, context);
        }
        return error.primitiveError;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#+
        if (sp.next.taggedI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
            return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
        }
        unreachable;
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
                exe.object(Object.maxInt/2),
            },
            &[_]Object{
                object.testObjects[0],
                exe.object(4),
                exe.object(Object.maxInt/2),
            },
        );
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        sp.traceStack("+", context, extra);
        if (sp.next.taggedI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra }));
            return @call(tailCall, process.check(pc.prim3()), .{ pc.next3(), newSp, process, context, extra });
        }
        trace("Float>>#inlinePrimitive: + {f}", .{sp.next});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra });
    }
};
pub const @"-" = struct {
    pub const number = 2;
    pub const inlined = signature(.@"-", number);
    inline fn with(self: i64, other: Object, sp: SP, context: *Context) !Object { // Subtract
        if (other.untaggedI()) |untagged| {
            const result, const overflow = @subWithOverflow(self, untagged);
            if (overflow == 0) return Object.fromTaggedI(result, sp, context);
        }
        return error.primitiveError;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#-
        if (sp.next.taggedI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
            return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
        }
        unreachable;
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        sp.traceStack("-", context, extra);
        if (sp.next.taggedI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra }));
            return @call(tailCall, process.check(pc.prim3()), .{ pc.next3(), newSp, process, context, extra });
        }
        trace("Float>>#inlinePrimitive: - {f}", .{sp.next});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra });
    }
};
pub const @"<=" = struct {
    pub const number = 5;
    pub const inlined = signature(.@"<=", number);
    inline fn with(self: i64, other: Object, sp: SP, context: *Context) !Object { // LessOrEqual
        if (other.taggedI()) |tagged|
            return Object.from(self <= tagged, sp, context);
        return error.primitiveError;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#<=
        if (sp.next.taggedI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
            return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
        }
        unreachable;
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        sp.traceStack("<=", context, extra);
        if (sp.next.taggedI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra }));
            return @call(tailCall, process.check(pc.prim3()), .{ pc.next3(), newSp, process, context, extra });
        }
        trace("Float>>#inlinePrimitive: <= {f}", .{sp.next});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra });
    }
    test "inline primitives" {
        var process: Process align(Process.alignment) = undefined;
        process.init();
        const sp = process.getSp();
        const context = process.getContext();
        try expectEqual(Object.True(), try with(0, Object.from(0, sp, context), sp, context));
        try expectEqual(Object.True(), try with(0, Object.from(1, sp, context), sp, context));
        try expectEqual(Object.False(), try with(0, Object.from(-1, sp, context), sp, context));
    }
};
pub const @"*" = struct {
    pub const number = 9;
    pub const inlined = signature(.@"*", number);
    inline fn with(self: i64, other: Object, sp: SP, context: *Context) !Object { // Multiply
        if (other.nativeI()) |native| {
            const result, const overflow = @mulWithOverflow(self, native);
            if (overflow == 0) return Object.fromUntaggedI(result, sp, context);
        }
        return error.primitiveError;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#*
        if (sp.next.untaggedI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
            return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
        }
        unreachable;
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        if (sp.next.untaggedI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra }));
            return @call(tailCall, process.check(pc.prim3()), .{ pc.next3(), newSp, process, context, extra });
        }
        trace("SmallInteger>>#inlinePrimitive: * {f}", .{sp.next});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra });
    }
    test "*" {
        var process: Process align(Process.alignment) = undefined;
        process.init();
        const sp = process.getSp();
        const context = process.getContext();
        std.debug.print("* {f} {f}\n", .{ Object.from(12, sp, context), try with(3, Object.from(4, sp, context), sp, context) });
        std.debug.print("* {x} {}\n", .{ Object.from(12, sp, context).testU(), (try with(3, Object.from(4, sp, context), sp, context)).testU()});
        try expectEqual(Object.from(12, sp, context), with(3, Object.from(4, sp, context), sp, context));
        try expectEqual(error.primitiveError, with(0x1_0000_0000, Object.from(0x100_0000, sp, context), sp, context));
        try expectEqual(error.primitiveError, with(0x1_0000_0000, Object.from(0x80_0000, sp, context), sp, context));
    }
};
pub const threadedFns = struct {
    pub const @"inline+I" = struct {
        pub const threadedFn = @"+".inlinePrimitive;
    };
    pub const @"inline-I" = struct {
        pub const threadedFn = @"-".inlinePrimitive;
    };
    pub const @"inline*I" = struct {
        pub const threadedFn = @"*".inlinePrimitive;
    };
    pub const @"inline<=I" = struct {
        pub const threadedFn = @"<=".inlinePrimitive;
    };
    pub const countDown = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            var result = True();
            if (sp.top.untaggedI()) |value| {
                const sum, const overflow = @addWithOverflow(Object.asUntaggedI(-1), value);
                if (overflow == 0) {
                    sp.top = Object.fromUntaggedI(sum, sp, context);
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
            var exe = Execution.initTest("countDown", .{ tf.countDown, tf.pushLiteral, "0One", tf.countDown, tf.pushLiteral, "1Neg", tf.countDown, tf.countDown });
            try exe.resolve(&[_]Object{ Object.fromNativeI(1, null, null), Object.fromNativeI(-5, null, null) });
            try config.skipForDebugging();
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
