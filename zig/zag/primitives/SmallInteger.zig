//std imports
const std = @import("std");
const divFloor = std.math.divFloor;
const divExact = std.math.divExact;

const expectEqual = std.testing.expectEqual;

//zag imports
const zag = @import("../zag.zig");

const tf = zag.threadedFn.Enum;
const Sym = zag.symbol.symbols;
const heap = zag.heap;
const signature = zag.symbol.signature;

//zag.config imports
const config = zag.config;
const trace = config.trace;
const tailCall = config.tailCall;

//zag.Context imports
const Context = zag.Context;
const Extra = Context.Extra;

//zag.execute imports
const execute = zag.execute;
const Code = execute.Code;
const PC = execute.PC;
const Result = execute.Result;
const Execution = execute.Execution;
const CompiledMethod = execute.CompiledMethod;
const fromPrimitive = execute.Signature.fromPrimitive;

//zag.Process imports
const Process = zag.Process;
const SP = Process.SP;

//zag.object imports
const object = zag.object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const Object = object.Object;
const empty = &[0]Object{};

pub const moduleName = "SmallInteger";
pub fn init() void {}

//number = 1
pub const @"+" = struct {
    pub const number = 1;
    pub const inlined = signature(.@"+", number);
    //inline fn with(self: i64, other: Object, process: *Process, context: *Context) !Object { // INLINED - Add
    // TODO: verify switch to sp and propogate
    inline fn with(self: i64, other: Object, sp: SP, context: *Context) !Object { // INLINED - Add
        if (other.untaggedI()) |untagged| {
            const result, const overflow = @addWithOverflow(self, untagged);
            if (overflow == 0)
                return Object.fromTaggedI(result, sp, context);
        }
        return error.primitiveError;
    }
    //TODO: verify primitive update acroos calls
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

//number = 2
pub const @"-" = struct {
    pub const number = 2;
    pub const inlined = signature(.@"-", number);
    inline fn with(self: i64, other: Object, sp: SP, context: *Context) !Object { // Subtract
        if (other.untaggedI()) |untagged| {
            const result, const overflow = @subWithOverflow(self, untagged);
            if (overflow == 0)
                return Object.fromTaggedI(result, sp, context);
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

//number = 3
pub const @"<" = struct {
    pub const number = 3;
    pub const inlined = signature(.@"<", number);
    pub inline fn with(self: i64, other: Object, sp: SP, context: *Context) !Object { // Less
        if (other.taggedI()) |tagged|
            return Object.from(self < tagged, sp, context);
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
        sp.traceStack("<", context, extra);
        if (sp.next.taggedI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra }));
            return @call(tailCall, process.check(pc.prim3()), .{ pc.next3(), newSp, process, context, extra });
        }
        trace("Float>>#inlinePrimitive: < {f}", .{sp.next});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra });
    }
};

//number = 4
pub const @">" = struct {
    pub const number = 4;
    pub const inlined = signature(.@">", number);
    inline fn with(self: i64, other: Object, sp: SP, context: *Context) !Object { // Greater
        if (other.taggedI()) |tagged|
            return Object.from(self > tagged, sp, context);
        return error.primitiveError;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#>
        if (sp.next.taggedI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
            return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
        }
        unreachable;
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        sp.traceStack(">", context, extra);
        if (sp.next.taggedI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra }));
            return @call(tailCall, process.check(pc.prim3()), .{ pc.next3(), newSp, process, context, extra });
        }
        trace("Float>>#inlinePrimitive: > {f}", .{sp.next});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra });
    }
};

//number = 5
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

//number = 6
pub const @">=" = struct {
    pub const number = 6;
    pub const inlined = signature(.@">=", number);
    inline fn with(self: i64, other: Object, sp: SP, context: *Context) !Object { // GreaterOrEqual
        if (other.taggedI()) |tagged|
            return Object.from(self >= tagged, sp, context);
        return error.primitiveError;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#>=
        if (sp.next.taggedI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
            return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
        }
        unreachable;
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        sp.traceStack(">=", context, extra);
        if (sp.next.taggedI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra }));
            return @call(tailCall, process.check(pc.prim3()), .{ pc.next3(), newSp, process, context, extra });
        }
        trace("Float>>#inlinePrimitive: >= {f}", .{sp.next});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra });
    }
};

//number = 7
pub const @"=" = struct {
    pub const number = 7;
    pub const inlined = signature(.@"=", number);
    inline fn with(self: i64, other: Object, sp: SP, context: *Context) !Object {  // Equal
        if (other.taggedI()) |tagged|
            return Object.from(self == tagged, sp, context);
        return error.primitiveError;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#=
        if (sp.next.taggedI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
            return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
        }
        unreachable;
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        sp.traceStack("=", context, extra);
        if (sp.next.taggedI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra }));
            return @call(tailCall, process.check(pc.prim3()), .{ pc.next3(), newSp, process, context, extra });
        }
        trace("Float>>#inlinePrimitive: = {f}", .{sp.next});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra });
    }
};

//number = 8
pub const @"~=" = struct {
    pub const number = 8;
    pub const inlined = signature(.@"~=", number);
    inline fn with(self: i64, other: Object, sp: SP, context: *Context) !Object {  // NotEqual
        if (other.taggedI()) |tagged|
            return Object.from(self != tagged, sp, context);
        return error.primitiveError;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#~=
        if (sp.next.taggedI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
            return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
        }
        unreachable;
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        sp.traceStack("~=", context, extra);
        if (sp.next.taggedI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra }));
            return @call(tailCall, process.check(pc.prim3()), .{ pc.next3(), newSp, process, context, extra });
        }
        trace("Float>>#inlinePrimitive: ~= {f}", .{sp.next});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra });
    }
};

//number = 9
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

// TODO: handle overflow where maxInt/-1
//number = 10
pub const @"/" = struct {
    pub const number = 10;
    pub const inlined = signature(.@"/", number);
    inline fn with(self: i64, other: Object, sp: SP, context: *Context) !Object { // Divide
        if (other.nativeI()) |native| {
            const result = divExact(i64, self, native) catch return error.primitiveError; //TODO mult div untagged values
            return Object.fromNativeI(result, sp, context);
        }
        return error.primitiveError;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#/
        if (sp.next.nativeI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
            return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
        }
        unreachable;
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        if (sp.next.nativeI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra }));
            return @call(tailCall, process.check(pc.prim3()), .{ pc.next3(), newSp, process, context, extra });
        }
        trace("SmallInteger>>#inlinePrimitive: / {f}", .{sp.next});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra });
    }
};

//number = 11
pub const @"\\\\" = struct {
    pub const number = 11;
    pub const inlined = signature(.@"\\\\", number);
    inline fn with(self: i64, other: Object, sp: SP, context: *Context) !Object { // modulo
        if (other.nativeI()) |native| {
            const result = @mod(self, native) catch return error.primitiveError;
            return Object.fromNativeI(result, sp, context);
        }
        return error.primitiveError;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#\\
        if (sp.next.nativeI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
            return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
        }
        unreachable;
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        if (sp.next.nativeI()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra }));
            return @call(tailCall, process.check(pc.prim3()), .{ pc.next3(), newSp, process, context, extra });
        }
        trace("SmallInteger>>#inlinePrimitive: \\\\ {f}", .{sp.next});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra });
    }
};

// TODO: handle overflow where maxInt/-1
//number = 12
pub const @"//" = struct {
    pub const number = 12;
    pub const inlined = signature(.@"//", number);
    inline fn with(self: i64, other: Object, sp: SP, context: *Context) !Object { // div floor
        if (other.nativeI()) |native| {
            const result = divFloor(self.taggedI_noCheck(), native) catch return error.primitiveError; //TODO verify we want to return error
            return Object.fromNativeI(result, sp, context);
        }
        return error.primitiveError;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#//
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
        trace("SmallInteger>>#inlinePrimitive: // {f}", .{sp.next});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra });
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
