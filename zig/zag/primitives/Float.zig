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

pub const @"+" = struct {
    pub const number = 41;
    pub const inlined = signature(.@"+", number);
    pub fn with(self: f64, other: Object, sp: SP, context: *Context) !Object { // INLINED - Add
        if (other.nativeF()) |untagged| {
            const result = self + untagged;
            return Object.fromNativeF(result, sp, context);
        }
        return error.primitiveError;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#+
        if (sp.next.nativeF()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
            return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
        } else
            return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
    }
    test "simple add" {
        if (true) return config.skipForDebugging;
        var exe = Execution.initTest("simple add", .{ tf.primitive, comptime fromPrimitive(1) });
        try exe.runTest(
            &[_]Object{
                exe.object(25.0),
                exe.object(17.0),
            },
            &[_]Object{
                exe.object(42.0),
            },
        );
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        sp.traceStack("+", context, extra);
        const receiver = sp.next;
        if (receiver.nativeF()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra }));
            return @call(tailCall, process.check(pc.prim3()), .{ pc.next3(), newSp, process, context, extra });
        }
        trace("Float>>#inlinePrimitive: + {f}", .{receiver});
        if (true) @panic("unreachable");
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra });
    }
};
pub const @"-" = struct {
    pub const number = 42;
    pub const inlined = signature(.@"-", number);
    pub inline fn with(self: f64, other: Object, sp: SP, context: *Context) !Object { // Subtract
        if (other.nativeF()) |untagged| {
            const result = self - untagged;
            return Object.fromNativeF(result, sp, context);
        }
        return error.primitiveError;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#-
        if (sp.next.nativeF()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
            return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
        }
        return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        sp.traceStack("-", context, extra);
        const receiver = sp.next;
        if (receiver.nativeF()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra }));
            return @call(tailCall, process.check(pc.prim3()), .{ pc.next3(), newSp, process, context, extra });
        }
        trace("Float>>#inlinePrimitive: - {f}", .{receiver});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra });
    }
};
pub const @"<=" = struct {
    pub const number = 45;
    pub const inlined = signature(.@"<=", number);
    pub fn with(self: f64, other: Object, sp: SP, context: *Context) !Object {
        if (other.nativeF()) |untagged| {
            const result = self <= untagged;
            return Object.from(result, sp, context);
        }
        return error.primitiveError;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#<=
        if (sp.next.nativeF()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
            return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
        }
        return @call(tailCall, process.check(context.npc), .{ context.tpc, sp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        sp.traceStack("<=", context, extra);
        const receiver = sp.next;
        if (receiver.nativeF()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra }));
            return @call(tailCall, process.check(pc.prim3()), .{ pc.next3(), newSp, process, context, extra });
        }
        trace("Inline <= called, {*} {f}", .{ sp, extra });
        return @call(tailCall, process.check(pc.prim3()), .{ pc.next3(), sp, process, context, extra });
    }
};
pub const @"*" = struct {
    pub const number = 49;
    pub const inlined = signature(.@"*", number);
    pub fn with(self: f64, other: Object, sp: SP, context: *Context) !Object {
        if (other.nativeF()) |untagged| {
            const result = self * untagged;
            return Object.fromNativeF(result, sp, context);
        }
        return error.primitiveError;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#*
        if (sp.next.nativeF()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
            return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
        }
        return @call(tailCall, process.check(context.npc), .{ context.tpc, sp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const receiver = sp.next;
        if (receiver.nativeF()) |self| {
            const newSp = sp.dropPut(with(self, sp.top, sp, context) catch
                return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra }));
            return @call(tailCall, process.check(pc.prim3()), .{ pc.next3(), newSp, process, context, extra });
        }
        return @call(tailCall, process.check(pc.prim3()), .{ pc.next3(), sp, process, context, extra });
    }
};
pub const threadedFns = struct {
    pub const @"inline+F" = struct {
        pub const threadedFn = @"+".inlinePrimitive;
    };
    pub const @"inline-F" = struct {
        pub const threadedFn = @"-".inlinePrimitive;
    };
    pub const @"inline*F" = struct {
        pub const threadedFn = @"*".inlinePrimitive;
    };
    pub const @"inline<=F" = struct {
        pub const threadedFn = @"<=".inlinePrimitive;
    };
};
