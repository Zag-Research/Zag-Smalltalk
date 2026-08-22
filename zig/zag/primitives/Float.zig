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
const Dispatch = zag.dispatch;

pub const moduleName = "SmallInteger";
pub fn init() void {}

pub const @"+" = struct {
    pub const number = 41;
    pub const inlined = signature(.@"+", number);
    inline fn with(self: f64, other: Object, sp: SP, context: *Context) ?Object { // INLINED - Add
        if (other.nativeF()) |untagged| {
            const result = self + untagged;
            return Object.fromNativeF(result, sp, context);
        }
        return null;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#+
        if (with(sp.next.nativeF() orelse unreachable, sp.top, sp, context)) |result| {
            const newSp = sp.dropPut(result);
            return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
        }
        return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
    }
    test "simple add" {
        try config.skipForDebugging();
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
        if (sp.next.nativeF()) |self| {
            if (with(self, sp.top, sp, context)) |result| {
                const newSp = sp.dropPut(result);
                return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
            }
        }
        return @call(tailCall, Dispatch.fail, .{ pc, sp, process, context, extra });
    }
};
pub const @"-" = struct {
    pub const number = 42;
    pub const inlined = signature(.@"-", number);
    inline fn with(self: f64, other: Object, sp: SP, context: *Context) ?Object { // Subtract
        if (other.nativeF()) |untagged| {
            const result = self - untagged;
            return Object.fromNativeF(result, sp, context);
        }
        return null;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#-
        if (with(sp.next.nativeF() orelse unreachable, sp.top, sp, context)) |result| {
            const newSp = sp.dropPut(result);
            return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
        }
        return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        sp.traceStack("-", context, extra);
        if (sp.next.nativeF()) |self| {
            if (with(self, sp.top, sp, context)) |result| {
                const newSp = sp.dropPut(result);
                return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
            }
        }
        return @call(tailCall, Dispatch.fail, .{ pc, sp, process, context, extra });
    }
};
pub const @"<=" = struct {
    pub const number = 45;
    pub const inlined = signature(.@"<=", number);
    inline fn with(self: f64, other: Object, sp: SP, context: *Context) ?Object {
        if (other.nativeF()) |untagged| {
            const result = self <= untagged;
            return Object.from(result, sp, context);
        }
        return null;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#<=
        if (with(sp.next.nativeF() orelse unreachable, sp.top, sp, context)) |result| {
            const newSp = sp.dropPut(result);
            return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
        }
        return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        sp.traceStack("<=", context, extra);
        if (sp.next.nativeF()) |self| {
            if (with(self, sp.top, sp, context)) |result| {
                const newSp = sp.dropPut(result);
                return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
            }
        }
        return @call(tailCall, Dispatch.fail, .{ pc, sp, process, context, extra });
    }
};
pub const @"*" = struct {
    pub const number = 49;
    pub const inlined = signature(.@"*", number);
    inline fn with(self: f64, other: Object, sp: SP, context: *Context) ?Object {
        if (other.nativeF()) |untagged| {
            const result = self * untagged;
            return Object.fromNativeF(result, sp, context);
        }
        return null;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#*
        if (with(sp.next.nativeF() orelse unreachable, sp.top, sp, context)) |result| {
            const newSp = sp.dropPut(result);
            return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
        }
        return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        if (sp.next.nativeF()) |self| {
            if (with(self, sp.top, sp, context)) |result| {
                const newSp = sp.dropPut(result);
                return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
            }
        }
        return @call(tailCall, Dispatch.fail, .{ pc, sp, process, context, extra });
    }
};
pub const threadedFns = struct {
    pub const Float_add = struct {
        pub const threadedFn = @"+".inlinePrimitive;
    };
    pub const Float_sub = struct {
        pub const threadedFn = @"-".inlinePrimitive;
    };
    pub const Float_mul = struct {
        pub const threadedFn = @"*".inlinePrimitive;
    };
    pub const Float_leq = struct {
        pub const threadedFn = @"<=".inlinePrimitive;
    };
};
