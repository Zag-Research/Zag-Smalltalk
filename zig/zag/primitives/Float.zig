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
    pub inline fn negated(self: Object, process: *Process) !Object { // Negate
        const result = -self.nativeF_noCheck();
        return Object.fromNativeF(result, process);
    }
    pub inline fn @"<"(self: Object, other: Object) !bool { // LessThan
        if (other.nativeF()) |tagged|
            return self.nativeF_noCheck() < tagged;
        return error.primitiveError;
    }
    pub inline fn @">"(self: Object, other: Object) !bool { // GreaterThan
        if (other.nativeF()) |tagged|
            return self.nativeF_noCheck() > tagged;
        return error.primitiveError;
    }
    pub inline fn @"<="(self: Object, other: Object) !bool { // LessOrEqual
        if (other.nativeF()) |tagged|
            return self.nativeF_noCheck() <= tagged;
        return error.primitiveError;
    }
    pub inline fn @">="(self: Object, other: Object) !bool { // GreaterOrEqual
        if (other.nativeF()) |tagged|
            return self.nativeF_noCheck() >= tagged;
        return error.primitiveError;
    }
    pub inline fn @"="(self: Object, other: Object) !bool { // Equal
        if (other.nativeF()) |tagged|
            return self.nativeF_noCheck() == tagged;
        return error.primitiveError;
    }
    pub inline fn @"<>"(self: Object, other: Object) !bool { // NotEqual
        if (other.nativeF()) |tagged|
            return self.nativeF_noCheck() != tagged;
        return error.primitiveError;
    }
    pub inline fn @"*"(self: Object, other: Object, sp: SP, context: *Context) !Object { // Multiply
        if (other.nativeF()) |untagged| {
            const result = self.nativeF_noCheck() * untagged;
            return Object.fromNativeF(result, sp, context);
        }
        return error.primitiveError;
    }
};

pub const @"+" = struct {
    pub const number = 41;
    pub const inlined = signature(.@"+", number);
    pub fn with(self: Object, other: Object, sp: SP, context: *Context) !Object { // INLINED - Add
        if (other.nativeF()) |untagged| {
            const result = self.nativeF_noCheck() + untagged;
            return Object.fromNativeF(result, sp, context);
        }
        return error.primitiveError;
    }
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#+
        const newSp = sp.dropPut(with(sp.next, sp.top, sp, context) catch
            return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
        return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
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
        if (!receiver.isFloat()) {
            trace("Float>>#inlinePrimitive: + {f}", .{receiver});
            if (true) @panic("unreachable");
            return @call(tailCall, primitives.inlinePrimitiveFailed, .{ pc, sp, process, context, extra });
        }
        const newSp = sp.dropPut(with(receiver, sp.top, sp, context) catch
            return @call(tailCall, primitives.inlinePrimitiveFailed, .{ pc, sp, process, context, extra }));
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
    }
};
pub const @"-" = struct {
    pub const number = 42;
    pub const inlined = signature(.@"-", number);
    pub inline fn with(self: Object, other: Object, sp: SP, context: *Context) !Object { // Subtract
        if (other.nativeF()) |untagged| {
            const result = self.nativeF_noCheck() - untagged;
            return Object.fromNativeF(result, sp, context);
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
        if (!receiver.isFloat()) {
            trace("Float>>#inlinePrimitive: - {f}", .{receiver});
            return @call(tailCall, primitives.inlinePrimitiveFailed, .{ pc, sp, process, context, extra });
        }
        const newSp = sp.dropPut(with(receiver, sp.top, sp, context) catch
            return @call(tailCall, primitives.inlinePrimitiveFailed, .{ pc, sp, process, context, extra }));
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
    }
};
pub const @"<=" = struct {
    pub const number = 45;
    pub const inlined = signature(.@"<=", number);
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#<=
        const newSp = sp.dropPut(Object.from(inlines.@"<="(sp.next, sp.top) catch
            return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }), sp, context));
        return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        sp.traceStack("<=", context, extra);
        const receiver = sp.next;
        if (!receiver.isFloat()) {
            trace("Float>>#inlinePrimitive: <= {f}", .{receiver});
            if (true) @panic("unreachable");
            return @call(tailCall, primitives.inlinePrimitiveFailed, .{ pc, sp, process, context, extra });
        }
        const newSp = sp.dropPut(Object.from(inlines.@"<="(receiver, sp.top) catch
            return @call(tailCall, primitives.inlinePrimitiveFailed, .{ pc, sp, process, context, extra }), sp, context));
        trace("Inline <= called, {*} {f}", .{ newSp, extra });
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
    }
};
pub const @"*" = struct {
    pub const number = 49;
    pub const inlined = signature(.@"*", number);
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#*
        const newSp = sp.dropPut(Object.from(inlines.@"*"(sp.next, sp.top, sp, context) catch
            return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }), sp, context));
        return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const receiver = sp.next;
        if (!receiver.isFloat()) {
            trace("Float>>#inlinePrimitive: * {f}", .{receiver});
            return @call(tailCall, primitives.inlinePrimitiveFailed, .{ pc, sp, process, context, extra });
        }
        const newSp = sp.dropPut(inlines.@"*"(receiver, sp.top, sp, context) catch
            return @call(tailCall, primitives.inlinePrimitiveFailed, .{ pc, sp, process, context, extra }));
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
    }
};
