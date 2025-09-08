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
const signature = zag.symbol.signature;
const heap = zag.heap;
const empty = &[0]Object{};
const tf = zag.threadedFn.Enum;

pub const moduleName = "SmallInteger";
pub fn init() void {}
pub const inlines = struct {
    pub fn @"+"(self: Object, other: Object, maybeProcess: ?*Process) !Object { // INLINED - Add
        if (other.nativeF()) |untagged| {
            const result = self.nativeF_noCheck() + untagged;
            return Object.fromNativeF(result, maybeProcess);
        }
        return error.primitiveError;
    }
    pub inline fn negated(self: Object, maybeProcess: ?*Process) !Object { // Negate
        const result = -self.nativeF_noCheck();
        return Object.fromNativeF(result, maybeProcess);
    }
    pub inline fn @"-"(self: Object, other: Object, maybeProcess: ?*Process) !Object { // Subtract
        if (other.nativeF()) |untagged| {
            const result = self.nativeF_noCheck() - untagged;
            return Object.fromNativeF(result, maybeProcess);
        }
        return error.primitiveError;
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
    pub inline fn @"*"(self: Object, other: Object, maybeProcess: ?*Process) !Object { // Multiply
        if (other.nativeF()) |untagged| {
            const result = self.nativeF_noCheck() * untagged;
            return Object.fromNativeF(result, maybeProcess);
        }
        return error.primitiveError;
    }
};

pub const @"+" = struct {
    pub const number = 41;
    pub const inlined = signature(.@"+", number);
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
                Object.from(25.0, null),
                Object.from(17.0, null),
            },
            &[_]Object{
                Object.from(42.0, null),
            },
        );
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        process.dumpStack(sp, "+");
        const receiver = sp.next;
        if (!receiver.isFloat()) {
            trace("Float>>#inlinePrimitive: + {f}", .{receiver});
            if (true) unreachable;
            return @call(tailCall, PC.inlinePrimitiveFailed, .{ pc, sp, process, context, extra });
        }
        const newSp = sp.dropPut(inlines.@"+"(receiver, sp.top, process) catch
            return @call(tailCall, PC.inlinePrimitiveFailed, .{ pc, sp, process, context, extra }));
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
    }
};
pub const @"-" = struct {
    pub const number = 42;
    pub const inlined = signature(.@"-", number);
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#-
        const newSp = sp.dropPut(inlines.@"-"(sp.next, sp.top, process) catch
            return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }));
        return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, unreachable });
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        process.dumpStack(sp, "-");
        const receiver = sp.next;
        if (!receiver.isFloat()) {
            trace("Float>>#inlinePrimitive: - {f}\n", .{receiver});
            return @call(tailCall, PC.inlinePrimitiveFailed, .{ pc, sp, process, context, extra });
        }
        const newSp = sp.dropPut(inlines.@"-"(receiver, sp.top, process) catch
            return @call(tailCall, PC.inlinePrimitiveFailed, .{ pc, sp, process, context, extra }));
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
    }
};
pub const @"<=" = struct {
    pub const number = 45;
    pub const inlined = signature(.@"<=", number);
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#<=
        const newSp = sp.dropPut(Object.from(inlines.@"<="(sp.next, sp.top) catch
            return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }), null));
        return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, unreachable });
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        process.dumpStack(sp, "<=");
        const receiver = sp.next;
        if (!receiver.isFloat()) {
            trace("Float>>#inlinePrimitive: <= {f}\n", .{receiver});
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
    pub const number = 49;
    pub const inlined = signature(.@"*", number);
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#*
        const newSp = sp.dropPut(Object.from(inlines.@"*"(sp.next, sp.top, process) catch
            return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra }), null));
        return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, unreachable });
    }
    pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const receiver = sp.next;
        if (!receiver.isFloat()) {
            trace("Float>>#inlinePrimitive: * {f}\n", .{receiver});
            return @call(tailCall, PC.inlinePrimitiveFailed, .{ pc, sp, process, context, extra });
        }
        const newSp = sp.dropPut(inlines.@"*"(receiver, sp.top, process) catch
            return @call(tailCall, PC.inlinePrimitiveFailed, .{ pc, sp, process, context, extra }));
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
    }
};
