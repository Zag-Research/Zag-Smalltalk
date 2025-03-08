const std = @import("std");
const assert = std.debug.assert;
const zag = @import("zag.zig");
const config = zag.config;
const trace = config.trace;
const tailCall = config.tailCall;
const stdCall = config.stdCall;
const object = zag.object;
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
//const primitives = zag.primitives;
//const globalArena = zag.globalArena;
const symbol = zag.symbol;
const Sym = symbol.symbols;
const Process = zag.Process;
const Context = zag.context;
const execute = zag.execute;
const ThreadedFn = execute.ThreadedFn;
const PC = execute.PC;
const SP = execute.SP;
const Extra = execute.Extra;
const compileMethod = execute.compileMethod;
const Execution = execute.Execution;
const tf = zag.threadedFn.Enum;
pub const branch = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
        std.debug.print("pc: 0x{x:0>8}\n", .{@as(u64, @bitCast(pc))});
        const target = pc.targetPC();
        if (process.needsCheck()) return @call(tailCall, Process.check, .{ target, sp, process, context, undefined });
        return @call(tailCall, target.prim(), .{ target.next(), sp, process.checkBump(), context, undefined });
    }
    test "branch" {
        try Execution.runTest(
            "branch",
            .{
                tf.branch,
                "label",
                ":label",
            },
            &[_]Object{},
            &[_]Object{},
        );
    }
};
pub const call = struct {
    pub const order = 0;
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
        context.setReturn(pc.next2());
        const method = pc.method();
        const newPc = PC.init(method.codePtr());
        if (process.needsCheck()) return @call(tailCall, Process.check, .{ newPc, sp, process, context, undefined });
        return @call(tailCall, method.executeFn, .{ newPc.next(), sp, process, context, Extra{ .method = method } });
    }
};
pub const classCase = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) callconv(stdCall) SP {
        var newPc = pc;
        const top = sp.top;
        const match = @intFromEnum(top.get_class());
        const newSp = sp.drop();
        while (true) {
            var classes = pc.object().to(u64);
            newPc = newPc.next();
            for (0..4) |_| {
                const class: u14 = @truncate(classes);
                if (class == match) {
                    newPc = newPc.targetPC();
                    return @call(tailCall, newPc.prim(), .{ newPc.next(), newSp, process, context, extra });
                }
                if (class == 0)
                    return @call(tailCall, newPc.prim(), .{ newPc.next(), newSp, process, context, extra });
                if (class == 0x3FFF)
                    return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, extra });
                classes >>= 14;
                newPc = newPc.next();
            }
        }
    }
};
pub const pushLiteral = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) callconv(stdCall) SP {
        const newSp = sp.push(pc.object());
        trace("\npushLiteral: {any}", .{context.stack(newSp, process)});
        return @call(tailCall, pc.prim2(), .{ pc.skip(2), newSp, process, context, extra });
    }
    test "pushLiteral" {
        try Execution.runTest(
            "pushLiteral",
            .{
                tf.pushLiteral,
                42,
            },
            &[_]Object{},
            &[_]Object{Object.from(42)},
        );
    }
};
pub const pushStack = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) callconv(stdCall) SP {
        const offset = pc.object().to(u64);
        const newSp = sp.push(sp.at(offset));
        trace("\npushStack: {any}", .{context.stack(newSp, process)});
        return @call(tailCall, pc.prim2(), .{ pc.skip(2), newSp, process, context, extra });
    }
    test "pushStack" {
        try Execution.runTest(
            "pushStack",
            .{ tf.pushStack, 1, tf.pushStack, 4 },
            &[_]Object{
                Object.from(42),
                Object.from(17),
                Object.from(2),
                Object.from(3),
            },
            &[_]Object{
                Object.from(3),
                Object.from(17),
                Object.from(42),
                Object.from(17),
                Object.from(2),
                Object.from(3),
            },
        );
    }
};
