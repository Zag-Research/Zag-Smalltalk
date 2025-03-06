const std = @import("std");
const assert = std.debug.assert;
const config = zag.config;
const tailCall = config.tailCall;
const stdCall = config.stdCall;
const zag = @import("zag.zig");
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

pub const branch = struct {
    pub const order = 0;
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
        const target = pc.targetPC();
        if (process.needsCheck()) return @call(tailCall, Process.check, .{ target, sp, process, context, undefined });
        return @call(tailCall, target.prim(), .{ target.next(), sp, process.checkBump(), context, undefined });
    }
};
pub const call = struct {
    pub const order = 0;
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
        context.setReturn(pc.next2());
        const method = pc.method();
        const newPc = PC.init(method.codePtr());
        if (process.needsCheck()) return @call(tailCall, Process.check, .{ newPc, sp, process, context, undefined });
        return @call(tailCall, method.executeFn, .{ newPc.next(), sp, process, context, Extra.from(method) });
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
