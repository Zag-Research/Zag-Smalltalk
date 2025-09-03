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
const rawSymbol = zag.symbol.rawSymbol;
const heap = zag.heap;
const empty = &[0]Object{};
const tf = zag.threadedFn.Enum;

pub const moduleName = "Boolean";
pub fn init() void {}
const dnu = execute.controlPrimitives.dnu;
pub const primitives = struct {};
pub const threadedFns = struct {
    pub const branchTrue = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            if (sp.top == Object.from(true, null)) {
                const newPc = pc.targetPC();
                return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), sp.drop(), process, context, extra });
            }
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), sp.drop(), process, context, extra });
        }
    };
    pub const branchFalse = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            process.dumpStack(sp, "branchFalse");
            if (sp.top == Object.from(false, null)) {
                const newPc = pc.targetPC();
                trace("Boolean>>#inlinePrimitive: branchFalse {f} {f}\n", .{ pc, newPc});
                return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), sp.drop(), process, context, extra });
            }
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), sp.drop(), process, context, extra });
        }
    };
};
