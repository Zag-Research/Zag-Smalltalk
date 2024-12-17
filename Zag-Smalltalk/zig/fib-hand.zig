const std = @import("std");
const config = @import("zag/config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const object = @import("zag/zobject.zig");
const Object = object.Object;
const Context = @import("zag/context.zig").Context;
const execute = @import("zag/execute.zig");
const PC = execute.PC;
const SP = execute.SP;
const TFProcess = execute.TFProcess;
const tfAsProcess = execute.tfAsProcess;
const TFContext = execute.TFContext;
const tfAsContext = execute.tfAsContext;
const MethodSignature = execute.MethodSignature;
const compileMethod = execute.compileMethod;
const Sym = @import("zag/symbol.zig").symbols;
// fib
//   self <= 2 ifTrue: [^ 1]
//   ^ (self - 1) fib + (self - 2) fib
const _fibCM = compileMethod(Sym.fibonacci,0,2,.SmallInteger,.{&fib,&fib1,&fib2});
const fibCM = _fibCM.asCompiledMethodPtr();
export fn fib(pc: PC, sp: SP, _process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
    const process = tfAsProcess(_process);
    const context = tfAsContext(_context);
    const self = sp.top;
    if (self.rawU() <= Object.from(2).rawU()) {
        sp.top = Object.from(1);
        return @call(tailCall, context.npc, .{ context.tpc, sp, process, context, undefined});
    }
    const ctxt = @call(.never_tail, &Context.push, .{context, sp, process, fibCM, 0, 2, 1});
    const newSp = ctxt.asNewSp();
    const newerSp = newSp.pushRawInt(self.rawU()-Object.from(1).untaggedInt());
    ctxt.setReturnBoth(fib1, pc.next());
    return @call(tailCall, &fib, .{ fibCM.codePc(), newerSp, process, ctxt, undefined});
}
pub fn fib1(pc: PC, sp: SP, _process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
    const process = tfAsProcess(_process);
    const context = tfAsContext(_context);
    const self = context.getSelf();
    const newSp = sp.pushRawInt(self.rawU()-Object.from(2).untaggedInt());
    context.setReturnBoth(fib2, pc.next());
    return @call(tailCall, &fib, .{ fibCM.codePc(), newSp, process, context, undefined});
}
pub fn fib2(_: PC, sp: SP, _process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
    const process = tfAsProcess(_process);
    const context = tfAsContext(_context);
    const result = sp.top.rawU()+sp.next.untaggedInt();
    const spAndContext = @call(.never_tail,&Context.pop, .{context,process});
    const newSp = spAndContext.sp;
    newSp.top = @bitCast(result);
    const ctxt = spAndContext.ctxt;
    return @call(tailCall, ctxt.npc, .{ ctxt.tpc, newSp, process, ctxt, undefined});
}
test "nothing" {
    _ = &fib;
}
