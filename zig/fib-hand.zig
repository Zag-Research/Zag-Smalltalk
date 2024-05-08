// fib
//   self <= 2 ifTrue: [^ 1]
//   ^ (self - 1) fib + (self - 2) fib
const fibCM = compileMethod(...);
pub fn fib(pc: PC, sp: SP, _process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
    const process = tfAsProcess(_process);
    const context = tfAsContext(_context);
    const self = sp.top;
    if (self.u() <= Object.from(2).u()) {
        sp.top = Object.from(1);
        return @call(tailCall, context.nPc, .{ context.tPc, sp, process, context, undefined});
    }
    const ctxt = context.push(sp, process, fibCM, 0, 2, 1);
    const newSp = ctxt.asNewSp();
    const newerSp = sp.push(self-Object.from(1).untagged());
    ctxt.setReturnBoth(fib1, pc.next());
    return @call(tailCall, &fib, .{ fibCM.codePtr(), newerSp, process, ctxt, undefined});
}
pub fn fib1(pc: PC, sp: SP, _process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
    const process = tfAsProcess(_process);
    const context = tfAsContext(_context);
    const self = context.self();
    const newSp = sp.push(self-Object.from(2).untagged());
    contxt.setReturnBoth(fib2, pc.next());
    return @call(tailCall, &fib, .{ fibCM.codePtr(), newSp, process, contxt, undefined});
}
pub fn fib2(pc: PC, sp: SP, _process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
    const process = tfAsProcess(_process);
    const context = tfAsContext(_context);
    const result = sp.top+sp.next.untagged();
    const spAndContext = context.pop(process);
    const newSp = spAndContext.sp;
    newSp.top = result;
    const ctxt = spAndContext.ctxt;
    return @call(tailCall, &ctxt.nPc, .{ ctxt.tPc, newSp, process, ctxt, undefined});
}
