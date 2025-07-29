const std = @import("std");
const zag = @import("../zag.zig");
const config = zag.config;
const tailCall = config.tailCall;
const trace = config.trace;
const execute = zag.execute;
const SendCache = execute.SendCache;
const Context = execute.Context;
const ContextPtr = *Context;
const Code = execute.Code;
const PC = execute.PC;
const SP = execute.SP;
const compileMethod = execute.compileMethod;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const Process = zag.Process;
const object = zag.object;
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const Sym = zag.symbol.symbols;
const heap = zag.heap;
const tf = zag.threadedFn.Enum;
const stringOf = zag.heap.CompileTimeString;
const HeapHeader = zag.heap.HeapHeader;
const expectEqual = std.testing.expectEqual;

pub fn init() void {}

pub const inlines = struct {
    pub inline fn p71(self: Object, other: Object) !Object { // basicNew:
        _ = self;
        _ = other;
        //        return error.primitiveError;
        unreachable;
    }
};
pub const embedded = struct {
    const fallback = execute.fallback;
    pub fn @"new:"(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, _: SendCache) SP {
        sp[1] = inlines.p71(sp[1], sp[0]) catch return @call(tailCall, process.check(fallback), .{ pc, sp, process, context, undefined, undefined });
        return @call(tailCall, process.check(pc[0].prim), .{ pc + 1, sp + 1, process, context, undefined, undefined });
    }
};
pub const primitives = struct {
    pub fn p70(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP { // basicNew
        _ = .{ pc, sp, process, context, selector, cache };
        unreachable;
    }
    pub fn p71(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP { // basicNew:
        _ = .{ pc, sp, process, context, selector, cache };
        unreachable;
    }
};
fn testExecute(method: CompiledMethodPtr) []Object {
    var te = execute.TestExecution.new();
    te.init();
    var objs = [_]Object{};
    const result = te.run(objs[0..], method);
    std.debug.print("result = {any}\n", .{result});
    return result;
}
