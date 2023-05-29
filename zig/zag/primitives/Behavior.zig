const std = @import("std");
const execute = @import("../execute.zig");
const trace = execute.trace;
const Context = execute.Context;
const ContextPtr = *Context;
const Code = execute.Code;
const tailCall = execute.tailCall;
const compileMethod = execute.compileMethod;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const Process = @import("../process.zig").Process;
const object = @import("../zobject.zig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const sym = @import("../symbol.zig").symbols;
const heap = @import("../heap.zig");
const MinSmallInteger: i64 = object.MinSmallInteger;
const MaxSmallInteger: i64 = object.MaxSmallInteger;

pub const inlines = struct {
    pub inline fn p71(self: Object, other: Object) !Object { // basicNew:
        _ = self; _ = other;
//        return error.primitiveError;
        unreachable;
    }
};
const noFallback = execute.noFallback.asFaceObject();
pub const embedded = struct {
    var @"Behavior>>#new:" = noFallback;
    pub fn p71(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) void {// Object>>#new:
        sp[1] = inlines.p71(sp[1],sp[0]) catch
            return @call(tailCall,Context.call,.{pc,sp,process,context,@"Behavior>>#new:".asFakeObject()});
        return @call(tailCall,pc[0].prim,.{pc+1,sp+1,process,context,selector});
    }
};
pub const primitives = struct {
    pub fn p71(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) void { // at:
        _ = pc; _ = sp; _ = process; _ = context; _ = selector; unreachable;
    }
};
const p = struct {
    usingnamespace execute.controlPrimitives;
    usingnamespace primitives;
};
fn testExecute(method: CompiledMethodPtr) []Object {
    var te = execute.TestExecution.new();
    te.init();
    var objs = [_]Object{};
    var result = te.run(objs[0..],method);
    std.debug.print("result = {any}\n",.{result});
    return result;
}
