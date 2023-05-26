const std = @import("std");
const execute = @import("../execute.zig");
const trace = execute.trace;
const ContextPtr = execute.CodeContextPtr;
const Code = execute.Code;
const tailCall = execute.tailCall;
const compileMethod = execute.compileMethod;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const Process = @import("../process.zig").Process;
const object = @import("../object.zig");
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
    pub inline fn p201(self: Object, other: Object) !Object { // value
        _ = self; _ = other;
        return error.primitiveError;
    }
    pub inline fn p202(self: Object, other: Object) !Object { // value:
        _ = self; _ = other;
        return error.primitiveError;
    }
    pub inline fn p203(self: Object, other: Object) !Object { // value:value:
        _ = self; _ = other;
        return error.primitiveError;
    }
    pub inline fn p204(self: Object, other: Object) bool { // value:value:value:
        return self.equals(other);
    }
    pub inline fn p205(self: Object, other: Object) !Object { // value:value:value:value:
        _ = self; _ = other;
        return error.primitiveError;
    }
};

pub const embedded = struct {
}
pub const primitives = struct {
    pub fn p201(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selectorHash: u32) void { // value
        _ = pc; _ = sp; _ = process; _ = context; _ = selectorHash; unreachable;
    }
    pub fn p202(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selectorHash: u32) void { // value:
        _ = pc; _ = sp; _ = process; _ = context; _ = selectorHash; unreachable;
    }
    pub fn p203(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selectorHash: u32) void { // value:value:
        _ = pc; _ = sp; _ = process; _ = context; _ = selectorHash; unreachable;
    }
    pub fn p204(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selectorHash: u32) void { // value:value:value:
        sp[1] = Object.from(inlines.p110(sp[1],sp[0]));
        return @call(tailCall,p.branch,.{pc,sp+1,process,context,selectorHash});
    }
    pub fn p205(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selectorHash: u32) void { // value:value:value:value:
        inlines.p145(sp[1],sp[0]) catch return @call(tailCall,pc[1].prim,.{pc+2,sp,process,context,selectorHash});
        return @call(tailCall,p.branch,.{pc,sp+1,process,context,selectorHash});
    }
};
const p = struct {
    usingnamespace @import("../execute.zig").primitives;
    usingnamespace primitives;
};
test "simple ==" {
    const expect = std.testing.expect;
    var prog = compileMethod(sym.value,0,0,.{
        &p.pushLiteral,Object.from(4),
        &p.pushLiteral,Object.from(4),
        &p.p110,"next",":next",
        &p.returnNoContext,
    });
    const result = testExecute(prog.asCompiledMethodPtr());
    try expect(result[0].to(bool));
}
fn testExecute(method: CompiledMethodPtr) []Object {
    var te = @import("context.zig").TestExecution.new();
    te.init();
    var objs = [_]Object{};
    var result = te.run(objs[0..],method);
    std.debug.print("result = {any}\n",.{result});
    return result;
}
