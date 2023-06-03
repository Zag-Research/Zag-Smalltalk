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
    pub inline fn generalClosure(oldSp: [*]Object, process: *Process, value: Object) ![*]Object {
        const sp = try process.allocStack(oldSp,4);
        sp[0] = Object.from(&sp[1]);
        sp[1] = Object.from(&valueClosureMethod);
        sp[2] = value;
        sp[3] = HeapObject.simpleStackObject(2,object.BlockClosure_I,@truncate(u24,@ptrToInt(sp)+%value.u()));
    }
};

pub const embedded = struct {
    pub const value = p201;
    pub fn p201(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // value
        _ = pc; _ = sp; _ = process; _ = context; _ = selector; unreachable;
    }
    pub fn immutableClosure(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        _ = pc; _ = sp; _ = process; _ = context; _ = selector; unreachable;
        const val = sp[0];
        var newSp = sp;
        if (val.isInt()) {
        } else if (val.isDouble() and (val.u()&0x1ffff)==0) {
        } else if (val.isImmediate()) {
        } else if (val.isHeapObject()) {
        } else {
            newSp = inlined.generalClosure(sp,process,val);
        }
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,process,context,selector});
    }
    pub fn generalClosure(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = inlined.generalClosure(sp+1,process,sp[0]) catch unreachable;
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,process,context,selector});
    }
};
fn testImmutableClosure(process: *Process,value: Object) []Object {
    const sp = process.endOfStack()-1;
    sp[0] = value;
    newSp = embedded.immutableClosure(execute.TestExecution.end,sp,process,context,Nil);
    return process.getStack(newSp);
}
test "immutableClosures" {
    var process = Process.new();
    process.init();
    try assert(mem.eql(Object,testImmutableClosure(process,Object.from(1)),.{Object.from(1)}));
}
pub const primitives = struct {
    pub fn p201(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // value
        if (!sym.value.equals(selector)) return @call(tailCall,dnu,.{pc,sp,process,context,selector});
        _ = pc; _ = sp; _ = process; _ = context; unreachable;
    }
    pub fn p202(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // value:
        _ = pc; _ = sp; _ = process; _ = context; _ = selector; unreachable;
    }
    pub fn p203(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // value:value:
        _ = pc; _ = sp; _ = process; _ = context; _ = selector; unreachable;
    }
    pub fn p204(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // value:value:value:
        _ = pc; _ = sp; _ = process; _ = context; _ = selector; unreachable;
    }
    pub fn p205(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // value:value:value:value:
        _ = pc; _ = sp; _ = process; _ = context; _ = selector; unreachable;
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
