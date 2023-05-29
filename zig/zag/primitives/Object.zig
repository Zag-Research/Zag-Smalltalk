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
    pub inline fn p60(self: Object, other: Object) !Object { // at:
        _ = self; _ = other;
        return error.primitiveError;
    }
    pub inline fn p61(self: Object, other: Object) !Object { // at:put:
        _ = self; _ = other;
        return error.primitiveError;
    }
    pub inline fn p71(self: Object, other: Object) !Object { // basicNew:
        _ = self; _ = other;
        return error.primitiveError;
    }
    pub inline fn p110(self: Object, other: Object) bool { // Identical - can't fail
        return self.equals(other);
    }
    pub inline fn p145(self: Object, other: Object) !Object { // atAllPut:
        _ = self; _ = other;
        return error.primitiveError;
    }
    pub inline fn p169(self: Object, other: Object) bool { // NotIdentical - can't fail
        return !self.equals(other);
    }
};
const noFallback = execute.noFallback;
pub const embedded = struct {
    var @"Object>>#at:" = noFallback;
    pub fn p60(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) void {// Object>>#at:
        sp[1] = inlines.p60(sp[1],sp[0]) catch
            return @call(tailCall,Context.call,.{pc,sp,process,context,@"Object>>#at:".asFakeObject()});
        return @call(tailCall,pc[0].prim,.{pc+1,sp+1,process,context,selector});
    }
    var @"Object>>#at:put:" = noFallback;
    pub fn p61(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) void {// Object>>#at:put:
        sp[1] = inlines.p61(sp[1],sp[0]) catch
            return @call(tailCall,Context.call,.{pc,sp,process,context,@"Object>>#at:put:".asFakeObject()});
        return @call(tailCall,pc[0].prim,.{pc+1,sp+1,process,context,selector});
    }
    var @"Object>>#atAllPut:" = noFallback;
    pub fn p145(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) void {// Object>>#atAllPut:
        sp[1] = inlines.p145(sp[1],sp[0]) catch
            return @call(tailCall,Context.call,.{pc,sp,process,context,@"Object>>#atAllPut:".asFakeObject()});
        return @call(tailCall,pc[0].prim,.{pc+1,sp+1,process,context,selector});
    }
};
const dnu = execute.controlPrimitives.dnu;
pub const primitives = struct {
    pub fn p60(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) void { // at:
        _ = pc; _ = sp; _ = process; _ = context; _ = selector; unreachable;
    }
    pub fn p61(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) void { // at:
        _ = pc; _ = sp; _ = process; _ = context; _ = selector; unreachable;
    }
    pub fn p71(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) void { // at:
        _ = pc; _ = sp; _ = process; _ = context; _ = selector; unreachable;
    }
    pub fn p110(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) void { // ProtoObject>>#==
        if (!sym.@"==".equals(selector)) return @call(tailCall,dnu,.{pc,sp,process,context,selector});
        sp[1] = Object.from(inlines.p110(sp[1],sp[0]));
        return @call(tailCall,p.branch,.{pc,sp+1,process,context,selector});
    }
    pub fn p145(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) void { // atAllPut:
        if (!sym.@"atAllPut:".equals(selector)) return @call(tailCall,dnu,.{pc,sp,process,context,selector});
        inlines.p1(sp[0]) catch
            return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
        return @call(tailCall,context.npc,.{context.tpc,sp+1,process,context,selector});
    }
    pub fn p169(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) void { // ProtoObject>>#~~
        if (!sym.@"~~".equals(selector)) return @call(tailCall,dnu,.{pc,sp,process,context,selector});
        sp[1] = Object.from(inlines.p169(sp[1],sp[0]));
        return @call(tailCall,p.branch,.{pc,sp+1,process,context,selector});
    }
    // pub inline fn p111(pc: [*]const Code, sp: [*]Object, heap: Hp, rpc: [*]const Code, process: *Process, caller: Context) Object { // ProtoObject>>class
};
const p = struct {
    usingnamespace execute.controlPrimitives;
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
    var te = execute.TestExecution.new();
    te.init();
    var objs = [_]Object{};
    var result = te.run(objs[0..],method);
    std.debug.print("result = {any}\n",.{result});
    return result;
}
test "simple compare" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(sym.value,0,0,.{
        &p.pushLiteral,Object.from(3),
        &p.pushLiteral,Object.from(4),
        &p.p110,"success",":success",
        &p.returnNoContext,
    });
    try expectEqual(testExecute(prog.asCompiledMethodPtr())[0],False);
}
test "simple compare and don't branch" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(sym.value,0,0,.{
        &p.pushLiteral,Object.from(3),
        &p.pushLiteral,Object.from(4),
        &p.p110,"success",":success",
        &p.ifTrue,"true",
        &p.pushLiteral,Object.from(17),
        &p.branch,"common",
        ":true",
        &p.pushLiteral,Object.from(42),
        ":common", &p.returnNoContext,
    });
    try expectEqual(testExecute(prog.asCompiledMethodPtr())[0].toInt(),17);
}
test "simple compare and branch" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(sym.value,0,0,.{
        &p.pushLiteral,Object.from(3),
        &p.pushLiteral,Object.from(4),
        &p.p169,"success",":success",
        &p.ifTrue,"true",
        &p.pushLiteral,Object.from(17),
        &p.branch,"common",
        ":true",
        &p.pushLiteral,Object.from(42),
        ":common", &p.returnNoContext,
    });
    try expectEqual(testExecute(prog.asCompiledMethodPtr())[0].toInt(),42);
}

test "dispatch3" {
}
