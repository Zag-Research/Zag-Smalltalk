const std = @import("std");
const execute = @import("../execute.zig");
const trace = execute.trace;
const ContextPtr = execute.CodeContextPtr;
const Code = execute.Code;
const tailCall = execute.tailCall;
const compileMethod = execute.compileMethod;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const Thread = @import("../thread.zig").Thread;
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

pub const embedded = struct {
}
pub const primitives = struct {
    pub fn p60(pc: [*]const Code, sp: [*]Object, thread: *Thread, context: ContextPtr, selectorHash: u32) void { // at:
        _ = pc; _ = sp; _ = thread; _ = context; _ = selectorHash; unreachable;
    }
    pub fn p61(pc: [*]const Code, sp: [*]Object, thread: *Thread, context: ContextPtr, selectorHash: u32) void { // at:
        _ = pc; _ = sp; _ = thread; _ = context; _ = selectorHash; unreachable;
    }
    pub fn p71(pc: [*]const Code, sp: [*]Object, thread: *Thread, context: ContextPtr, selectorHash: u32) void { // at:
        _ = pc; _ = sp; _ = thread; _ = context; _ = selectorHash; unreachable;
    }
    pub fn p110(pc: [*]const Code, sp: [*]Object, thread: *Thread, context: ContextPtr, selectorHash: u32) void { // ProtoObject>>#==
        sp[1] = Object.from(inlines.p110(sp[1],sp[0]));
        return @call(tailCall,p.branch,.{pc,sp+1,thread,context,selectorHash});
    }
    pub fn p145(pc: [*]const Code, sp: [*]Object, thread: *Thread, context: ContextPtr, selectorHash: u32) void { // atAllPut:
        inlines.p145(sp[1],sp[0]) catch return @call(tailCall,pc[1].prim,.{pc+2,sp,thread,context,selectorHash});
        return @call(tailCall,p.branch,.{pc,sp+1,thread,context,selectorHash});
    }
    pub fn p169(pc: [*]const Code, sp: [*]Object, thread: *Thread, context: ContextPtr, selectorHash: u32) void { // ProtoObject>>#~~
        sp[1] = Object.from(inlines.p169(sp[1],sp[0]));
        return @call(tailCall,p.branch,.{pc,sp+1,thread,context,selectorHash});
    }
    // pub inline fn p111(pc: [*]const Code, sp: [*]Object, heap: Hp, rpc: [*]const Code, thread: *Thread, caller: Context) Object { // ProtoObject>>class
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
test "simple add" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(sym.value,0,0,.{
        &p.pushLiteral,Object.from(3),
        &p.pushLiteral,Object.from(4),
        &p.p1,"success",
        &p.pushLiteral,Object.from(-999),
        ":success", &p.returnNoContext,
    });
    const result = testExecute(prog.asCompiledMethodPtr());
    try expectEqual(result[0].toInt(),7);
}
test "simple add with overflow" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(sym.value,0,0,.{
        &p.pushLiteral,Object.from(4),            
        &p.pushLiteral,Object.from(0x3_ffffffffffff),
        &p.p1,"succeeded",
        &p.pushLiteral,Object.from(-999),
        ":succeeded",
        &p.returnNoContext,
    });
    try expectEqual(testExecute(prog.asCompiledMethodPtr())[0].toInt(),-999);
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
