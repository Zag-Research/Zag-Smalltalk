const std = @import("std");
const execute = @import("execute.zig");
const ContextPtr = execute.ContextPtr;
const Code = execute.Code;
const tailCall = execute.tailCall;
const compileMethod = execute.compileMethod;
const testExecute = execute.testing.testExecute;
const failed_test = execute.testing.failed_test;
const return_tos = execute.testing.return_tos;
const Thread = @import("thread.zig").Thread;
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const sym = @import("symbol.zig");
const heap = @import("heap.zig");
const HeapPtr = heap.HeapPtr;
const MinSmallInteger: i64 = object.MinSmallInteger;
const MaxSmallInteger: i64 = object.MaxSmallInteger;

pub const inlines = struct {
    pub inline fn p110(sp: [*]Object) [*]Object { // Identical - can't fail
        sp[1] = Object.from(sp[1].equals(sp[0]));
        return sp+1;
    }
    pub inline fn p169(sp: [*]Object) [*]Object { // NotIdentical - can't fail
        sp[1] = Object.from(!sp[1].equals(sp[0]));
        return sp+1;
    }
    pub inline fn p1(sp: [*]Object, intBase: u64) ![*]Object { // Add
        const other = sp[0];
        if (other.isInt(intBase)) {
            const result = @bitCast(Object,@bitCast(i64,sp[1])+other.toUnchecked(i64));
            if (result.isInt(intBase)) {
                sp[1]=result;
                return sp+1;
            }
        }
        return error.primitiveError;
    }
};

pub const primitives = struct {
    pub fn p1(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {//SmallInteger>>#+
        const newSp = inlines.p1(sp,u64_MINVAL) catch return @call(tailCall,pc[1].prim.*,.{pc+2,sp,hp,doCheck,thread,context});
        return @call(tailCall,p.branch,.{pc,newSp,hp,doCheck,thread,context});
    }
    pub fn p110(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void { // ProtoObject>>==
        const newSp = inlines.p110(sp);
        return @call(tailCall,p.branch,.{pc,newSp,hp,doCheck,thread,context});
    }
    pub fn p169(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void { // ProtoObject>>~~
        const newSp = inlines.p169(sp);
        return @call(tailCall,p.branch,.{pc,newSp,hp,doCheck,thread,context});
    }
    // pub inline fn p111(pc: [*]const Code, sp: [*]Object, heap: HeapPtr, rpc: [*]const Code, thread: *Thread, caller: Context) Object { // ProtoObject>>class
};
const p = struct {
    usingnamespace @import("execute.zig").controlPrimitives;
    usingnamespace primitives;
};
test "simple add" {
    const expectEqual = std.testing.expectEqual;
    const prog = compileMethod(Nil,0,0,.{
        &p.pushLiteral,3,
        &p.pushLiteral,4,            
        &p.p1,"success",
        &failed_test,
        "success:", &return_tos,
    });
    const pr = std.io.getStdOut().writer().print;
    const result = testExecute(prog.asMethodPtr());
    try pr("result = {}\n",.{result});
    try expectEqual(result.toInt(u64_MINVAL),7);
}
test "simple add with overflow" {
    const expectEqual = std.testing.expectEqual;
    const prog = compileMethod(Nil,0,0,.{
        &p.pushLiteral, 0x3_ffffffffffff,
        &p.pushLiteral,4,            
        &p.p1,"success",
        &return_tos,
        "success:",&failed_test,
    });
    try expectEqual(testExecute(prog.asMethodPtr()).toInt(u64_MINVAL),4);
}
// fibonacci
//	self <= 2 ifTrue: [ ^ 1 ].
//	^ (self - 1) fibonacci + (self - 2) fibonacci
test "fibonacci" {
    return error.not_implemented;
}
test "simple compare" {
    const expectEqual = std.testing.expectEqual;
    const prog = compileMethod(Nil,0,0,.{
        &p.pushLiteral,3,
        &p.pushLiteral,4,            
        &p.p110,"success","success:",
        &return_tos,
    });
    try expectEqual(testExecute(prog.asMethodPtr()),False);
}
test "simple compare and don't branch" {
    const expectEqual = std.testing.expectEqual;
    const prog = compileMethod(Nil,0,0,.{
        &p.pushLiteral,3,
        &p.pushLiteral,4,            
        &p.p110,"success","success:",
        &p.ifTrue,"true",
        &p.pushLiteral,17,
        &p.branch,"common",
        "true:",
        &p.pushLiteral,42,
        "common:", &return_tos,
    });
    try expectEqual(testExecute(prog.asMethodPtr()).toInt(u64_MINVAL),17);
}
test "simple compare and branch" {
    const expectEqual = std.testing.expectEqual;
    const prog = compileMethod(Nil,0,0,.{
        &p.pushLiteral,3,
        &p.pushLiteral,4,            
        &p.p169,"success","success:",
        &p.ifTrue,"true",
        &p.pushLiteral,17,
        &p.branch,"common",
        "true:",
        &p.pushLiteral,42,
        "common:", &return_tos,
    });
    try expectEqual(testExecute(prog.asMethodPtr()).toInt(u64_MINVAL),42);
}

test "dispatch3" {
}
pub fn main() void {
    const prog = compileMethod(Nil,0,0,.{
        &p.pushLiteral,3,
        &p.pushLiteral,4,            
        &p.p1,"success",
        &failed_test,
        "success:", &return_tos,
    });
    _ = testExecute(prog.asMethodPtr());
}
