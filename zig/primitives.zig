const std = @import("std");
const execute = @import("execute.zig");
const Context = execute.Context;
const Code = execute.Code;
const tailCall = execute.tailCall;
const compileTuple = execute.compileTuple;
const testExecute = execute.testing.testExecute;
const failed_test = execute.testing.failed_test;
const return_tos = execute.testing.return_tos;
const Thread = @import("thread.zig").Thread;
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const MinSmallInteger: i64 = object.MinSmallInteger;
const MaxSmallInteger: i64 = object.MaxSmallInteger;

pub const primitives = struct {
    pub fn p1(pc: [*]const Code, sp: [*]Object, heap: HeapPtr, thread: *Thread, caller: Context) Object {//SmallInteger>>#+
        const other = sp[0];
        if (other.is_int()) {
            const o = other.toUnchecked(i64);
            const result = @bitCast(Object,@bitCast(i64,sp[1])+o);
            if (result.is_int()) {
                sp[1]=result;
                return @call(tailCall,pc[1].prim,.{pc+2,sp+1,heap,thread,caller});
            }
        }
        return @call(tailCall,p.branch,.{pc,sp,heap,thread,caller});
    }
    pub fn p110(pc: [*]const Code, sp: [*]Object, heap: HeapPtr, thread: *Thread, caller: Context) Object { // ProtoObject>>==
        const result = Object.from(sp[1].equals(sp[0]));
        sp[1]=result;
        return @call(tailCall,pc[0].prim,.{pc+1,sp+1,heap,thread,caller});
    }
    pub fn p169(pc: [*]const Code, sp: [*]Object, heap: HeapPtr, thread: *Thread, caller: Context) Object { // ProtoObject>>!!
        const result = Object.from(!sp[1].equals(sp[0]));
        sp[1]=result;
        return @call(tailCall,pc[0].prim,.{pc+1,sp+1,heap,thread,caller});
    }
    // pub inline fn p111(pc: [*]const Code, sp: [*]Object, heap: HeapPtr, thread: *Thread, caller: Context) Object { // ProtoObject>>class
};
const p = struct {
    usingnamespace @import("execute.zig").controlPrimitives;
    usingnamespace primitives;
};
test "simple add" {
    const expectEqual = std.testing.expectEqual;
    const prog = compileTuple(Nil,.{
        p.pushConst,3,
        p.pushConst,4,            
        p.p1,"fail",
        return_tos,
        "fail:", failed_test,
    });
    try expectEqual(testExecute(prog[0..]).to(i64),7);
}
test "simple add with overflow" {
    const expectEqual = std.testing.expectEqual;
    const prog = compileTuple(Nil,.{
        p.pushConst, 0x3_ffffffffffff,
        p.pushConst,4,            
        p.p1,"fail",
        failed_test,
        "fail:", return_tos,
    });
    try expectEqual(testExecute(prog[0..]).to(i64),4);
}
test "simple compare" {
    const expectEqual = std.testing.expectEqual;
    const prog = compileTuple(Nil,.{
        p.pushConst,3,
        p.pushConst,4,            
        p.p110,
        return_tos,
    });
    try expectEqual(testExecute(prog[0..]),False);
}
test "simple compare and don't branch" {
    const expectEqual = std.testing.expectEqual;
    const prog = compileTuple(Nil,.{
        p.pushConst,3,
        p.pushConst,4,            
        p.p110,
        p.if_true,"true",
        p.pushConst,17,
        p.branch,"common",
        "true:",
        p.pushConst,42,
        "common:", return_tos,
    });
    try expectEqual(testExecute(prog[0..]).to(i64),17);
}
test "simple compare and branch" {
    const expectEqual = std.testing.expectEqual;
    const prog = compileTuple(Nil,.{
        p.pushConst,3,
        p.pushConst,4,            
        p.p169,
        p.if_true,"true",
        p.pushConst,17,
        p.branch,"common",
        "true:",
        p.pushConst,42,
        "common:", return_tos,
    });
    try expectEqual(testExecute(prog[0..]).to(i64),42);
}
