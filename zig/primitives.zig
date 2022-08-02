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
    pub fn p1(next: [*]const Code, tos: [*]Object, heap: [*]Object, thread: *Thread, caller: Context) Object {//SmallInteger>>#+
        const other = tos[0];
        if (other.is_int()) {
            const o = other.toUnchecked(i64);
            const result = @bitCast(Object,@bitCast(i64,tos[1])+o);
            if (result.is_int()) {
                tos[1]=result;
                return @call(tailCall,next[1].prim,.{next+2,tos+1,heap,thread,caller});
            }
        }
        return @call(tailCall,p.branch,.{next,tos,heap,thread,caller});
    }
    pub fn p110(next: [*]const Code, tos: [*]Object, heap: [*]Object, thread: *Thread, caller: Context) Object { // ProtoObject>>==
        const result = Object.from(tos[1].equals(tos[0]));
        tos[1]=result;
        return @call(tailCall,next[0].prim,.{next+1,tos+1,heap,thread,caller});
    }
    pub fn p169(next: [*]const Code, tos: [*]Object, heap: [*]Object, thread: *Thread, caller: Context) Object { // ProtoObject>>!!
        const result = Object.from(!tos[1].equals(tos[0]));
        tos[1]=result;
        return @call(tailCall,next[0].prim,.{next+1,tos+1,heap,thread,caller});
    }
    // pub inline fn p111(thread: *Thread, next: [*]const Code, heap: [*]Object, tos: [*]Object, caller: Context) Object { // ProtoObject>>class
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
