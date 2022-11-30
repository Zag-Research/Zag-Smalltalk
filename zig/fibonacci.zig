const std = @import("std");
const debug = std.debug;
const math = std.math;
const tailCall: std.builtin.CallOptions = .{.modifier = .always_tail};
const noInlineCall: std.builtin.CallOptions = .{.modifier = .never_inline};
const stdout = std.io.getStdOut().writer();
const Object = @import("zag/object.zig").Object;
const Nil = @import("zag/object.zig").Nil;
const Code = @import("zag/execute.zig").Code;
const compileMethod = @import("zag/execute.zig").compileMethod;
const ContextPtr = @import("zag/execute.zig").CodeContextPtr;
const compileByteCodeMethod = @import("zag/byte-interp.zig").compileByteCodeMethod;
const TestCodeExecution = @import("zag/execute.zig").TestCodeExecution;
const TestByteCodeExecution = @import("zag/byte-interp.zig").TestByteCodeExecution;
const Hp = @import("zag/heap.zig").HeaderArray;
const Thread = @import("zag/thread.zig").Thread;
const uniqueSymbol = @import("zag/symbol.zig").uniqueSymbol;
//pub fn uniqueSymbol(uniqueNumber:u24) Object {
//    return @bitCast(Object,uniqueNumber|@as(u64,0xfff60007ff000000));
//y}

const i = struct {
    usingnamespace @import("zag/primitives.zig").inlines;
};
const p = struct {
    usingnamespace @import("zag/execute.zig").controlPrimitives;
    usingnamespace @import("zag/primitives.zig").primitives;
};
var fibCompM = compileMethod(Nil,0,0,.{&fibComp});
const fibCompT = @ptrCast([*]Code,&fibCompM.code[0]);
// fibonacci
//	self <= 2 ifTrue: [ ^ 1 ].
//	^ (self - 1) fibonacci + (self - 2) fibonacci
fn fibNative(self: u64) u64 {
    if (self <= 2) return 1;
    return fibNative(self-1) + fibNative(self-2);
}
fn fibComp(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
    if (i.p5N(sp[0],Object.from(2))) {
        sp[0] = Object.from(1);
        return @call(tailCall,context.npc,.{context.tpc,sp,hp,thread,context});
    }
    const result = context.push(sp,hp,thread,fibThread.asCompiledMethodPtr(),0,1);
    const newContext = result.ctxt;
    const newHp = result.hp;
    const newSp = newContext.asObjectPtr()-1;
    const m1 = i.p2L(sp[0],1) catch @panic("int subtract failed in fibComp");
    newSp[0] = m1;
    newContext.tpc = pc+15; // label4 + callLocal
    newContext.npc = fibComp1;
    return @call(tailCall,fibComp,.{fibCompT+1,newSp,newHp,thread,newContext});
}
fn fibComp1(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
    const newSp = sp-1;
    const m2 = i.p2L(context.getTemp(0),2) catch @panic("int add failed in fibComp1");
    newSp[0] = m2;
    context.tpc = pc+6; // after 2nd callLocal
    context.npc = fibComp2;
    return @call(tailCall,fibComp,.{fibCompT+1,newSp,hp,thread,context});
}
fn fibComp2(_: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
    const sum = i.p1(sp[1],sp[0]) catch @panic("int add failed in fibComp2");
    context.setTemp(0,sum);
    const result = context.pop(thread,0);
    const newSp = result.sp;
    const callerContext = result.ctxt;
    return @call(tailCall,callerContext.npc,.{callerContext.tpc,newSp,hp,thread,callerContext});
}
const fibThreadRef = uniqueSymbol(42);
var fibThread =
    compileMethod(Nil,0,0,.{
        "recurse:",
        &p.dup,
        //&p.pushLiteral, Object.from(2),
        &p.pushLiteral2,
        //&p.p5,"label1",
        //&p.primFailure,
        //"label1:",
        &p.p5N, // know that self and 2 are definitely integers
        &p.ifFalse,"label3",
        &p.drop, // self
        &p.pushLiteral1,
        &p.returnNoContext,
        "label3:",
        &p.pushContext,"^",
        &p.pushTemp1,
        //&p.pushLiteral1,
        //&p.p2, "label4",
        &p.p2L1, "label4",
        &p.primFailure,
        "label4:",
        &p.callLocal, "recurse",
        &p.pushTemp1,
        &p.p2L2,"label5",
        &p.primFailure,
        "label5:",
        &p.callLocal, "recurse",
        &p.p1,"label6",
        &p.primFailure,
        "label6:",
        &p.returnTop0,
});
test "fibThread" {
    const method = fibThread.asCompiledMethodPtr();
    fibThread.update(fibThreadRef,method);
    var n:u32 = 1;
    while (n<10) : (n += 1) {
        var objs = [_]Object{Object.from(n)};
        var te =  TestCodeExecution.new();
        te.init();
        const result = te.run(objs[0..],method);
        std.debug.print("fib({}) = {any}\n",.{n,result});
        try std.testing.expectEqual(result.len,1);
        try std.testing.expectEqual(result[0].toInt(),@as(i64,@truncate(u51,fibNative(n))));
    }
}
fn timeThread(n: i64) void {
    const method = fibThread.asCompiledMethodPtr();
    fibThread.update(fibThreadRef,method);
    var objs = [_]Object{Object.from(n)};
    var te = TestCodeExecution.new();
    te.init();
    _ = te.run(objs[0..],method);
}
test "fibComp" {
    var method = compileMethod(Nil,0,0,.{
        &fibComp,
    });
    var n:u32 = 1;
    while (n<20) : (n += 1) {
        var objs = [_]Object{Object.from(n)};
        var te =  TestCodeExecution.new();
        te.init();
        const result = te.run(objs[0..],method.asCompiledMethodPtr());
        std.debug.print("fib({}) = {any}\n",.{n,result});
        try std.testing.expectEqual(result.len,1);
        try std.testing.expectEqual(result[0].toInt(),@as(i64,@truncate(u51,fibNative(n))));
    }
}
fn timeComp(n: i64) void {
    var method = compileMethod(Nil,0,0,.{
        &fibComp,
    });
    var objs = [_]Object{Object.from(n)};
    var te = TestCodeExecution.new();
    te.init();
    _ = te.run(objs[0..],method.asCompiledMethodPtr());
}
const b = @import("zag/byte-interp.zig").ByteCode;
test "fibByte" {
    var fibByte =
        compileByteCodeMethod(Nil,0,0,.{
            "recurse:",
            b.dup,
            b.pushLiteral, Object.from(2),
            b.p5,"label1",
            b.primFailure,
            "label1:",
            b.ifFalse,"label3",
            b.drop,
            b.pushLiteral, Object.from(1),
            b.returnNoContext,
            "label3:",
            b.pushContext,"^",
            b.pushTemp1,
            b.pushLiteral, Object.from(1),
            b.p2, "label4",
            b.primFailure,
            "label4:",
            b.callLocal, "recurse",
            b.pushTemp1,
            b.pushLiteral, Object.from(2),
            b.p2,"label5",
            b.primFailure,
            "label5:",
            b.callLocal, "recurse",
            b.p1,"label6",
            b.primFailure,
            "label6:",
            b.returnTop,0,
    });
    const method = fibByte.asCompiledByteCodeMethodPtr();
    var n:u32 = 1;
    while (n<10) : (n += 1) {
        var objs = [_]Object{Object.from(n)};
        var te =  TestByteCodeExecution.new();
        te.init();
        const result = te.run(objs[0..],method);
        std.debug.print("fib({}) = {any}\n",.{n,result});
        try std.testing.expectEqual(result.len,1);
        try std.testing.expectEqual(result[0].toInt(),@as(i64,@truncate(u51,fibNative(n))));
    }
}
 fn timeByte(n: i64) void {
     var fibByte =
         compileByteCodeMethod(Nil,0,0,.{
             "recurse:",
             b.dup,
             b.pushLiteral, Object.from(2),
             b.p5,"label1",
             b.primFailure,
             "label1:",
             b.ifFalse,"label3",
             b.drop,
             b.pushLiteral, Object.from(1),
             b.returnNoContext,
             "label3:",
             b.pushContext,"^",
             b.pushTemp1,
             b.pushLiteral, Object.from(1),
             b.p2, "label4",
             b.primFailure,
             "label4:",
             b.callLocal, "recurse",
             b.pushTemp1,
             b.pushLiteral, Object.from(2),
             b.p2,"label5",
             b.primFailure,
             "label5:",
             b.callLocal, "recurse",
             b.p1,"label6",
             b.primFailure,
             "label6:",
             b.returnTop,0,
     });
     const method = fibByte.asCompiledByteCodeMethodPtr();
     var objs = [_]Object{Object.from(n)};
     var te = TestByteCodeExecution.new();
     te.init();
     _ = te.run(objs[0..],method);
}
pub fn timing(runs: u32) !void {
    const ts=std.time.nanoTimestamp;
    try stdout.print("for {} runs\n",.{runs});
    var start=ts();
    _ = fibNative(runs);
    var base = ts()-start;
    try stdout.print("fibNative: {d:8.3}s {d:8.3}ns\n",.{@intToFloat(f64,base)/1000000000,@intToFloat(f64,base)/@intToFloat(f64,runs)});
    start=ts();
    _ = timeComp(runs);
    _ = fibThread;
    _ = Object;
    var time = ts()-start;
    try stdout.print("fibComp: {d:8.3}s {d:8.3}ns +{d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time)/@intToFloat(f64,runs),@intToFloat(f64,time-base)*100.0/@intToFloat(f64,base)});
    start=ts();
    _ = timeThread(runs);
    time = ts()-start;
    try stdout.print("fibThread: {d:8.3}s {d:8.3}ns +{d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time)/@intToFloat(f64,runs),@intToFloat(f64,time-base)*100.0/@intToFloat(f64,base)});
    start=ts();
    _ = timeByte(runs);
    time = ts()-start;
    try stdout.print("fibByte: {d:8.3}s {d:8.3}ns +{d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time)/@intToFloat(f64,runs),@intToFloat(f64,time-base)*100.0/@intToFloat(f64,base)});
}
pub fn main() !void {
    try timing(40);
}
