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

const il = struct {
    usingnamespace @import("zag/primitives.zig").inlines;
};
const p = struct {
    usingnamespace @import("zag/execute.zig").controlPrimitives;
    usingnamespace @import("zag/primitives.zig").primitives;
};
var sieveCompM = compileMethod(Nil,0,0,.{&sieveComp});
const sieveCompT = @ptrCast([*]Code,&sieveCompM.code[0]);
//	| size flags prime k count |
//	size := 8190.
//	1 to: self do: [ :iter | 
//		count := 0.
//		flags := (ByteArray new: size) atAllPut: 1.
//		1 to: size do: [ :i | 
//			(flags at: i)=1 ifTrue: [ 
//				prime := i + 1.
//				k := i + prime.
//				[ k <= size ] whileTrue: [ 
//					flags at: k put: 0.
//					k := k + prime ].
//				count := count + 1 ] ] ].
//    ^ count
pub fn sieveNative(self: i64) i64 {
    const size = 8190;
    var iter: u64 = 1;
    var count: i64 = undefined;
    while (iter<=self) : (iter += 1) {
        count = 0;
        var flags = [_]i8{1}**(size+1);
        var i: u64 = 1;
        while (i<=size) : (i += 1) {
            if (flags[i]==1) {
                const prime = i + 1;
                var k = i + prime;
                while (k <= size) {
                    flags[k] = 0;
                    k += prime;
                }
                count += 1;
            }
        }
    }
    return count;
}
test "sieveNative" {
    const n = 5;
    const result = sieveNative(5);
    std.debug.print("sieve({}) = {any}\n",.{n,result});
    try std.testing.expectEqual(result,1028);
}
pub fn sieveObject(self: Object) Object {
    const size = Object.from(8190);
    var iter = Object.from(1);
    var count = Nil;
    while (il.p5N(iter,self)) : (iter = il.p1L(iter,1) catch @panic("p1L error in sieveObject")) {
        count = Object.from(0);
        var flags = il.p71(count,size) catch @panic("p71 error in sieveObject");
        il.p145(flags,Object.from(1)) catch @panic("p145 error in sieveObject");
        var i = Object.from(1);
        while (il.p5N(i,size)) : (i = il.p1L(i,1) catch @panic("p1L error in sieveObject")) {
            if (il.p7(il.p60(flags,i) catch @panic("p60 error in sieveObject"),Object.from(1)) catch @panic("p7 error in sieveObject")) {
                const prime = il.p1L(i,1) catch @panic("p1L error in sieveObject");
                var k = il.p1L(i,prime) catch @panic("p1L error in sieveObject");
                while (il.p5N(k,size)) {
                    il.p61(flags,k,Object.from(0)) catch @panic("p61 error in sieveObject");
                    k = il.p1(k,prime) catch @panic("p1 error in sieveObject");
                }
                count = il.p1L(count,1) catch @panic("p1L error in sieveObject");
            }
        }
    }
    return count;
}
pub fn sieveComp(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
    if (il.p5N(sp[0],Object.from(2))) {
        sp[0] = Object.from(1);
        return @call(tailCall,context.npc,.{context.tpc,sp,hp,thread,context});
    }
    const result = context.push(sp,hp,thread,sieveThread.asCompiledMethodPtr(),0,1);
    const newContext = result.ctxt;
    const newHp = result.hp;
    const newSp = newContext.asObjectPtr()-1;
    const m1 = il.p2L(sp[0],1) catch @panic("int subtract failed in sieveComp");
    newSp[0] = m1;
    newContext.tpc = pc+15; // label4 + callLocal
    newContext.npc = sieveComp1;
    return @call(tailCall,sieveComp,.{sieveCompT+1,newSp,newHp,thread,newContext});
}
fn sieveComp1(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
    const newSp = sp-1;
    const m2 = il.p2L(context.getTemp(0),2) catch @panic("int add failed in sieveComp1");
    newSp[0] = m2;
    context.tpc = pc+6; // after 2nd callLocal
    context.npc = sieveComp2;
    return @call(tailCall,sieveComp,.{sieveCompT+1,newSp,hp,thread,context});
}
fn sieveComp2(_: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
    const sum = il.p1(sp[1],sp[0]) catch @panic("int add failed in sieveComp2");
    context.setTemp(0,sum);
    const result = context.pop(thread,0);
    const newSp = result.sp;
    const callerContext = result.ctxt;
    return @call(tailCall,callerContext.npc,.{callerContext.tpc,newSp,hp,thread,callerContext});
}
const sieveThreadRef = uniqueSymbol(42);
const ByteArray = 42;
var sieveThread =
    compileMethod(Nil,0,0,.{
        "label1:",
        &p.pushLiteral, Object.from(8190),
        &p.popIntoTemp, 1, //size
        &p.pushLiteral1,
        &p.popIntoTemp, 6, //iter
        "label2:",
        &p.pushTemp, 6, //iter
        &p.pushTemp, 8, //self
        &p.p5,
        &p.ifFalse, "label12",
        "label3:",
        &p.pushLiteral0,
        &p.popIntoTemp, 5, //count
        &p.pushLiteral, Object.from(ByteArray),
        &p.pushTemp, 1, //size
        &p.p71,
        &p.pushLiteral1,
        &p.p145,
        &p.popIntoTemp, 2, //flags
        &p.pushTemp, 1, //size
        &p.popIntoTemp, 8, //limit_i
        &p.pushLiteral1,
        &p.popIntoTemp, 7, //i
        "label4:",
        &p.pushTemp, 7, //i
        &p.pushTemp, 8, //limit_i
        &p.p5,
        &p.ifFalse, "label11",
        "label5:",
        &p.pushTemp, 2, //flags
        &p.pushTemp, 7, //i
        &p.p60,
        &p.pushLiteral1,
        &p.p7,
        &p.ifFalse, "label10",
        "label6:",
        &p.pushTemp, 7, //i
        &p.pushLiteral1,
        &p.p1,
        &p.popIntoTemp, 3, //prime
        &p.pushTemp, 7, //i
        &p.pushTemp, 3, //prime
        &p.p1,
        &p.popIntoTemp, 4, //k
        "label7:",
        &p.pushTemp, 4, //k
        &p.pushTemp, 1, //size
        &p.p5,
        &p.ifFalse, "label9",
        "label8:",
        &p.pushTemp, 2, //flags
        &p.pushTemp, 4, //k
        &p.pushLiteral0,
        &p.p61,
        &p.drop,
        &p.pushTemp, 4, //k
        &p.pushTemp, 3, //prime
        &p.p1,
        &p.popIntoTemp, 4, //k
        &p.branch, "label7",
        "label9:",
        &p.pushTemp, 5, //count
        &p.pushLiteral1,
        &p.p1,
        &p.popIntoTemp, 5, //count
        "label10:",
        &p.pushTemp, 7, //i
        &p.pushLiteral1,
        &p.p1,
        &p.popIntoTemp, 7, //i
        &p.branch, "label4",
        "label11:",
        &p.pushTemp, 6, //iter
        &p.pushLiteral1,
        &p.p1,
        &p.popIntoTemp, 6, //iter
        &p.branch, "label2",
        "label12:",
        &p.pushTemp, 5, //count
        &p.returnTop
});
test "sieveObject" {
    var n:i32 = 1;
    while (n<10) : (n += 1) {
        const result = sieveObject(Object.from(n));
        std.debug.print("sieve({}) = {any}\n",.{n,result});
        try std.testing.expectEqual(result.toInt(),@truncate(i51,sieveNative(n)));
    }
}
fn timeObject(n: i64) void {
    _ = sieveObject(Object.from(n));
}
test "sieveThread" {
    const method = sieveThread.asCompiledMethodPtr();
    sieveThread.update(sieveThreadRef,method);
    var n:u32 = 1;
    while (n<10) : (n += 1) {
        var objs = [_]Object{Object.from(n)};
        var te =  TestCodeExecution.new();
        te.init();
        const result = te.run(objs[0..],method);
        std.debug.print("sieve({}) = {any}\n",.{n,result});
        try std.testing.expectEqual(result.len,1);
        try std.testing.expectEqual(result[0].toInt(),@truncate(i51,sieveNative(n)));
    }
}
fn timeThread(n: i64) void {
    const method = sieveThread.asCompiledMethodPtr();
    sieveThread.update(sieveThreadRef,method);
    var objs = [_]Object{Object.from(n)};
    var te = TestCodeExecution.new();
    te.init();
    _ = te.run(objs[0..],method);
}
test "sieveComp" {
    var method = compileMethod(Nil,0,0,.{
        &sieveComp,
    });
    var n:i32 = 1;
    while (n<20) : (n += 1) {
        var objs = [_]Object{Object.from(n)};
        var te =  TestCodeExecution.new();
        te.init();
        const result = te.run(objs[0..],method.asCompiledMethodPtr());
        std.debug.print("sieve({}) = {any}\n",.{n,result});
        try std.testing.expectEqual(result.len,1);
        try std.testing.expectEqual(result[0].toInt(),@truncate(i51,sieveNative(n)));
    }
}
fn timeComp(n: i64) void {
    var method = compileMethod(Nil,0,0,.{
        &sieveComp,
    });
    var objs = [_]Object{Object.from(n)};
    var te = TestCodeExecution.new();
    te.init();
    _ = te.run(objs[0..],method.asCompiledMethodPtr());
}
pub fn timing(runs: u32) !void {
    const ts=std.time.nanoTimestamp;
    try stdout.print("for {} runs\n",.{runs});
    var start=ts();
    _ = sieveNative(runs);
    var base = ts()-start;
    try stdout.print("sieveNative: {d:8.3}s {d:8.3}ns\n",.{@intToFloat(f64,base)/1000000000,@intToFloat(f64,base)/@intToFloat(f64,runs)});
    // start=ts();
    // _ = timeObject(runs);
    // _ = sieveThread;
    // _ = Object;
    // var time = ts()-start;
    // try stdout.print("sieveObject: {d:8.3}s {d:8.3}ns +{d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time)/@intToFloat(f64,runs),@intToFloat(f64,time-base)*100.0/@intToFloat(f64,base)});
    // start=ts();
    // _ = timeComp(runs);
    // _ = sieveThread;
    // _ = Object;
    // time = ts()-start;
    // try stdout.print("sieveComp:   {d:8.3}s {d:8.3}ns +{d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time)/@intToFloat(f64,runs),@intToFloat(f64,time-base)*100.0/@intToFloat(f64,base)});
    // start=ts();
    // _ = timeThread(runs);
    // time = ts()-start;
    // try stdout.print("sieveThread: {d:8.3}s {d:8.3}ns +{d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time)/@intToFloat(f64,runs),@intToFloat(f64,time-base)*100.0/@intToFloat(f64,base)});
}
pub fn main() !void {
    try timing(5000);
}
