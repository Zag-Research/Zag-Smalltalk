const std = @import("std");
const debug = std.debug;
const math = std.math;
const stdout = std.io.getStdOut().writer();
const Object = @import("zag/zobject.zig").Object;
const indexSymbol = @import("zag/zobject.zig").indexSymbol;
const Nil = @import("zag/zobject.zig").Nil;
const execute = @import("zag/execute.zig");
const tailCall = execute.tailCall;
const Code = execute.Code;
const compileMethod = execute.compileMethod;
const ContextPtr = execute.CodeContextPtr;
const compileByteCodeMethod = @import("zag/byte-interp.zig").compileByteCodeMethod;
const TestExecution = execute.TestExecution;
const Process = @import("zag/process.zig").Process;
const uniqueSymbol = @import("zag/symbol.zig").uniqueSymbol;
const symbol =  @import("zag/symbol.zig");
const heap = @import("zag/heap.zig");
const empty = &[0]Object{};
const Sym = struct {
    fibonacci: Object,
    const ss = heap.compileStrings(.{
        "fibonacci",
    });
    usingnamespace symbol.symbols;
    fn init() Sym {
        return .{
            .fibonacci = symbol.intern(ss[0].asObject()),
        };
    }
};
var sym: Sym = undefined;
const i = @import("zag/primitives.zig").inlines;
const e = @import("zag/primitives.zig").embedded;
const p = @import("zag/primitives.zig").primitives;
var fibCPSM = compileMethod(Sym.value,0,0,.{&fibCPS});
const fibCPST = @ptrCast([*]Code,&fibCPSM.code[0]);
// fibonacci
//	self <= 2 ifTrue: [ ^ 1 ].
//	^ (self - 1) fibonacci + (self - 2) fibonacci
pub fn fibNative(self: i64) i64 {
    if (self <= 2) return 1;
    return fibNative(self-1) + fibNative(self-2);
}
pub fn fibObject(self: Object) Object {
    if (self.u() <= Object.from(2).u()) return Object.from(1);
    const m1 = i.p2L(self,1) catch @panic("int subtract failed in fibObject");
    const fm1 = fibObject(m1);
    const m2 = i.p2L(self,2) catch @panic("int subtract failed in fibObject");
    const fm2 = fibObject(m2);
    return i.p1(fm1,fm2) catch @panic("int add failed in fibObject");
}
const fibSym = Sym.value;
const dnu = execute.controlPrimitives.dnu;
pub fn fibCPS(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
    if (!fibSym.hashEquals(selector)) return @call(tailCall,dnu,.{pc,sp,process,context,selector});
    if (i.p5N(sp[0],Object.from(2))) {
        sp[0] = Object.from(1);
        return @call(tailCall,context.npc,.{context.tpc,sp,process,context,selector});
    }
    const newContext = context.push(sp,process,fibThread.asCompiledMethodPtr(),0,2,0);
    const newSp = newContext.asObjectPtr()-1;
    newSp[0] = i.p2L(sp[0],1) catch return @call(tailCall,pc[10].prim,.{pc+11,newSp+1,process,context,fibSym});
    newContext.setReturnBoth(fibCPS1,pc+13); // after first callRecursive
    return @call(tailCall,fibCPS,.{fibCPST+1,newSp,process,newContext,fibSym});
}
fn fibCPS1(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, _: Object) [*]Object {
    const newSp = sp-1;
    newSp[0] = i.p2L(context.getLocal(0),2) catch return @call(tailCall,pc[0].prim,.{pc+1,newSp,process,context,fibSym});
    context.setReturnBoth(fibCPS2,pc+3); // after 2nd callRecursive
    return @call(tailCall,fibCPS,.{fibCPST+1,newSp,process,context,fibSym});
}
fn fibCPS2(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
    const sum = i.p1(sp[1],sp[0]) catch return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,fibSym}); 
    context.setLocal(0,sum);
    var result = context.pop(process);
    const newSp = result.sp;
    var callerContext = result.ctxt;
    return @call(tailCall,callerContext.npc,.{callerContext.tpc,newSp,process,callerContext,selector});
}
test "fibObject" {
    var n:i32 = 1;
    while (n<10) : (n += 1) {
        const result = fibObject(Object.from(n));
        std.debug.print("\nfib({}) = {any}",.{n,result});
        try std.testing.expectEqual(result.toInt(),@truncate(i51,fibNative(n)));
    }
}
fn timeObject(n: i64) void {
    _ = fibObject(Object.from(n));
}
var fibThread =
    compileMethod(Sym.value,0,2,.{
        ":recurse",
        &e.dup, // self
        &e.pushLiteral2, //&e.pushLiteral, Object.from(2),
        &e.p5N, // <= know that self and 2 are definitely integers
        &e.ifFalse,"label3",
        &e.drop, // self
        &e.pushLiteral1,
        &e.returnNoContext,
        ":label3",
        &e.pushContext,"^",
        &e.pushLocal0,
        &e.p2L1, // -1 &e.pushLiteral1,&e.p2,
        &e.callRecursive, "recurse",
        &e.pushLocal0,
        &e.p2L2, // -2
        &e.callRecursive, "recurse",
        &e.p1, // +
        &e.returnTop,
});
test "fibThread" {
    const method = fibThread.asCompiledMethodPtr();
//    fibThread.update(fibThreadRef,method);
    var n:u32 = 1;
    while (n<10) : (n += 1) {
        var objs = [_]Object{Object.from(n)};
        var te =  TestExecution.new();
        te.init();
        const result = te.run(objs[0..],method);
        std.debug.print("\nfib({}) = {any}",.{n,result});
        try std.testing.expectEqual(result.len,1);
        try std.testing.expectEqual(result[0].toInt(),@truncate(i51,fibNative(n)));
    }
}
fn timeThread(n: i64) void {
    const method = fibThread.asCompiledMethodPtr();
    var objs = [_]Object{Object.from(n)};
    var te = TestExecution.new();
    te.init();
    _ = te.run(objs[0..],method);
}
const index_1 = indexSymbol(1);
var fibDispatch =
    compileMethod(index_1,0,2,.{
        &e.dup, // self
        &e.pushLiteral2, //&e.pushLiteral, Object.from(2),
        &e.p5N, // <= know that self and 2 are definitely integers
        &e.ifFalse,"label3",
        &e.drop, // self
        &e.pushLiteral1,
        &e.returnNoContext,
        ":label3",
        &e.pushContext,"^",
        &e.pushLocal0,
        &e.p2L1, // -1 &e.pushLiteral1,&e.p2,
        &e.send0, index_1,
        &e.pushLocal0,
        &e.p2L2, // -2
        &e.send0, index_1,
        &e.p1, // +
        &e.returnTop,
});
test "fibDispatch" {
    const method = fibDispatch.asCompiledMethodPtr();
//    fibDispatch.update(fibDispatchRef,method);
    var n:u32 = 1;
    sym = Sym.init();
    fibDispatch.setLiterals(&[_]Object{sym.fibonacci},empty);
    while (n<10) : (n += 1) {
        var objs = [_]Object{Object.from(n)};
        var te =  TestExecution.new();
        te.init();
        const result = te.run(objs[0..],method);
        std.debug.print("\nfib({}) = {any}",.{n,result});
        try std.testing.expectEqual(result.len,1);
        try std.testing.expectEqual(result[0].toInt(),@truncate(i51,fibNative(n)));
    }
}
fn timeDispatch(n: i64) void {
    const method = fibDispatch.asCompiledMethodPtr();
    var objs = [_]Object{Object.from(n)};
    var te = TestExecution.new();
    te.init();
    _ = te.run(objs[0..],method);
}
test "fibCPS" {
    var method = compileMethod(Sym.value,0,2,.{
        &fibCPS,
    });
    var n:i32 = 1;
    while (n<20) : (n += 1) {
        var objs = [_]Object{Object.from(n)};
        var te =  TestExecution.new();
        te.init();
        const result = te.run(objs[0..],method.asCompiledMethodPtr());
        std.debug.print("fib({}) = {any}\n",.{n,result});
        try std.testing.expectEqual(result.len,1);
        try std.testing.expectEqual(result[0].toInt(),@truncate(i51,fibNative(n)));
    }
}
fn timeCPS(n: i64) void {
    var method = compileMethod(Sym.value,0,0,.{
        &fibCPS,
    });
    var objs = [_]Object{Object.from(n)};
    var te = TestExecution.new();
    te.init();
    _ = te.run(objs[0..],method.asCompiledMethodPtr());
}
const b = @import("zag/byte-interp.zig").ByteCode;
// test "fibByte" {
//     var fibByte =
//         compileByteCodeMethod(Sym.value,0,0,.{
//             ":recurse",
//             b.dup,
//             b.pushLiteral, Object.from(2),
//             b.p5,"label1",
//             b.primFailure,
//             ":label1",
//             b.ifFalse,"label3",
//             b.drop,
//             b.pushLiteral, Object.from(1),
//             b.returnNoContext,
//             ":label3",
//             b.pushContext,"^",
//             b.pushTemp1,
//             b.pushLiteral, Object.from(1),
//             b.p2, "label4",
//             b.primFailure,
//             ":label4",
//             b.callLocal, "recurse",
//             b.pushTemp1,
//             b.pushLiteral, Object.from(2),
//             b.p2,"label5",
//             b.primFailure,
//             ":label5",
//             b.callLocal, "recurse",
//             b.p1,"label6",
//             b.primFailure,
//             ":label6",
//             b.returnTop,0,
//     });
//     const method = fibByte.asCompiledByteCodeMethodPtr();
//     var n:i32 = 1;
//     while (n<10) : (n += 1) {
//         var objs = [_]Object{Object.from(n)};
//         var te =  TestExecution.new();
//         te.init();
//         const result = te.run(objs[0..],method);
//         std.debug.print("fib({}) = {any}\n",.{n,result});
//         try std.testing.expectEqual(result.len,1);
//         try std.testing.expectEqual(result[0].toInt(),@truncate(i51,fibNative(n)));
//     }
// }
 fn timeByte(n: i64) void {
     var fibByte =
         compileByteCodeMethod(Sym.value,0,0,.{
             ":recurse",
             b.dup,
             b.pushLiteral, Object.from(2),
             b.p5,"label1",
             b.primFailure,
             ":label1",
             b.ifFalse,"label3",
             b.drop,
             b.pushLiteral, Object.from(1),
             b.returnNoContext,
             ":label3",
             b.pushContext,"^",
             b.pushTemp1,
             b.pushLiteral, Object.from(1),
             b.p2, "label4",
             b.primFailure,
             ":label4",
             b.callLocal, "recurse",
             b.pushTemp1,
             b.pushLiteral, Object.from(2),
             b.p2,"label5",
             b.primFailure,
             ":label5",
             b.callLocal, "recurse",
             b.p1,"label6",
             b.primFailure,
             ":label6",
             b.returnTop,0,
     });
     fibByte.setReferences(&[0]Object{});
     const method = fibByte.asCompiledMethodPtr();
     var objs = [_]Object{Object.from(n)};
     var te = TestExecution.new();
     te.init();
     std.debug.print("about to run\n",.{});
     _ = te.run(objs[0..],method);
}
const ts=std.time.nanoTimestamp;
fn tstart() i128 {
    const t = ts();
    while (true) {
        const newT = ts();
        if (newT!=t) return newT;
    }
}
pub fn timing(runs: u6) !void {
    try stdout.print("for '{} fibonacci'\n",.{runs});
    var start=tstart();
    _ = fibNative(runs);
    var base = ts()-start;
    try stdout.print("fibNative: {d:8.3}s\n",.{@intToFloat(f64,base)/1000000000});
    start=tstart();
    _ = timeObject(runs);
    var time = ts()-start;
    try stdout.print("fibObject: {d:8.3}s +{d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time-base)*100.0/@intToFloat(f64,base)});
    start=tstart();
    _ = timeCPS(runs);
    time = ts()-start;
    try stdout.print("fibCPS:    {d:8.3}s +{d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time-base)*100.0/@intToFloat(f64,base)});
    start=tstart();
    _ = timeThread(runs);
    time = ts()-start;
    try stdout.print("fibThread: {d:8.3}s +{d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time-base)*100.0/@intToFloat(f64,base)});
    start=tstart();
    _ = timeDispatch(runs);
    time = ts()-start;
    try stdout.print("fibDispatch: {d:8.3}s +{d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time-base)*100.0/@intToFloat(f64,base)});
    // start=tstart();
    // _ = timeByte(runs);
    // time = ts()-start;
    // try stdout.print("fibByte:   {d:8.3}s +{d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time-base)*100.0/@intToFloat(f64,base)});
}
pub fn main() !void {
    try timing(40);
}
