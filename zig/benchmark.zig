const std = @import("std");
const debug = std.debug;
const math = std.math;
const stdout = std.io.getStdOut().writer();
const Object = @import("zag/zobject.zig").Object;
const Nil = @import("zag/zobject.zig").Nil;
const tailCall = @import("zag/config.zig").tailCall;
const Code = @import("zag/execute.zig").Code;
const compileMethod = @import("zag/execute.zig").compileMethod;
const ContextPtr = @import("zag/execute.zig").CodeContextPtr;
//const compileByteCodeMethod = @import("zag/byte-interp.zig").compileByteCodeMethod;
const TestExecution = @import("zag/execute.zig").TestExecution;
const Process = @import("zag/process.zig").Process;
const uniqueSymbol = @import("zag/symbol.zig").uniqueSymbol;
const sym = @import("zag/symbol.zig").symbols;

const i = @import("zag/primitives.zig").inlines;
const e = @import("zag/primitives.zig").embedded;
const p = @import("zag/primitives.zig").primitives;
var benchCompM = compileMethod(sym.value, 0, 0, .{&benchComp});
const benchCompT = @as([*]Code, @ptrCast(&benchCompM.code[0]));
// benchonacci
//	self <= 2 ifTrue: [ ^ 1 ].
//	^ (self - 1) benchonacci + (self - 2) benchonacci
pub fn benchNative(self: i64) i64 {
    if (self <= 2) return 1;
    return benchNative(self - 1) + benchNative(self - 2);
}
pub fn benchObject(self: Object) Object {
    if (self.u() <= Object.from(2).u()) return Object.from(1);
    const m1 = i.p2L(self, 1) catch @panic("int subtract failed in benchObject");
    const fm1 = benchObject(m1);
    const m2 = i.p2L(self, 2) catch @panic("int subtract failed in benchObject");
    const fm2 = benchObject(m2);
    return i.p1(fm1, fm2) catch @panic("int add failed in benchObject");
}
const benchSym = sym.value;
pub fn benchComp(pc: [*]const Code, sp: SP, process: *Process, context: ContextPtr, selector: Object) void {
    if (!benchSym.hashEquals(selector)) @panic("wrong selector");
    if (i.p5N(sp[0], Object.from(2))) {
        sp[0] = Object.from(1);
        return @call(tailCall, context.npc, .{ context.tpc, sp, process, context, selector });
    }
    const newContext = context.push(sp, process, benchThread.asCompiledMethodPtr(), 0, 2, 0);
    const newSp = newContext.asObjectPtr() - 1;
    const m1 = i.p2L(sp[0], 1) catch @panic("int subtract failed in benchComp");
    newSp[0] = m1;
    newContext.tpc = pc + 15; // label4 + callLocal
    newContext.npc = benchComp1;
    return @call(tailCall, benchComp, .{ benchCompT + 1, newSp, process, newContext, benchSym });
}
fn benchComp1(pc: [*]const Code, sp: SP, process: *Process, context: ContextPtr, _: Object) void {
    const newSp = sp - 1;
    const m2 = i.p2L(context.getTemp(0), 2) catch @panic("int add failed in benchComp1");
    newSp[0] = m2;
    context.tpc = pc + 6; // after 2nd callLocal
    context.npc = benchComp2;
    return @call(tailCall, benchComp, .{ benchCompT + 1, newSp, process, context, benchSym });
}
fn benchComp2(_: [*]const Code, sp: SP, process: *Process, context: ContextPtr, selector: Object) void {
    const sum = i.p1(sp[1], sp[0]) catch @panic("int add failed in benchComp2");
    context.setTemp(0, sum);
    const result = context.pop(process);
    const newSp = result.sp;
    const callerContext = result.ctxt;
    return @call(tailCall, callerContext.npc, .{ callerContext.tpc, newSp, process, callerContext, selector });
}
test "benchObject" {
    var n: i32 = 1;
    while (n < 10) : (n += 1) {
        const result = benchObject(Object.from(n));
        std.debug.print("\nbench({}) = {any}", .{ n, result });
        try std.testing.expectEqual(result.toInt(), @as(i51, @truncate(benchNative(n))));
    }
}
fn timeObject(n: i64) void {
    _ = benchObject(Object.from(n));
}
var benchThread =
    compileMethod(sym.value, 0, 2, .{
    // i:0 iter:1 size:2 flags:3 prime:4 k:5 count:6
    ":label1",
    &e.pushLiteral,
    8190,
    &e.popIntoTemp,  2, // size
    &e.pushLiteral1,
    &e.popIntoTemp, 1, // iter
    &e.branch,      "label2",

    ":label2",
    &e.pushTemp, 1, // iter
    &e.pushSelf,
    &e.p5, // #'<='
    &e.ifFalse,
    "label12",

    ":label3",
    &e.pushLiteral0,
    &e.popIntoTemp,         6, // count
    &e.pushLiteralIndirect, "0Array",
    &e.pushTemp, 2, // size
    &e.p71, // #new:
    &e.pushLiteralTrue,
    &e.p145, // #atAllPut:
    &e.popIntoTemp,  3, // flags
    &e.pushLiteral1,
    &e.popIntoTemp0, // i
    &e.branch,
    "label4",

    ":label4",
    &e.pushTemp0, // i
    &e.pushTemp, 2, // size
    &e.p5, // #'<='
    &e.ifFalse,
    "label11",

    ":label5",
    &e.pushTemp, 3, // flags
    &e.pushTemp0, // i
    &e.p60, // #at:
    &e.ifFalse,
    "label10",

    ":label6",
    &e.pushTemp0, // i
    &e.pushLiteral1,
    &e.p1,
    &e.popIntoTemp, 4, // prime
    &e.pushTemp0, // i
    &e.pushTemp, 4, // prime
    &e.p1,
    &e.popIntoTemp, 5, // k
    &e.branch,      "label7",

    ":label7",
    &e.pushTemp, 5, // k
    &e.pushTemp, 2, // size
    &e.p5, // #'<='
    &e.ifFalse,
    "label9",

    ":label8",
    &e.pushTemp, 3, // flags
    &e.pushTemp,         5, // k
    &e.pushLiteralFalse,
    &e.p61, // #at:put:
    &e.drop,
    &e.pushTemp, 5, // k
    &e.pushTemp, 4, // prime
    &e.p1,
    &e.popIntoTemp, 5, // k
    &e.branch,      "label7",

    ":label9",
    &e.pushTemp,     6, // count
    &e.pushLiteral1, &e.p1,
    &e.popIntoTemp, 6, // count
    &e.branch,      "label10",

    ":label10",
    &e.pushTemp0, // i
    &e.pushLiteral1,
    &e.p1,
    &e.popIntoTemp0, // i
    &e.branch,
    "label4",

    ":label11",
    &e.pushTemp,     1, // iter
    &e.pushLiteral1, &e.p1,
    &e.popIntoTemp, 1, // iter
    &e.branch,      "label2",

    ":label12",
    &e.pushTemp,  6, // count
    &e.returnTop,
});
test "benchThread" {
    const method = benchThread.asCompiledMethodPtr();
    //    benchThread.update(benchThreadRef,method);
    var n: u32 = 1;
    while (n < 10) : (n += 1) {
        var objs = [_]Object{Object.from(n)};
        var te = TestExecution.new();
        te.init();
        const result = te.run(objs[0..], method);
        std.debug.print("\nbench({}) = {any}", .{ n, result });
        try std.testing.expectEqual(result.len, 1);
        try std.testing.expectEqual(result[0].toInt(), @as(i51, @truncate(benchNative(n))));
    }
}
fn timeThread(n: i64) void {
    const method = benchThread.asCompiledMethodPtr();
    var objs = [_]Object{Object.from(n)};
    var te = TestExecution.new();
    te.init();
    _ = te.run(objs[0..], method);
}
test "benchComp" {
    var method = compileMethod(sym.value, 0, 2, .{
        &benchComp,
    });
    var n: i32 = 1;
    while (n < 20) : (n += 1) {
        var objs = [_]Object{Object.from(n)};
        var te = TestExecution.new();
        te.init();
        const result = te.run(objs[0..], method.asCompiledMethodPtr());
        std.debug.print("bench({}) = {any}\n", .{ n, result });
        try std.testing.expectEqual(result.len, 1);
        try std.testing.expectEqual(result[0].toInt(), @as(i51, @truncate(benchNative(n))));
    }
}
fn timeComp(n: i64) void {
    var method = compileMethod(sym.value, 0, 0, .{
        &benchComp,
    });
    var objs = [_]Object{Object.from(n)};
    var te = TestExecution.new();
    te.init();
    _ = te.run(objs[0..], method.asCompiledMethodPtr());
}
// const b = @import("zag/byte-interp.zig").ByteCode;
// test "benchByte" {
//     var benchByte =
//         compileByteCodeMethod(sym.value,0,0,.{
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
//     const method = benchByte.asCompiledByteCodeMethodPtr();
//     var n:i32 = 1;
//     while (n<10) : (n += 1) {
//         var objs = [_]Object{Object.from(n)};
//         var te =  TestExecution.new();
//         te.init();
//         const result = te.run(objs[0..],method);
//         std.debug.print("bench({}) = {any}\n",.{n,result});
//         try std.testing.expectEqual(result.len,1);
//         try std.testing.expectEqual(result[0].toInt(),@truncate(i51,benchNative(n)));
//     }
// }
//  fn timeByte(n: i64) void {
//      var benchByte =
//          compileByteCodeMethod(sym.value,0,0,.{
//              ":recurse",
//              b.dup,
//              b.pushLiteral, Object.from(2),
//              b.p5,"label1",
//              b.primFailure,
//              ":label1",
//              b.ifFalse,"label3",
//              b.drop,
//              b.pushLiteral, Object.from(1),
//              b.returnNoContext,
//              ":label3",
//              b.pushContext,"^",
//              b.pushTemp1,
//              b.pushLiteral, Object.from(1),
//              b.p2, "label4",
//              b.primFailure,
//              ":label4",
//              b.callLocal, "recurse",
//              b.pushTemp1,
//              b.pushLiteral, Object.from(2),
//              b.p2,"label5",
//              b.primFailure,
//              ":label5",
//              b.callLocal, "recurse",
//              b.p1,"label6",
//              b.primFailure,
//              ":label6",
//              b.returnTop,0,
//      });
//      benchByte.setReferences(&[0]Object{});
//      const method = benchByte.asCompiledMethodPtr();
//      var objs = [_]Object{Object.from(n)};
//      var te = TestExecution.new();
//      te.init();
//      _ = te.run(objs[0..],method);
// }
pub fn timing(runs: u32) !void {
    const ts = std.time.nanoTimestamp;
    try stdout.print("for {} runs\n", .{runs});
    var start = ts();
    _ = benchNative(runs);
    var base = ts() - start;
    try stdout.print("benchNative: {d:8.3}s {d:8.3}ns\n", .{ @as(f64, @floatFromInt(base)) / 1000000000, @as(f64, @floatFromInt(base)) / @as(f64, @floatFromInt(runs)) });
    start = ts();
    _ = timeObject(runs);
    _ = benchThread;
    _ = Object;
    var time = ts() - start;
    try stdout.print("benchObject: {d:8.3}s {d:8.3}ns +{d:6.2}%\n", .{ @as(f64, @floatFromInt(time)) / 1000000000, @as(f64, @floatFromInt(time)) / @as(f64, @floatFromInt(runs)), @as(f64, @floatFromInt(time - base)) * 100.0 / @as(f64, @floatFromInt(base)) });
    start = ts();
    _ = timeComp(runs);
    _ = benchThread;
    _ = Object;
    time = ts() - start;
    try stdout.print("benchComp:   {d:8.3}s {d:8.3}ns +{d:6.2}%\n", .{ @as(f64, @floatFromInt(time)) / 1000000000, @as(f64, @floatFromInt(time)) / @as(f64, @floatFromInt(runs)), @as(f64, @floatFromInt(time - base)) * 100.0 / @as(f64, @floatFromInt(base)) });
    start = ts();
    _ = timeThread(runs);
    time = ts() - start;
    try stdout.print("benchThread:{d:8.3}s {d:8.3}ns +{d:6.2}%\n", .{ @as(f64, @floatFromInt(time)) / 1000000000, @as(f64, @floatFromInt(time)) / @as(f64, @floatFromInt(runs)), @as(f64, @floatFromInt(time - base)) * 100.0 / @as(f64, @floatFromInt(base)) });
    // start=ts();
    // _ = timeByte(runs);
    // time = ts()-start;
    // try stdout.print("benchByte:   {d:8.3}s {d:8.3}ns +{d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time)/@intToFloat(f64,runs),@intToFloat(f64,time-base)*100.0/@intToFloat(f64,base)});
}
pub fn main() !void {
    try timing(40);
}
