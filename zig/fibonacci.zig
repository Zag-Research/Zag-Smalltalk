const std = @import("std");
const debug = std.debug;
const math = std.math;
const stdout = std.io.getStdOut().writer();
const object = @import("zag/zobject.zig");
const Object = object.Object;
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
const symbol = @import("zag/symbol.zig");
const heap = @import("zag/heap.zig");
const dispatch = @import("zag/dispatch.zig");
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
const fibSym = Sym.value;
const dnu = execute.controlPrimitives.dnu;
const i = @import("zag/primitives.zig").inlines;
const e = @import("zag/primitives.zig").embedded;
const p = @import("zag/primitives.zig").primitives;
const testReps = 7;
var fibCPSM = compileMethod(Sym.value, 0, 0, .{&fibCPS});
const fibCPST = @as([*]Code, @ptrCast(&fibCPSM.code[0]));
// fibonacci
//	self <= 2 ifTrue: [ ^ 1 ].
//	^ (self - 1) fibonacci + (self - 2) fibonacci
const callsToFib40 = 204_668_309;
pub fn fibNative(self: i64) i64 {
    // count += 1;
    if (self <= 2) return 1;
    return fibNative(self - 1) + fibNative(self - 2);
}
const one = Object.from(1);
const two = Object.from(2);
pub fn fibObject(self: Object) Object {
    if (i.p5N(self,two)) return one;
    const m1 = i.p2L(self, 1) catch @panic("int subtract failed in fibObject");
    const fm1 = fibObject(m1);
    const m2 = i.p2L(self, 2) catch @panic("int subtract failed in fibObject");
    const fm2 = fibObject(m2);
    return i.p1(fm1, fm2) catch @panic("int add failed in fibObject");
}
test "fibObject" {
    var n: i32 = 1;
    while (n <= testReps) : (n += 1) {
        const result = fibObject(Object.from(n));
        std.debug.print("\nfib({}) = {any}", .{ n, result });
        try std.testing.expectEqual(result.toInt(), @as(i51, @truncate(fibNative(n))));
    }
}
fn timeObject(n: i64) void {
    _ = fibObject(Object.from(n));
}
pub fn fibCPS(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
    if (!fibSym.hashEquals(selector)) return @call(tailCall, dnu, .{ pc, sp, process, context, selector });
    if (i.p5N(sp[0], two)) {
        sp[0] = one;
        return @call(tailCall, context.npc, .{ context.tpc, sp, process, context, selector });
    }
    const newContext = context.push(sp, process, fibThread.asCompiledMethodPtr(), 0, 2, 0);
    const newSp = newContext.asObjectPtr() - 1;
    newSp[0] = i.p2L(sp[0], 1) catch return @call(tailCall, pc[10].prim, .{ pc + 11, newSp + 1, process, context, fibSym });
    newContext.setReturnBoth(fibCPS1, pc + 13); // after first callRecursive
    return @call(tailCall, fibCPS, .{ fibCPST + 1, newSp, process, newContext, fibSym });
}
fn fibCPS1(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, _: Object) [*]Object {
    const newSp = sp - 1;
    newSp[0] = i.p2L(context.getLocal(0), 2) catch return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, fibSym });
    context.setReturnBoth(fibCPS2, pc + 3); // after 2nd callRecursive
    return @call(tailCall, fibCPS, .{ fibCPST + 1, newSp, process, context, fibSym });
}
fn fibCPS2(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
    const sum = i.p1(sp[1], sp[0]) catch return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, fibSym });
    var result = context.pop(process);
    const newSp = result.sp;
    newSp[0] = sum;
    var callerContext = result.ctxt;
    return @call(tailCall, callerContext.npc, .{ callerContext.tpc, newSp, process, callerContext, selector });
}
test "fibCPS" {
    var method = compileMethod(Sym.value, 0, 2, .{
        &fibCPS,
    });
    var n: i32 = 1;
    while (n <= testReps) : (n += 1) {
        var objs = [_]Object{Object.from(n)};
        var te = TestExecution.new();
        te.init();
        const result = te.run(objs[0..], method.asCompiledMethodPtr());
        std.debug.print("\nfib({}) = {any}", .{ n, result });
        try std.testing.expectEqual(result.len, 1);
        try std.testing.expectEqual(result[0].toInt(), @as(i51, @truncate(fibNative(n))));
    }
}
fn timeCPS(n: i64) void {
    var method = compileMethod(Sym.value, 0, 0, .{
        &fibCPS,
    });
    var objs = [_]Object{Object.from(n)};
    var te = TestExecution.new();
    te.init();
    _ = te.run(objs[0..], method.asCompiledMethodPtr());
}
var fibThread =
    compileMethod(Sym.i_1, 0, 2, .{
    &e.verifySelector,
    ":recurse",
    &e.dup, // self
    &e.pushLiteral2, //&e.pushLiteral, two,
    &e.SmallInteger.@"<=_N", // <= know that self and 2 are definitely integers
    &e.ifFalse,
    "label3",
    &e.drop, // self
    &e.pushLiteral1,
    &e.returnNoContext,
    ":label3",
    &e.pushContext,
    "^",
    &e.pushLocal0,
    &e.SmallInteger.@"-_L1", // -1 &e.pushLiteral1,&e.p2,
    &e.callRecursive,
    "recurse",
    &e.pushLocal0,
    &e.SmallInteger.@"-_L2", // -2
    &e.callRecursive,
    "recurse",
    &e.SmallInteger.@"+", // +
    &e.returnTop,
});
test "fibThread" {
    const method = fibThread.asCompiledMethodPtr();
    fibThread.setLiterals(&[_]Object{sym.fibonacci}, empty);
    var n: u32 = 1;
    while (n <= testReps) : (n += 1) {
        var objs = [_]Object{Object.from(n)};
        var te = TestExecution.new();
        te.init();
        const result = te.run(objs[0..], method);
        std.debug.print("\nfib({}) = {any}", .{ n, result });
        try std.testing.expectEqual(result.len, 1);
        try std.testing.expectEqual(result[0].toInt(), @as(i51, @truncate(fibNative(n))));
    }
}
fn timeThread(n: i64) void {
    const method = fibThread.asCompiledMethodPtr();
    var objs = [_]Object{Object.from(n)};
    var te = TestExecution.new();
    te.init();
    _ = te.run(objs[0..], method);
}
var fibDispatch =
    compileMethod(Sym.i_1, 0, 2, .{
    &e.verifySelector,
    &e.dup, // self
    &e.pushLiteral2, //&e.pushLiteral, two,
    &e.SmallInteger.@"<=_N", // <= know that self and 2 are definitely integers
    &e.ifFalse,
    "label3",
    &e.drop, // self
    &e.pushLiteral1,
    &e.returnNoContext,
    ":label3",
    &e.pushContext,
    "^",
    &e.pushLocal0,
    &e.SmallInteger.@"-_L1", // -1 &e.pushLiteral1,&e.p2,
    &e.send0,
    Sym.i_1,
    &e.pushLocal0,
    &e.SmallInteger.@"-_L2", // -2
    &e.send0,
    Sym.i_1,
    &e.SmallInteger.@"+", // +
    &e.returnTop,
});
var fibDispatchStart =
    compileMethod(Sym.value, 0, 2, .{
    &e.pushContext,
    "^",
    &e.pushLocal0,
    &e.send0,
    Sym.i_1,
    &e.returnTop,
});
test "fibDispatch" {
    const fibonacci = fibDispatch.asCompiledMethodPtr();
    const start = fibDispatchStart.asCompiledMethodPtr();
    var n: u32 = 1;
    sym = Sym.init();
    fibDispatch.setLiterals(&[_]Object{sym.fibonacci}, empty);
    fibDispatchStart.setLiterals(&[_]Object{sym.fibonacci}, empty);
    dispatch.init();
    try dispatch.addMethod(object.ClassIndex.SmallInteger, fibonacci);
//    std.debug.print("\nfibDispatch: {*} {*}", .{ &e.verifySelector, fibonacci.codePtr() });
    while (n <= testReps) : (n += 1) {
        var objs = [_]Object{Object.from(n)};
        var te = TestExecution.new();
        te.init();
        const result = te.run(objs[0..], start);
        std.debug.print("\nfib({}) = {any}", .{ n, result });
        try std.testing.expectEqual(result.len, 1);
        try std.testing.expectEqual(result[0].toInt(), @as(i51, @truncate(fibNative(n))));
    }
}
fn timeDispatch(n: i64) void {
    const fibonacci = fibDispatch.asCompiledMethodPtr();
    const start = fibDispatchStart.asCompiledMethodPtr();
    sym = Sym.init();
    fibDispatch.setLiterals(&[_]Object{sym.fibonacci}, empty);
    fibDispatchStart.setLiterals(&[_]Object{sym.fibonacci}, empty);
    dispatch.init();
    dispatch.addMethod(object.ClassIndex.SmallInteger, fibonacci) catch @panic("add failed");
    var objs = [_]Object{Object.from(n)};
    var te = TestExecution.new();
    te.init();
    _ = te.run(objs[0..], start);
}
const b = @import("zag/byte-interp.zig").ByteCode;
var fibByte =
    compileByteCodeMethod(Sym.i_1, 0, 2, .{
        ":recurse",
        b.dup,
        b.pushLiteral,
        two,
        b.p5,
        b.ifFalse,
        "label3",
        b.drop,
        b.pushLiteral,
        one,
        b.returnNoContext,
        ":label3",
        b.pushContext,
        "^",
        b.pushTemp1,
        b.pushLiteral,
        one,
        b.p2,
        b.callRecursive,
        "recurse",
        b.pushTemp1,
        b.pushLiteral,
        two,
        b.p2,
        b.callRecursive,
        "recurse",
        b.p1,
        b.returnTop,
});
 test "fibByte" {
    const method = fibByte.asCompiledMethodPtr();
    sym = Sym.init();
    fibByte.setLiterals(&[_]Object{sym.fibonacci}, empty);
    var n:i32 = 1;
    while (n<=testReps) : (n += 1) {
        var objs = [_]Object{Object.from(n)};
        var te =  TestExecution.new();
        te.init();
        const result = te.run(objs[0..],method);
        std.debug.print("\nfib({}) = {any}",.{n,result});
        try std.testing.expectEqual(result.len,1);
        try std.testing.expectEqual(result[0].toInt(),@as(i51,@truncate(fibNative(n))));
    }
}
fn timeByte(n: i64) void {
    sym = Sym.init();
    fibByte.setLiterals(&[_]Object{sym.fibonacci}, empty);
    const method = fibByte.asCompiledMethodPtr();
    var objs = [_]Object{Object.from(n)};
    var te = TestExecution.new();
    te.init();
    _ = te.run(objs[0..], method);
}
const ts = std.time.nanoTimestamp;
fn tstart() i128 {
    const t = ts();
    while (true) {
        const newT = ts();
        if (newT != t) return newT;
    }
}
pub fn timing(runs: u6) !void {
    try stdout.print("for '{} fibonacci'\n", .{runs});
    var start = tstart();
    _ = fibNative(runs);
    var base = ts() - start;
    try stdout.print("fibNative:  {d:7.3}s\n", .{@as(f64, @floatFromInt(base)) / 1000000000});
    //std.debug.print("calls to fib {}\n",.{count});
    start = tstart();
    _ = timeObject(runs);
    var time = ts() - start;
    try stdout.print("fibObject:  {d:7.3}s +{d:6.2}%\n", .{ @as(f64, @floatFromInt(time)) / 1000000000, @as(f64, @floatFromInt(time - base)) * 100.0 / @as(f64, @floatFromInt(base)) });
    base = time;
    start = tstart();
    _ = timeCPS(runs);
    time = ts() - start;
    try stdout.print("fibCPS:     {d:7.3}s +{d:6.2}%\n", .{ @as(f64, @floatFromInt(time)) / 1000000000, @as(f64, @floatFromInt(time - base)) * 100.0 / @as(f64, @floatFromInt(base)) });
    start = tstart();
    //_ = timeThread(runs);
    time = ts() - start;
    try stdout.print("fibThread:  {d:7.3}s +{d:6.2}% from CPS\n", .{ @as(f64, @floatFromInt(time)) / 1000000000, @as(f64, @floatFromInt(time - base)) * 100.0 / @as(f64, @floatFromInt(base)) });
    start = tstart();
    _ = timeDispatch(runs);
    time = ts() - start;
    try stdout.print("fibDispatch:{d:7.3}s +{d:6.2}% from CPS\n", .{ @as(f64, @floatFromInt(time)) / 1000000000, @as(f64, @floatFromInt(time - base)) * 100.0 / @as(f64, @floatFromInt(base)) });
    start=tstart();
    _ = timeByte(runs);
    time = ts()-start;
    try stdout.print("fibByte:    {d:7.3}s +{d:6.2}% from CPS\n",.{@as(f64,@floatFromInt(time))/1000000000,@as(f64,@floatFromInt(time-base))*100.0/@as(f64,@floatFromInt(base))});
}
pub fn main() !void {
    try timing(40);
}
