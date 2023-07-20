const std = @import("std");
const config = @import("zag/config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const debug = std.debug;
const math = std.math;
const stdout = std.io.getStdOut().writer();
const object = @import("zag/zobject.zig");
const Object = object.Object;
const ClassIndex = object.ClassIndex;
const Nil = @import("zag/zobject.zig").Nil;
const execute = @import("zag/execute.zig");
const Code = execute.Code;
const compileMethod = execute.compileMethod;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const ContextPtr = execute.CodeContextPtr;
const compileByteCodeMethod = @import("zag/byte-interp.zig").compileByteCodeMethod;
const TestExecution = execute.TestExecution;
const Process = @import("zag/process.zig").Process;
const uniqueSymbol = @import("zag/symbol.zig").uniqueSymbol;
const symbol = @import("zag/symbol.zig");
const heap = @import("zag/heap.zig");
const dispatch = @import("zag/dispatch.zig");
const empty = &[0]Object{};
const primitives = @import("zag/primitives.zig");
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
const dnu = execute.controlPrimitives.dnu;
const i = primitives.inlines;
const e = primitives.embedded;
const p = primitives.primitives;
const testReps = 7;
var fibCPSM = compileMethod(Sym.i_1, 0, 0, .{&fibCPS});
const fibCPST = @as([*]Code, @ptrCast(&fibCPSM.code[0]));
var fibCPSSendM = compileMethod(Sym.i_1, 0, 0, .{&fibCPSSend});
const fibCPSSendT = @as([*]Code, @ptrCast(&fibCPSSendM.code[0]));
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
    if (!sym.fibonacci.hashEquals(selector)) return @call(tailCall, dnu, .{ pc, sp, process, context, selector });
    if (i.p5N(sp[0], two)) {
        sp[0] = one;
        return @call(tailCall, context.npc, .{ context.tpc, sp, process, context, selector });
    }
    const newContext = context.push(sp, process, fibThread.asCompiledMethodPtr(), 0, 2, 0);
    const newSp = newContext.asObjectPtr() - 1;
    newSp[0] = i.p2L(sp[0], 1) catch return @call(tailCall, pc[10].prim, .{ pc + 11, newSp + 1, process, context, selector });
    newContext.setReturnBoth(fibCPS1, pc + 13); // after first callRecursive
    return @call(tailCall, fibCPS, .{ fibCPST + 1, newSp, process, newContext, selector });
}
fn fibCPS1(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
    const newSp = sp - 1;
    newSp[0] = i.p2L(context.getLocal(0), 2) catch return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector });
    context.setReturnBoth(fibCPS2, pc + 3); // after 2nd callRecursive
    return @call(tailCall, fibCPS, .{ fibCPST + 1, newSp, process, context, selector });
}
fn fibCPS2(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
    const sum = i.p1(sp[1], sp[0]) catch return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector });
    var result = context.pop(process);
    const newSp = result.sp;
    newSp[0] = sum;
    var callerContext = result.ctxt;
    return @call(tailCall, callerContext.npc, .{ callerContext.tpc, newSp, process, callerContext, selector });
}
test "fibCPS" {
    const method = fibCPSM.asCompiledMethodPtr();
    fibCPSM.setLiterals(&[_]Object{sym.fibonacci}, empty);
    var n: i32 = 1;
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
fn timeCPS(n: i64) void {
    const method = fibCPSM.asCompiledMethodPtr();
    fibCPSM.setLiterals(&[_]Object{sym.fibonacci}, empty);
    var objs = [_]Object{Object.from(n)};
    var te = TestExecution.new();
    te.init();
    _ = te.run(objs[0..], method);
}
pub fn fibCPSSend(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
    if (!sym.fibonacci.hashEquals(selector)) return @call(tailCall, dnu, .{ pc, sp, process, context, selector });
    if (i.p5N(sp[0], two)) {
        sp[0] = one;
        return @call(tailCall, context.npc, .{ context.tpc, sp, process, context, selector });
    }
    const newContext = context.push(sp, process, fibThread.asCompiledMethodPtr(), 0, 2, 0);
    const newSp = newContext.asObjectPtr() - 1;
    newSp[0] = i.p2L(sp[0], 1) catch return @call(tailCall, pc[10].prim, .{ pc + 11, newSp + 1, process, context, selector });
    newContext.setReturnBoth(fibCPSSend1, pc + 13); // after first callRecursive
    const newPc = dispatch.lookup(selector, sp[0].get_class());
    return @call(tailCall, newPc[0].prim, .{ newPc + 1, newSp, process, newContext, selector });
}
fn fibCPSSend1(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
    const newSp = sp - 1;
    newSp[0] = i.p2L(context.getLocal(0), 2) catch return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector });
    context.setReturnBoth(fibCPSSend2, pc + 3); // after 2nd callRecursive
    const newPc = dispatch.lookup(selector, sp[0].get_class());
    return @call(tailCall, newPc[0].prim, .{ newPc + 1, newSp, process, context, selector });
}
fn fibCPSSend2(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
    const sum = i.p1(sp[1], sp[0]) catch return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector });
    var result = context.pop(process);
    const newSp = result.sp;
    newSp[0] = sum;
    var callerContext = result.ctxt;
    return @call(tailCall, callerContext.npc, .{ callerContext.tpc, newSp, process, callerContext, selector });
}
fn fibCPSSendSetup() CompiledMethodPtr {
    const fibonacci = fibCPSSendM.asCompiledMethodPtr();
    sym = Sym.init();
    fibCPSSendM.setLiterals(&[_]Object{sym.fibonacci}, empty);
    dispatch.init();
    dispatch.addMethod(ClassIndex.SmallInteger, fibonacci) catch @panic("addMethod failed");
    return fibonacci;
}
test "fibCPSSend" {
    const method = fibCPSSendSetup();
    var n: i32 = 1;
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
fn timeCPSSend(n: i64) void {
    const method = fibCPSSendSetup();
    var objs = [_]Object{Object.from(n)};
    var te = TestExecution.new();
    te.init();
    _ = te.run(objs[0..], method);
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
    fibThread.setLiterals(&[_]Object{sym.fibonacci}, empty);
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
    compileMethod(Sym.i_1, 0, 2, .{
    &e.pushContext,
    "^",
    &e.pushLocal0,
    &e.send0,
    Sym.i_1,
    &e.returnTop,
});
fn fibDispatchSetup() CompiledMethodPtr {
    const fibonacci = fibDispatch.asCompiledMethodPtr();
    const start = fibDispatchStart.asCompiledMethodPtr();
    sym = Sym.init();
    fibDispatch.setLiterals(&[_]Object{sym.fibonacci}, empty);
    fibDispatchStart.setLiterals(&[_]Object{sym.fibonacci}, empty);
    dispatch.init();
    dispatch.addMethod(ClassIndex.SmallInteger, fibonacci) catch @panic("addMethod failed");
    return start;
}
test "fibDispatch" {
    const start = fibDispatchSetup();
    var n: u32 = 1;
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
    const start = fibDispatchSetup();
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
        b.pushLiteral2,
        b.p5,
        b.ifFalse,
        "label3",
        b.drop,
        b.pushLiteral1,
        b.returnNoContext,
        ":label3",
        b.pushContext,
        "^",
        b.pushTemp1,
        b.pushLiteral1,
        b.p2,
        b.callRecursive,
        "recurse",
        b.pushTemp1,
        b.pushLiteral2,
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
var @"Integer>>+" =
    compileMethod(Sym.@"+", 0, 0, .{
        &p.p1,
        &e.primitiveFailed,
});
var @"Integer>>-" =
    compileMethod(Sym.@"-", 0, 0, .{
        &p.p2,
        &e.primitiveFailed,
});
var @"Integer>><=" =
    compileMethod(Sym.@"<=", 0, 0, .{
        &p.p5,
        &e.primitiveFailed,
});
var @"True>>ifTrue:" =
    compileMethod(Sym.@"ifTrue:", 0, 0, .{
        &e.swap,
        &e.drop,
        &e.BlockClosure.value,
        &e.returnNoContext,
});
var @"False>>ifTrue:" =
    compileMethod(Sym.@"ifTrue:", 0, 0, .{
        &e.drop,
        &e.returnNoContext
});
var fibFull =
    compileMethod(Sym.i_1, 0, 2, .{ // self-0
        &e.verifySelector,
        &e.pushContext,
        "^",
        &e.pushLocal,   0, // self
        &e.pushLiteral, Object.from(2),
        &e.send1,       Sym.@"<=",
        &e.BlockClosure.pushNonlocalBlock_one, // [^ 1]
        &e.send1,       Sym.@"ifTrue:",
        &e.drop,
        &e.pushLocal,   0, // self
        &e.pushLiteral, Object.from(1),
        &e.send1,       Sym.@"-",
        &e.send0,       Sym.i_1,
        &e.pushLocal,   0, // self
        &e.pushLiteral, Object.from(2),
        &e.send1,       Sym.@"-",
        &e.send0,       Sym.i_1,
        &e.send1,       Sym.@"+",
        &e.returnTop,
});
fn initSmalltalk() !void {
    dispatch.init();
    primitives.init();
    sym = Sym.init();
    fibFull.setLiterals(&[_]Object{sym.fibonacci}, empty);
    try dispatch.addMethod(ClassIndex.SmallInteger, @"Integer>>+".asCompiledMethodPtr());
    try dispatch.addMethod(ClassIndex.SmallInteger, @"Integer>>-".asCompiledMethodPtr());
    try dispatch.addMethod(ClassIndex.SmallInteger, @"Integer>><=".asCompiledMethodPtr());
    try dispatch.addMethod(ClassIndex.True, @"True>>ifTrue:".asCompiledMethodPtr());
    try dispatch.addMethod(ClassIndex.False, @"False>>ifTrue:".asCompiledMethodPtr());
    try dispatch.addMethod(ClassIndex.SmallInteger, fibFull.asCompiledMethodPtr());
}
test "fibFull" {
    try initSmalltalk();
    const method =  fibFull.asCompiledMethodPtr();
    var n:u32 = 1;
    while (n <= testReps) : (n += 1) {
        var objs = [_]Object{Object.from(n)};
        var te =  TestExecution.new();
        te.init();
        const result = te.run(objs[0..],method);
        std.debug.print("\nfib({}) = {any}",.{n,result});
        try std.testing.expectEqual(result.len,1);
        try std.testing.expectEqual(result[0].toInt(),@as(i51,@truncate(fibNative(n))));
    }
}
fn timeFull(n: i64) void {
    initSmalltalk() catch @panic("init");
    const method =  fibFull.asCompiledMethodPtr();
    var objs = [_]Object{Object.from(n)};
    var te = TestExecution.new();
    te.init();
    _ = te.run(objs[0..],method);
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
    base = time;
    start = tstart();
    _ = timeCPSSend(runs);
    time = ts() - start;
    try stdout.print("fibCPSSend: {d:7.3}s :{d:6.2} x CPS\n", .{ @as(f64, @floatFromInt(time)) / 1000000000, @as(f64, @floatFromInt(time)) / @as(f64, @floatFromInt(base)) });
    start = tstart();
    _ = timeThread(runs);
    time = ts() - start;
    try stdout.print("fibThread:  {d:7.3}s :{d:6.2} x CPS\n", .{ @as(f64, @floatFromInt(time)) / 1000000000, @as(f64, @floatFromInt(time)) / @as(f64, @floatFromInt(base)) });
    start = tstart();
    _ = timeDispatch(runs);
    time = ts() - start;
    try stdout.print("fibDispatch:{d:7.3}s :{d:6.2} x CPS\n", .{ @as(f64, @floatFromInt(time)) / 1000000000, @as(f64, @floatFromInt(time)) / @as(f64, @floatFromInt(base)) });
    start=tstart();
    _ = timeByte(runs);
    time = ts()-start;
    try stdout.print("fibByte:    {d:7.3}s :{d:6.2} x CPS\n",.{@as(f64,@floatFromInt(time))/1000000000,@as(f64,@floatFromInt(time)) / @as(f64,@floatFromInt(base))});
    start=tstart();
    _ = timeFull(runs);
    time = ts()-start;
    try stdout.print("fibFull:    {d:7.3}s :{d:6.2} x CPS\n",.{@as(f64,@floatFromInt(time))/1000000000,@as(f64,@floatFromInt(time)) / @as(f64,@floatFromInt(base))});
}
pub fn main() !void {
    try stdout.print("@sizeOf(fibThread) = {}, @sizeOf(fibByte) = {}\n",.{@sizeOf(@TypeOf(fibThread)), @sizeOf(@TypeOf(fibByte))});
    try timing(40);
}
