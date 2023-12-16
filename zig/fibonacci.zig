const std = @import("std");
const config = @import("zag/config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const dispatchCache = config.dispatchCache;
const cached = if (dispatchCache) "cached" else "not cached";
const debug = std.debug;
const math = std.math;
// const stdout = std.io.getStdOut().writer(); // outside of a functions, stdout causes error on Windows
const object = @import("zag/zobject.zig");
const Object = object.Object;
const ClassIndex = object.ClassIndex;
const Nil = @import("zag/zobject.zig").Nil;
const execute = @import("zag/execute.zig");
const SendCache = execute.SendCache;
const Code = execute.Code;
const PC = execute.PC;
const SP = execute.SP;
const compileMethod = execute.compileMethod;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const ContextPtr = execute.CodeContextPtr;
const compileByteCodeMethod = @import("zag/byte-interp.zig").compileByteCodeMethod;
const TestExecution = execute.TestExecution;
const Process = @import("zag/process.zig").Process;
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
const i = primitives.inlines;
const e = primitives.embedded;
const p = primitives.primitives;
const testReps = 7;

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
    if (i.p5N(self, two)) return one;
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
fn runObject(run: usize) usize {
    if (debugPrint) std.debug.print("{},", .{run});
    const start = tstart();
    _ = fibObject(Object.from(runs)); // convert int 40 to a Zag object first
    return @bitCast(@divTrunc(@as(i64, @truncate(ts() - start)), 1000000));
}

var fibCPSM = compileMethod(Sym.i_0, 0, 0, .{&fibCPS});
const fibCPST = PC.init(&fibCPSM.code[0]);
pub fn fibCPS(_: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
    if (!fibCPSM.selector.selectorEquals(selector)) {
        const dPc = cache.current();
        return @call(tailCall, dPc.prim(), .{ dPc.next(), sp, process, context, selector, cache.next() });
    }
    return @call(tailCall, fibCPS0, .{ fibCPST.next(), sp, process, context, selector, cache });
}
pub fn fibCPS0(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
    if (i.p5N(sp.top, two)) {
        sp.top = one;
        return @call(tailCall, context.npc, .{ context.tpc, sp, process, context, selector, cache });
    }
    const newContext = context.push(sp, process, fibThreadMethod, 0, 2, 0);
    const newSp = newContext.asNewSp().push(i.p2L(sp.top, 1) catch return @call(tailCall, pc.skip(10).prim(), .{ pc.skip(11), newContext.asNewSp().drop(), process, context, selector, cache }));
    newContext.setReturnBoth(fibCPS1, pc.skip(13)); // after first callRecursive
    return @call(tailCall, fibCPS0, .{ fibCPST.next(), newSp, process, newContext, selector, cache });
}
fn fibCPS1(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
    const newSp = sp.push(i.p2L(context.getLocal(0), 2) catch return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector, cache }));
    context.setReturnBoth(fibCPS2, pc.skip(3)); // after 2nd callRecursive
    return @call(tailCall, fibCPS0, .{ fibCPST.next(), newSp, process, context, selector, cache });
}
fn fibCPS2(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
    const sum = i.p1(sp.next, sp.top) catch return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector, cache });
    const result = context.pop(process);
    const newSp = result.sp;
    newSp.top = sum;
    const callerContext = result.ctxt;
    return @call(tailCall, callerContext.npc, .{ callerContext.tpc, newSp, process, callerContext, selector, cache });
}
test "fibCPS" {
    fibCPSM.setLiterals(&[_]Object{sym.fibonacci}, empty, null);
    fibThreadMethod = @ptrCast(&fibThread);
    fibThread.setLiterals(&[_]Object{sym.fibonacci}, empty, null);
    var n: i32 = 1;
    while (n <= testReps) : (n += 1) {
        var objs = [_]Object{Object.from(n)};
        var te = TestExecution.new();
        te.init();
        const result = te.run(objs[0..], &fibCPSM);
        std.debug.print("\nfib({}) = {any}", .{ n, result });
        try std.testing.expectEqual(result.len, 1);
        try std.testing.expectEqual(result[0].toInt(), @as(i51, @truncate(fibNative(n))));
    }
}
fn runCPS(run: usize) usize {
    // For debugging
    const p1 = object.oImm(object.ClassIndex.Symbol, 0xff00_0001);
    _ = p1;
    const fibCPSM2 = compileMethod(Sym.i_0, 0, 0, .{&fibCPS});
    _ = fibCPSM2;
    // end for debugging

    fibThreadMethod = fibThread.asCompiledMethodPtr();
    fibThread.setLiterals(&[_]Object{sym.fibonacci}, empty, null);
    if (debugPrint) std.debug.print("{},", .{run});
    const method = fibCPSM.asCompiledMethodPtr();
    fibCPSM.setLiterals(&[_]Object{sym.fibonacci}, empty, null);
    var objs = [_]Object{Object.from(runs)};
    var te = TestExecution.new();
    te.init();
    const start = tstart();
    _ = te.run(objs[0..], method);
    return @bitCast(@divTrunc(@as(i64, @truncate(ts() - start)), 1000000));
}

var fibCPSSendM = compileMethod(Sym.i_0, 0, 0, .{&fibCPSSend});
const fibCPSSendT = @as([*]Code, @ptrCast(&fibCPSSendM.code[0]));
var fibCPSSendCache = execute.SendCacheStruct.init();
pub fn fibCPSSend(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
    if (!fibCPSSendM.selector.selectorEquals(selector)) {
        const dPc = cache.current();
        return @call(tailCall, dPc.prim(), .{ dPc.next(), sp, process, context, selector, cache.next() });
    }
    if (i.p5N(sp.top, two)) {
        sp.top = one;
        return @call(tailCall, context.npc, .{ context.tpc, sp, process, context, selector, cache });
    }
    const newContext = context.push(sp, process, fibThreadMethod, 0, 2, 0);
    const newSp = newContext.asNewSp();
    newSp.top = i.p2L(sp.top, 1) catch return @call(tailCall, pc.skip(10).prim(), .{ pc.skip(11), newSp.drop(), process, context, selector, cache });
    newContext.setReturnBoth(fibCPSSend1, pc.skip(13)); // after first callRecursive
    const newSelector = if (dispatchCache) selector.withClass(.SmallInteger) else selector;
    const newCache = if (dispatchCache) &fibCPSSendCache else cache;
    const newPc = if (dispatchCache) newCache.current() else dispatch.lookupAddress(newSelector, .SmallInteger);
    return @call(tailCall, newPc.prim(), .{ newPc.next(), newSp, process, newContext, newSelector, if (dispatchCache) newCache.next() else cache });
}
fn fibCPSSend1(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
    const newSp = sp.push(i.p2L(context.getLocal(0), 2) catch return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector, cache }));
    context.setReturnBoth(fibCPSSend2, pc.skip(3)); // after 2nd callRecursive
    const newSelector = if (dispatchCache) selector.withClass(.SmallInteger) else selector;
    const newCache = if (dispatchCache) &fibCPSSendCache else cache;
    const newPc = if (dispatchCache) newCache.current() else dispatch.lookupAddress(newSelector, .SmallInteger);
    return @call(tailCall, newPc.prim(), .{ newPc.next(), newSp, process, context, newSelector, if (dispatchCache) newCache.next() else cache });
}
fn fibCPSSend2(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
    const sum = i.p1(sp.next, sp.top) catch return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector, cache });
    const result = context.pop(process);
    const newSp = result.sp;
    newSp.top = sum;
    const callerContext = result.ctxt;
    return @call(tailCall, callerContext.npc, .{ callerContext.tpc, newSp, process, callerContext, selector, cache });
}
var fibCPSSendStart =
    compileMethod(Sym.value, 0, 2, .{
    &e.pushContext,
    "^",
    &e.pushLocal0,
    &e.send0,
    Sym.i_0,
    &e.returnTop,
});
fn fibCPSSendSetup() CompiledMethodPtr {
    sym = Sym.init();
    dispatch.initClass(.SmallInteger);
    fibThreadMethod = fibThread.asCompiledMethodPtr();
    fibThread.setLiterals(&[_]Object{sym.fibonacci}, empty, null);
    const fibonacci = fibCPSSendM.asCompiledMethodPtr();
    fibCPSSendM.setLiterals(&[_]Object{sym.fibonacci}, empty, null);
    const start = fibCPSSendStart.asCompiledMethodPtr();
    fibCPSSendStart.setLiterals(&[_]Object{sym.fibonacci}, empty, null);
    dispatch.init();
    fibonacci.forDispatch(.SmallInteger);
    return start;
}
test "fibCPSSend" {
    const start = fibCPSSendSetup();
    var n: i32 = 1;
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
fn runCPSSend(run: usize) usize {
    if (debugPrint) std.debug.print("{},", .{run});
    const method = fibCPSSendSetup();
    var objs = [_]Object{Object.from(runs)};
    var te = TestExecution.new();
    te.init();
    const start = tstart();
    _ = te.run(objs[0..], method);
    return @bitCast(@divTrunc(@as(i64, @truncate(ts() - start)), 1000000));
}

var fibThread =
    compileMethod(Sym.i_0, 0, 2, .{
    &e.verifySelector,
    ":recurse",
    &e.dup, // self
    &e.pushLiteral2, //&e.pushLiteral, two,
    &e.SmallInteger.@"<=_N", // <= know that self and 2 are definitely integers
    &e.ifFalse,
    "label3",
    &e.replaceLiteral1, // self
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
var fibThreadMethod: CompiledMethodPtr = undefined;
test "fibThread" {
    fibThreadMethod = @ptrCast(&fibThread);
    fibThread.setLiterals(&[_]Object{sym.fibonacci}, empty, null);
    var n: u32 = 1;
    while (n <= testReps) : (n += 1) {
        var objs = [_]Object{Object.from(n)};
        var te = TestExecution.new();
        te.init();
        const result = te.run(objs[0..], fibThreadMethod);
        std.debug.print("\nfib({}) = {any}", .{ n, result });
        try std.testing.expectEqual(result.len, 1);
        try std.testing.expectEqual(result[0].toInt(), @as(i51, @truncate(fibNative(n))));
    }
}
fn runThread(run: usize) usize {
    if (debugPrint) std.debug.print("{},", .{run});
    fibThreadMethod = fibThread.asCompiledMethodPtr();
    fibThread.setLiterals(&[_]Object{sym.fibonacci}, empty, null);
    var objs = [_]Object{Object.from(runs)};
    var te = TestExecution.new();
    te.init();
    const start = tstart();
    _ = te.run(objs[0..], fibThreadMethod);
    return @bitCast(@divTrunc(@as(i64, @truncate(ts() - start)), 1000000));
}

var fibDispatch =
    compileMethod(Sym.i_0, 0, 2, .{
    &e.verifySelector,
    &e.dup, // self
    &e.pushLiteral2, //&e.pushLiteral, two,
    &e.SmallInteger.@"<=_N", // <= know that self and 2 are definitely integers
    &e.ifFalse,
    "label3",
    &e.replaceLiteral1, // self
    &e.returnNoContext,
    ":label3",
    &e.pushContext,
    "^",
    &e.pushLocal0,
    &e.SmallInteger.@"-_L1", // -1 &e.pushLiteral1,&e.p2,
    &e.send0,

    Sym.i_0,
    &e.pushLocal0,
    &e.SmallInteger.@"-_L2", // -2
    &e.send0,
    Sym.i_0,
    &e.SmallInteger.@"+", // +
    &e.returnTop,
});
var fibDispatchStart =
    compileMethod(Sym.value, 0, 2, .{
    &e.pushContext,
    "^",
    &e.pushLocal0,
    &e.send0,
    Sym.i_0,
    &e.returnTop,
});
fn fibDispatchSetup() CompiledMethodPtr {
    sym = Sym.init();
    dispatch.initClass(.SmallInteger);
    const fibonacci = fibDispatch.asCompiledMethodPtr();
    fibDispatch.setLiterals(&[_]Object{sym.fibonacci}, empty, null);
    const start = fibDispatchStart.asCompiledMethodPtr();
    fibDispatchStart.setLiterals(&[_]Object{sym.fibonacci}, empty, null);
    dispatch.init();
    fibonacci.forDispatch(.SmallInteger);
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
        std.testing.expectEqual(result.len, 1) catch @panic("result.len");
        try std.testing.expectEqual(result[0].toInt(), @as(i51, @truncate(fibNative(n))));
    }
}
fn runDispatch(run: usize) usize {
    if (debugPrint) std.debug.print("{},", .{run});
    const method = fibDispatchSetup();
    var objs = [_]Object{Object.from(runs)};
    var te = TestExecution.new();
    te.init();
    const start = tstart();
    _ = te.run(objs[0..], method);
    return @bitCast(@divTrunc(@as(i64, @truncate(ts() - start)), 1000000));
}

// const b = @import("zag/byte-interp.zig").ByteCode;
// var fibByte =
//     compileByteCodeMethod(Sym.i_0, 0, 2, .{
//     ":recurse",
//     b.dup,
//     b.pushLiteral2,
//     b.p5,
//     b.ifFalse,
//     "label3",
//     b.drop,
//     b.pushLiteral1,
//     b.returnNoContext,
//     ":label3",
//     b.pushContext,
//     "^",
//     b.pushTemp1,
//     b.pushLiteral1,
//     b.p2,
//     b.callRecursive,
//     "recurse",
//     b.pushTemp1,
//     b.pushLiteral2,
//     b.p2,
//     b.callRecursive,
//     "recurse",
//     b.p1,
//     b.returnTop,
// });
// test "fibByte" {
//     sym = Sym.init();
//     const method = fibByte.setLiterals(&[_]Object{sym.fibonacci}, empty, null);
//     var n: i32 = 1;
//     while (n <= testReps) : (n += 1) {
//         var objs = [_]Object{Object.from(n)};
//         var te = TestExecution.new();
//         te.init();
//         const result = te.run(objs[0..], method);
//         std.debug.print("\nfib({}) = {any}", .{ n, result });
//         try std.testing.expectEqual(result.len, 1);
//         try std.testing.expectEqual(result[0].toInt(), @as(i51, @truncate(fibNative(n))));
//     }
// }
// fn runByte(run: usize) usize {
//     if (debugPrint) std.debug.print("{},", .{run});
//     sym = Sym.init();
//     const method = fibByte.setLiterals(&[_]Object{sym.fibonacci}, empty);
//     var objs = [_]Object{Object.from(runs)};
//     var te = TestExecution.new();
//     te.init();
//     const start = tstart();
//     _ = te.run(objs[0..], method);
//     return @bitCast(@divTrunc(@as(i64, @truncate(ts() - start)), 1000000));
// }

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
    &e.verifySelector,
    &e.dropNext,
    &e.BlockClosure.value,
    &e.returnNoContext,
});
var @"False>>ifTrue:" =
    compileMethod(Sym.@"ifTrue:", 0, 0, .{ &e.verifySelector, &e.drop, &e.returnNoContext });
var fibFull =
    compileMethod(Sym.i_0, 0, 2, .{ // self-0
    &e.verifySelector,
    &e.pushContext,
    "^",
    &e.pushLocal,   0, // self
    &e.pushLiteral, Object.from(2),
    &e.send1,       Sym.@"<=",
    &e.BlockClosure.pushNonlocalBlock_one, // [^ 1]
    &e.send1,
    Sym.@"ifTrue:",
    &e.drop,
    &e.pushLocal,   0, // self
    &e.pushLiteral, Object.from(1),
    &e.send1,       Sym.@"-",
    &e.send0,       Sym.i_0,
    &e.pushLocal,   0, // self
    &e.pushLiteral, Object.from(2),
    &e.send1,       Sym.@"-",
    &e.send0,       Sym.i_0,
    &e.send1,       Sym.@"+",
    &e.returnTop,
});
var fibFullStart =
    compileMethod(Sym.value, 0, 2, .{
    &e.pushContext,
    "^",
    &e.pushLocal0,
    &e.send0,
    Sym.i_0,
    &e.returnTop,
});
fn fibFullSetup() CompiledMethodPtr {
    dispatch.init();
    dispatch.initClass(.SmallInteger);
    dispatch.dump(.SmallInteger);
    dispatch.initClass(.True);
    dispatch.initClass(.False);
    primitives.init();
    sym = Sym.init();
    fibFull.setLiterals(&[_]Object{sym.fibonacci}, empty, null);
    fibFullStart.setLiterals(&[_]Object{sym.fibonacci}, empty, null);
    @"Integer>>+".asCompiledMethodPtr().forDispatch(.SmallInteger);
    @"Integer>>-".asCompiledMethodPtr().forDispatch(.SmallInteger);
    @"Integer>><=".asCompiledMethodPtr().forDispatch(.SmallInteger);
    @"True>>ifTrue:".asCompiledMethodPtr().forDispatch(.True);
    @"False>>ifTrue:".asCompiledMethodPtr().forDispatch(.False);
    fibFull.asCompiledMethodPtr().forDispatch(.SmallInteger);

    trace("\nfibFullSetup: code pointers" ++
            \\  @Integer>>+={x}
            \\  @Integer>>-={x}
            \\  @Integer>><=={x}
            \\  @True>>ifTrue:={x}
            \\  @False>>ifTrue:={x}
            \\  fibFull={x}
            ,.{
                @intFromPtr(@"Integer>>+".asCompiledMethodPtr().codePtr()),
                @intFromPtr(@"Integer>>-".asCompiledMethodPtr().codePtr()),
                @intFromPtr(@"Integer>><=".asCompiledMethodPtr().codePtr()),
                @intFromPtr(@"True>>ifTrue:".asCompiledMethodPtr().codePtr()),
                @intFromPtr(@"False>>ifTrue:".asCompiledMethodPtr().codePtr()),
                @intFromPtr(fibFull.asCompiledMethodPtr().codePtr()),
    });
    dispatch.dump(.SmallInteger);
    return @ptrCast(&fibFullStart);
}
test "fibFull" {
    const method = fibFullSetup();
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
const debugPrint = false;
fn runFull(run: usize) usize {
    const method = fibFullSetup();
    var objs = [_]Object{Object.from(runs)};
    var te = TestExecution.new();
    te.init();
    if (debugPrint) std.debug.print("{},", .{run});
    const start = tstart();
    _ = te.run(objs[0..], method);
    return @bitCast(@divTrunc(@as(i64, @truncate(ts() - start)), 1000000));
}

const ts = std.time.nanoTimestamp;
fn tstart() i128 {
    const t = ts();
    while (true) {
        const newT = ts();
        if (newT != t) return newT;
    }
}
fn runNative(run: usize) usize {
    if (debugPrint) std.debug.print("{},", .{run});
    std.debug.print("{s}", .{""});
    const start = tstart();
    _ = fibNative(runs);
    return @bitCast(@divTrunc(@as(i64, @truncate(ts() - start)), 1_000_000));
}
const Stats = @import("zag/utilities/stats.zig").Stats;
pub fn timing(args: [][]const u8, default: bool) !void {
    const nRuns = 5;
    const eql = std.mem.eql;
    const print = std.debug.print;
    var stat = Stats(usize, nRuns).init();
    for (args) |arg| {
        if (eql(u8, arg, "Config")) {
            print("Config {s}dispatch cache, {s}direct dispatch\n",.{if (dispatchCache) "" else "no ",if (config.indirectDispatch) "in" else ""});
        } else if (eql(u8, arg, "Header")) {
            print("for '{} fibonacci'\n", .{runs});
            print("          Median   Mean   StdDev  SD/Mean ({} runs)\n", .{nRuns});
        } else if (eql(u8, arg, "Native")) {
            print("Native:  ", .{});
            stat.run(runNative);
            print("{?d:5}ms {d:5}ms {d:6.2}ms {d:5.1}%\n", .{ stat.median(), stat.mean(), stat.stdDev(), stat.stdDev() * 100 / @as(f64, @floatFromInt(stat.mean())) });
        } else if (eql(u8, arg, "Object")) {
            stat = Stats(usize, nRuns).init();
            print("Object:  ", .{});
            stat.run(runObject);
            print("{?d:5}ms {d:5}ms {d:6.2}ms {d:5.1}%\n", .{ stat.median(), stat.mean(), stat.stdDev(), stat.stdDev() * 100 / @as(f64, @floatFromInt(stat.mean())) });
        } else if (eql(u8, arg, "CPS")) {
            stat = Stats(usize, nRuns).init();
            print("CPS:     ", .{});
            stat.run(runCPS);
            print("{?d:5}ms {d:5}ms {d:6.2}ms {d:5.1}%\n", .{ stat.median(), stat.mean(), stat.stdDev(), stat.stdDev() * 100 / @as(f64, @floatFromInt(stat.mean())) });
        } else if (eql(u8, arg, "CPSSend")) {
            stat = Stats(usize, nRuns).init();
            print("CPSSend: ", .{});
            stat.run(runCPSSend);
            print("{?d:5}ms {d:5}ms {d:6.2}ms {d:5.1}% {s}\n", .{ stat.median(), stat.mean(), stat.stdDev(), stat.stdDev() * 100 / @as(f64, @floatFromInt(stat.mean())), cached });
        } else if (eql(u8, arg, "Thread")) {
            stat = Stats(usize, nRuns).init();
            print("Thread:  ", .{});
            stat.run(runThread);
            print("{?d:5}ms {d:5}ms {d:6.2}ms {d:5.1}%\n", .{ stat.median(), stat.mean(), stat.stdDev(), stat.stdDev() * 100 / @as(f64, @floatFromInt(stat.mean())) });
        } else if (eql(u8, arg, "Dispatch")) {
            stat = Stats(usize, nRuns).init();
            print("Dispatch:", .{});
            stat.run(runDispatch);
            print("{?d:5}ms {d:5}ms {d:6.2}ms {d:5.1}% {s}\n", .{ stat.median(), stat.mean(), stat.stdDev(), stat.stdDev() * 100 / @as(f64, @floatFromInt(stat.mean())), cached });
            // } else if (eql(u8,arg,"Byte")) {
            //     stat = Stats(usize,nRuns).init();
            //     print("Byte:    ", .{});
            //     stat.run(runByte);
            //     print("{?d:5}ms {d:5}ms {d:6.2}ms {d:5.1}%\n", .{ stat.median(), stat.mean(), stat.stdDev(), stat.stdDev()*100/@as(f64,@floatFromInt(stat.mean()))});

        } else if (eql(u8, arg, "Full")) {
            stat = Stats(usize, nRuns).init();
            print("Full:    ", .{});
            stat.run(runFull);
            print("{?d:5}ms {d:5}ms {d:6.2}ms {d:5.1}% {s}\n", .{ stat.median(), stat.mean(), stat.stdDev(), stat.stdDev() * 100 / @as(f64, @floatFromInt(stat.mean())), cached });
        } else if (!default)
            print("Unknown argument: {s}\n", .{arg});
    }
}
pub fn main() !void {
    const do_all = [_][]const u8{ "Config", "Header", "Native", "Object", "CPS", "Thread", "Byte", "CPSSend", "Dispatch", "Full" };
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer {
        const deinit_status = gpa.deinit();
        //fail test; can't try in defer as defer is executed after we return
        if (deinit_status == .leak) @panic("TEST FAIL");
    }
    const args = try std.process.argsAlloc(allocator);
    const default = args.len <= 1;
    try timing(if (default) @constCast(do_all[0..]) else args[1..],default);
}
const runs: u6 = 40;
