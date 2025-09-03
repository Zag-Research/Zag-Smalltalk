const std = @import("std");
const debug = std.debug;
const assert = debug.assert;
const math = std.math;
const zag = @import("../zag.zig");
const config = zag.config;
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const object = zag.object;
const Object = object.Object;
const ClassIndex = object.ClassIndex;
const Nil = object.Nil;
const execute = zag.execute;
const MethodSignature = execute.MethodSignature;
const Code = execute.Code;
const PC = execute.PC;
const SP = execute.SP;
const compileMethod = execute.compileMethod;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const ContextPtr = execute.CodeContextPtr;
const TestExecution = execute.Execution;
const Process = zag.Process;
const symbol = zag.symbol;
const heap = zag.heap;
const empty = &[0]Object{};
const primitives = zag.primitives;

const testReps = 7;

// fibonacci
//    self <= 2 ifTrue: [ ^ 1 ].
//    ^ (self - 1) fibonacci + (self - 2) fibonacci

const fibThread = if (threadIncluded) struct {
    const included = true;
    inline fn doTest() !void {
        fibThreadMethod = @ptrCast(&fibThread);
        fib.setLiterals(&[_]Object{sym.fibonacci}, empty, null);
        var n: u32 = 1;
        while (n <= testReps) : (n += 1) {
            var objs = [_]Object{Object.from(n)};
            var te = TestExecution.new();
            te.init();
            const result = te.run(objs[0..], fibThreadMethod);
            std.debug.print("\nfib({}) = {any}", .{ n, result });
            try std.testing.expectEqual(result.len, 1);
            try std.testing.expectEqual(result[0].toInt(), @as(i51, @truncate(fibNative.fib(n))));
        }
    }
    var fib =
        compileMethod(Sym.i_0, 0, 2, .none, .{
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
    fn runIt(_: usize) void {
        fibThreadMethod = fib.asCompiledMethodPtr();
        fib.setLiterals(&[_]Object{sym.fibonacci}, empty);
        var objs = [_]Object{Object.from(runs)};
        var te = TestExecution.new();
        te.init();
        _ = te.run(objs[0..], fibThreadMethod);
    }
} else notIncluded;
test "fibThread" {
    try fibThread.doTest();
}

const fibDispatch = if (dispatchIncluded) struct {
    const included = true;
    inline fn doTest() !void {
        const start = setup();
        var n: u32 = 1;
        while (n <= testReps) : (n += 1) {
            var objs = [_]Object{Object.from(n)};
            var te = TestExecution.new();
            te.init();
            const result = te.run(objs[0..], start);
            std.debug.print("\nfib({}) = {any}", .{ n, result });
            std.testing.expectEqual(result.len, 1) catch @panic("result.len");
            try std.testing.expectEqual(result[0].toInt(), @as(i51, @truncate(fibNative.fib(n))));
        }
    }
    var fib =
        compileMethod(Sym.i_0, 0, 2, .none, .{
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
    fn setup() CompiledMethodPtr {
        sym = Sym.init();
        execute.initClass(.SmallInteger);
        const fibonacci = fib.asCompiledMethodPtr();
        fib.setLiterals(&[_]Object{sym.fibonacci}, empty);
        const start = fibDispatchStart.asCompiledMethodPtr();
        fibDispatchStart.setLiterals(&[_]Object{sym.fibonacci}, empty);
        execute.init();
        fibonacci.forDispatch(.SmallInteger);
        return start;
    }
    fn runIt(_: usize) void {
        const method = setup();
        var objs = [_]Object{Object.from(runs)};
        var te = TestExecution.new();
        te.init();
        _ = te.run(objs[0..], method);
    }
} else notIncluded;
test "fibDispatch" {
    try fibDispatch.doTest();
}

const fibByte = if (byteIncluded) struct {
    const included = true;
    inline fn doTest() !void {
        sym = Sym.init();
        const method = fib.setLiterals(&[_]Object{sym.fibonacci}, empty, null);
        var n: i32 = 1;
        while (n <= testReps) : (n += 1) {
            var objs = [_]Object{Object.from(n)};
            var te = TestExecution.new();
            te.init();
            const result = te.run(objs[0..], method);
            std.debug.print("\nfib({}) = {any}", .{ n, result });
            try std.testing.expectEqual(result.len, 1);
            try std.testing.expectEqual(result[0].toInt(), @as(i51, @truncate(fibNative.fib(n))));
        }
    }
    const b = @import("zag/byte-interp.zig").ByteCode;
    var fib =
        compileByteCodeMethod(Sym.i_0, 0, 2, .{
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
    fn runIt(_: usize) void {
        sym = Sym.init();
        const method = fib.setLiterals(&[_]Object{sym.fibonacci}, empty);
        var objs = [_]Object{Object.from(runs)};
        var te = TestExecution.new();
        te.init();
        _ = te.run(objs[0..], method);
    }
} else notIncluded;
test "fibByte" {
    try fibByte.doTest();
}

const fibFull = if (fullIncluded) struct {
    const included = true;
    inline fn doTest() !void {
        const method = fibFull.setup();
        var n: u32 = 1;
        while (n <= testReps) : (n += 1) {
            var objs = [_]Object{Object.from(n)};
            var te = TestExecution.new();
            te.init();
            const result = te.run(objs[0..], method);
            std.debug.print("\nfib({}) = {any}", .{ n, result });
            try std.testing.expectEqual(result.len, 1);
            try std.testing.expectEqual(result[0].toInt(), @as(i51, @truncate(fibNative.fib(n))));
        }
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
            &e.dropNext,
            &e.BlockClosure.value,
            &e.returnNoContext,
        });
    var @"False>>ifTrue:" =
        compileMethod(Sym.@"ifTrue:", 0, 0, .{ &e.drop, &e.returnNoContext });
    var fib =
        compileMethod(Sym.i_0, 0, 2, .none, .{ // self-0
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
    fn setup() CompiledMethodPtr {
        execute.init();
        execute.initClass(.SmallInteger);
        execute.dump(.SmallInteger);
        execute.initClass(.True);
        execute.initClass(.False);
        primitives.init();
        sym = Sym.init();
        fib.setLiterals(&[_]Object{sym.fibonacci}, empty, null);
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
        , .{
            @intFromPtr(@"Integer>>+".asCompiledMethodPtr().codePtr()),
            @intFromPtr(@"Integer>>-".asCompiledMethodPtr().codePtr()),
            @intFromPtr(@"Integer>><=".asCompiledMethodPtr().codePtr()),
            @intFromPtr(@"True>>ifTrue:".asCompiledMethodPtr().codePtr()),
            @intFromPtr(@"False>>ifTrue:".asCompiledMethodPtr().codePtr()),
            @intFromPtr(fibFull.asCompiledMethodPtr().codePtr()),
        });
        execute.dump(.SmallInteger);
        return @ptrCast(&fibFullStart);
    }
    fn runIt(_: usize) void {
        const method = fibFull.setup();
        var objs = [_]Object{Object.from(runs)};
        var te = TestExecution.new();
        te.init();
        _ = te.run(objs[0..], method);
    }
} else notIncluded;
test "fibFull" {
    try fibFull.doTest();
}

const ts = std.time.nanoTimestamp;
fn tstart() i128 {
    const t = ts();
    while (true) {
        const newT = ts();
        if (newT != t) return newT;
    }
}
const Stats = @import("zag/utilities/stats.zig").Stats;
pub fn timing(args: [][]const u8, default: bool) !void {
    const nRuns = 5;
    const eql = std.mem.eql;
    const print = std.debug.print;
    var stat = Stats(usize, nRuns, .milliseconds).init();
    const cached = "";
    for (args) |arg| {
        if (eql(u8, arg, "Config")) {
            print("Config {s}dispatch cache\n", .{"no "});
        } else if (eql(u8, arg, "Header")) {
            print("for '{} fibonacci'\n", .{runs});
            print("          Median   Mean   StdDev  SD/Mean ({} runs)\n", .{nRuns});
        } else if (fibNative.included and eql(u8, arg, "Native")) {
            print("Native:  ", .{});
            stat.reset();
            stat.time(fibNative.runIt);
            print("{?d:5}ms {d:5}ms {d:6.2}ms {d:5.1}%\n", .{ stat.median(), stat.mean(), stat.stdDev(), stat.stdDev() * 100 / @as(f64, @floatFromInt(stat.mean())) });
        } else if (fibObject.included and eql(u8, arg, "Object")) {
            print("Object:  ", .{});
            stat.reset();
            stat.time(fibObject.runIt);
            print("{?d:5}ms {d:5}ms {d:6.2}ms {d:5.1}%\n", .{ stat.median(), stat.mean(), stat.stdDev(), stat.stdDev() * 100 / @as(f64, @floatFromInt(stat.mean())) });
        } else if (fibCPS.included and eql(u8, arg, "CPS")) {
            print("CPS:     ", .{});
            stat.reset();
            stat.time(fibCPS.runIt);
            print("{?d:5}ms {d:5}ms {d:6.2}ms {d:5.1}%\n", .{ stat.median(), stat.mean(), stat.stdDev(), stat.stdDev() * 100 / @as(f64, @floatFromInt(stat.mean())) });
        } else if (fibCPSSend.included and eql(u8, arg, "CPSSend")) {
            print("CPSSend: ", .{});
            stat.reset();
            stat.time(fibCPSSend.runIt);
            print("{?d:5}ms {d:5}ms {d:6.2}ms {d:5.1}% {s}\n", .{ stat.median(), stat.mean(), stat.stdDev(), stat.stdDev() * 100 / @as(f64, @floatFromInt(stat.mean())), cached });
        } else if (fibThread.included and eql(u8, arg, "Thread")) {
            print("Thread:  ", .{});
            stat.reset();
            stat.time(fibThread.runIt);
            print("{?d:5}ms {d:5}ms {d:6.2}ms {d:5.1}%\n", .{ stat.median(), stat.mean(), stat.stdDev(), stat.stdDev() * 100 / @as(f64, @floatFromInt(stat.mean())) });
        } else if (fibDispatch.included and eql(u8, arg, "Dispatch")) {
            print("Dispatch:", .{});
            stat.reset();
            stat.time(fibDispatch.runIt);
            print("{?d:5}ms {d:5}ms {d:6.2}ms {d:5.1}% {s}\n", .{ stat.median(), stat.mean(), stat.stdDev(), stat.stdDev() * 100 / @as(f64, @floatFromInt(stat.mean())), cached });
            // } else if (fibByte.included and eql(u8,arg,"Byte")) {
            //     print("Byte:    ", .{});
            //     stat.reset();
            //     stat.time(fibByte.runIt);
            //     print("{?d:5}ms {d:5}ms {d:6.2}ms {d:5.1}%\n", .{ stat.median(), stat.mean(), stat.stdDev(), stat.stdDev()*100/@as(f64,@floatFromInt(stat.mean()))});

        } else if (fibFull.included and eql(u8, arg, "Full")) {
            print("Full:    ", .{});
            stat.reset();
            stat.time(fibFull.runIt);
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
    try timing(if (default) @constCast(do_all[0..]) else args[1..], default);
}
const runs: u6 = 40;
