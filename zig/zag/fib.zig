const zag = @import("zag.zig");

const intIncluded = true;

const notIncluded = struct {
    const included = false;
};
const fibInteger = if (intIncluded) struct {
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
    test "fibThread" {
        try fibThread.doTest();
    }
} else notIncluded;

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
