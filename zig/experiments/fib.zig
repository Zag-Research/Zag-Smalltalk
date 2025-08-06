const std = @import("std");
const zag = @import("zag");
const Object = zag.Object;
const Execution = zag.execute.Execution;
const compileMethod = zag.execute.compileMethod;
const tf = zag.threadedFn.Enum;
const Sym = zag.symbol.symbols;

fn fibNative(n: u32) u64 {
    if (n == 0) return 0;
    if (n == 1) return 1;
    var a: u64 = 0;
    var b: u64 = 1;
    var i: u32 = 2;
    while (i <= n) : (i += 1) {
        const c = a + b;
        a = b;
        b = c;
    }
    return b;
}

const fibInteger = struct {
    const included = true;
    const name = "Integer";
    inline fn doTest() !void {
        fib.setLiterals(&[_]Object{Sym.fibonacci}, Object.empty, null);
        var n: u32 = 1;
        while (n <= testReps) : (n += 1) {
            var objs = [_]Object{Object.from(n)};
            var exe = Execution.new();
            exe.init();
            const result = exe.run(objs[0..], &fib);
            std.debug.print("\nfib({}) = {any}", .{ n, result });
            try std.testing.expectEqual(result.len, 1);
            if (result[0].nativeI()) |int| {
                try std.testing.expectEqual(int, fibNative(n));
            }
        }
    }
    var fib =
        compileMethod(Sym.fibonacci, 0, .SmallInteger, .{
            // ":recurse",
            // tf.push, variable(0,1,.{}), // self
            // tf.pushLiteral, 2, //&e.pushLiteral, two,
            // tf.immediatePrimitive,
            // immediatePrimitive(@"<=",SmallInteger.@"<="), // <= know that self and 2 are definitely integers
            // tf.classCase,
            // classCase(.{ Sym.False }),
            // "label3",
            // tf.push, variable(0,1,.{}), // self
            // tf.returnTop,
            // ":label3",
            // tf.push, variable(0,1,.{}), // self
            // tf.pushLiteral, 1,
            // tf.immediatePrimitive,
            // immediatePrimitive(@"-",SmallInteger.@"-"),
            // tf.send,
            // "0fib",
            // nullMethod,
            // tf.push, variable(0,1,.{}), // self
            // tf.pushLiteral, 2,
            // tf.immediatePrimitive,
            // immediatePrimitive(@"-",SmallInteger.@"-"),
            // tf.send,
            // "0fib",
            // nullMethod,
            // tf.immediatePrimitive,
            // immediatePrimitive(@"+",SmallInteger.@"+"),
            // tf.returnTop,
        });
    fn init() void {
        fib.resolve(&[_]Object{Sym.fibonacci}) catch unreachable;
        zag.dispatch.addMethod(@ptrCast(&fib));
        std.debug.print("fib:\n{}\n", .{fib});
    }
    fn runIt(_: usize, _: usize) usize {
        if ((Execution.mainSendTo(Sym.fibonacci, Object.from(runs, null)) catch unreachable).nativeU()) |result| {
            return result;
        }
        unreachable;
    }
    test "fibInteger" {
        try fibInteger.doTest();
    }
};
const Stats = zag.Stats;
pub fn timing(args: []const []const u8, default: bool) !void {
    const nRuns = 5;
    const eql = std.mem.eql;
    const print = std.debug.print;
    var stat = Stats(void, void, nRuns, .milliseconds).init();
    for (args) |arg| {
        if (eql(u8, arg, "Config")) {
            print("Config {s}dispatch cache\n", .{"no "});
        } else if (eql(u8, arg, "Header")) {
            print("for '{} fibonacci'\n", .{runs});
            print("          Median   Mean   StdDev  SD/Mean ({} runs)\n", .{nRuns});
        } else {
            var anyRun = false;
            inline for (&.{fibInteger}) | benchmark | {
                if (benchmark.included and eql(u8, arg, benchmark.name)) {
                    anyRun = true;
                    print("{s:>9}", .{benchmark.name});
                    benchmark.init();
                    stat.reset();
                    stat.time(benchmark.runIt, void);
                    print("{?d:5}ms {d:5}ms {d:6.2}ms {d:5.1}%\n", .{ stat.median(), stat.mean(), stat.stdDev(), stat.stdDev() * 100 / @as(f64, @floatFromInt(stat.mean())) });
                }
            }
            if (!default and !anyRun)
                print("Unknown argument: {s}\n", .{arg});
        }
    }
}
pub fn main() !void {
    const do_all = [_][]const u8{ "Config", "Header", "Native", "Integer" };
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
const testReps = 10;
const runs: u6 = 40;
