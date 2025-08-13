const std = @import("std");
const zag = @import("zag");
const Object = zag.Object;
const Execution = zag.execute.Execution;
const compileMethod = zag.execute.compileMethod;
const tf = zag.threadedFn.Enum;
const Sym = zag.symbol.symbols;
const SmallInteger = zag.primitives.Smallinteger;
fn fibNative(n: u32) u64 {
    if (n < 2) return n;
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
    const self = zag.Context.makeVariable(0, 1, .Parameter, &.{});
    const leq = SmallInteger.@"<=".inlined;
    const plus = SmallInteger.@"+".inlined;
    const minus = SmallInteger.@"-".inlined;
    const classCase = Object.PackedObject.classCase;
    const rawSymbol = zag.symbol.rawSymbol;
    const nullMethod = zag.dispatch.nullMethod;
    var fib =
        compileMethod(Sym.fibonacci, 0, .SmallInteger, .{
            tf.push,                  self,
            tf.pushLiteral,           2,
            tf.inlinePrimitive,       leq,
            tf.classCase,             classCase(&.{.False}),
            "label3",
            tf.returnSelf,
            ":label3",                tf.push,
            self,                     tf.pushLiteral,
            1,                        tf.inlinePrimitive,
            minus,                    tf.send,
            rawSymbol(.fibonacci, 0), &nullMethod,
            tf.push,                  self,
            tf.pushLiteral,           2,
            tf.inlinePrimitive,       minus,
            tf.send,                  rawSymbol(.fibonacci, 0),
            &nullMethod,              tf.inlinePrimitive,
            plus,                     tf.returnTop,
        });
    fn init() void {
        fib.resolve(&[_]Object{}) catch unreachable;
        zag.dispatch.addMethod(@ptrCast(&fib));
    }
    fn runIt(_: usize, _: usize) usize {
        if ((Execution.mainSendTo(Sym.fibonacci, Object.from(fibN, null)) catch unreachable).nativeU()) |result| {
            std.debug.print("fib result: {}\n", .{result});
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
    const nRuns = 1;
    const eql = std.mem.eql;
    const print = std.debug.print;
    var stat = Stats(void, void, nRuns, 0, .milliseconds).init();
    for (args) |arg| {
        if (eql(u8, arg, "Config")) {
            zag.config.printConfig();
        } else if (eql(u8, arg, "Header")) {
            print("for '{} fibonacci'\n", .{fibN});
            print("          Median   Mean   StdDev  SD/Mean ({} run{s}, {} warmup{s})\n", .{ stat.runs, if (stat.runs != 1) "s" else "", stat.warmups, if (stat.warmups != 1) "s" else "" });
        } else {
            var anyRun = false;
            inline for (&.{fibInteger}) |benchmark| {
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
    // var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    // const allocator = gpa.allocator();
    const allocator = std.heap.page_allocator;
    // defer {
    //     const deinit_status = gpa.deinit();
    //     //fail test; can't try in defer as defer is executed after we return
    //     if (deinit_status == .leak) @panic("TEST FAIL");
    // }
    const args = try std.process.argsAlloc(allocator);
    const default = args.len <= 1;
    try timing(if (default) @constCast(do_all[0..]) else args[1..], default);
}
const testReps = 10;
const fibN: u6 = 3;
