const std = @import("std");
const zag = @import("zag");
const Object = zag.Object;
const Execution = zag.execute.Execution;
const compileMethod = zag.execute.compileMethod;
const tf = zag.threadedFn.Enum;
const Sym = zag.symbol.symbols;
const SmallInteger = zag.primitives.SmallInteger;
const Float = zag.primitives.Float;

fn fibCheck(n: u32) u64 {
    if (n < 2) return n;
    var a: u64 = 0;
    var b: u64 = 1;
    var i: u32 = 1;
    while (i <= n) : (i += 1) {
        const c = a + b;
        a = b;
        b = c;
    }
    return b;
}

const fibNative = struct {
    const included = true;
    const name = "Native";
    fn init() void {}
    fn runIt(comptime _: void, _: usize) usize {
        _ = fib(fibN);
        return 0;
    }
    fn fib(n: u64) u64 {
        if (n <= 2) return n;
        return fib(n - 1) + fib(n - 2);
    }
};

const fibNativeFloat = struct {
    const included = true;
    const name = "NativeF";
    fn init() void {}
    fn runIt(comptime _: void, _: usize) usize {
        _ = fib(@floatFromInt(fibN));
        return 0;
    }
    fn fib(n: f64) f64 {
        if (n <= 2) return n;
        return fib(n - 1) + fib(n - 2);
    }
};

const fibInteger = struct {
    const included = true;
    const name = "Integer";
    const self = zag.Context.makeVariable(0, 1, .Parameter, &.{});
    const leq = SmallInteger.@"<=".inlined;
    const plus = SmallInteger.@"+".inlined;
    const minus = SmallInteger.@"-".inlined;
    const classes = Object.PackedObject.classes;
    const signature = zag.symbol.signature;
    const nullMethod = zag.dispatch.nullMethod;
    var fib =
        compileMethod(Sym.fibonacci, 0, .SmallInteger, .{
            tf.push,                  self,
            tf.pushLiteral,           2,
            tf.inlinePrimitive,       leq,
            tf.classCase,             classes(&.{.False}),
            "false",                  tf.returnSelf,
            ":false",                 tf.push,
            self,                     tf.pushLiteral,
            1,                        tf.inlinePrimitive,
            minus,                    tf.send,
            signature(.fibonacci, 0), &nullMethod,
            tf.push,                  self,
            tf.pushLiteral,           2,
            tf.inlinePrimitive,       minus,
            tf.send,                  signature(.fibonacci, 0),
            &nullMethod,              tf.inlinePrimitive,
            plus,                     tf.returnTop,
        });
    fn init() void {
        fib.resolve(&[_]Object{}) catch unreachable;
        fib.initExecute();
        zag.dispatch.addMethod(@ptrCast(&fib));
        if (zag.config.show_trace) {
            std.debug.print("\n", .{});
            fib.dump();
        } else {
            const threaded = runIt({}, 0);
            const native = fibCheck(fibN);
            if (threaded != native) {
                std.debug.print("threaded={}, native={}\n", .{ threaded, native });
                unreachable;
            }
        }
    }
    fn runIt(comptime _: void, _: usize) usize {
        const obj = Execution.mainSendTo(Sym.fibonacci, Object.from(fibN, null)) catch unreachable;
        if (obj.nativeU()) |result| {
            return result;
        }
        std.debug.print("fib object: {f}\n", .{obj});
        unreachable;
    }
};

const fibInteger0 = struct {
    const included = true;
    const name = "Integer0";
    const self = zag.Context.makeVariable(0, 1, .Parameter, &.{});
    const leq = SmallInteger.@"<=".inlined;
    const plus = SmallInteger.@"+".inlined;
    const minus = SmallInteger.@"-".inlined;
    const classes = Object.PackedObject.classes;
    const signature = zag.symbol.signature;
    const nullMethod = zag.dispatch.nullMethod;
    var fib =
        compileMethod(Sym.fibonacci, 0, .SmallInteger, .{
            tf.push,                  self,
            tf.pushLiteral,           2,
            tf.inlinePrimitive,       leq,
            tf.classCase,             classes(&.{.False}),
            "false",                  tf.returnSelf,
            ":false",                 tf.push,
            self,                     tf.pushLiteral,
            1,                        tf.inlinePrimitive,
            minus,                    tf.send0,
            signature(.fibonacci, 0), &nullMethod,
            tf.push,                  self,
            tf.pushLiteral,           2,
            tf.inlinePrimitive,       minus,
            tf.send0,                 signature(.fibonacci, 0),
            &nullMethod,              tf.inlinePrimitive,
            plus,                     tf.returnTop,
        });
    fn init() void {
        fib.resolve(&[_]Object{}) catch unreachable;
        fib.initExecute();
        zag.dispatch.addMethod(@ptrCast(&fib));
        if (zag.config.show_trace) {
            std.debug.print("\n", .{});
            fib.dump();
        } else {
            const threaded = runIt({}, 0);
            const native = fibCheck(fibN);
            if (threaded != native) {
                std.debug.print("threaded={}, native={}\n", .{ threaded, native });
                unreachable;
            }
        }
    }
    fn runIt(comptime _: void, _: usize) usize {
        const obj = Execution.mainSendTo(Sym.fibonacci, Object.from(fibN, null)) catch unreachable;
        if (obj.nativeU()) |result| {
            return result;
        }
        std.debug.print("fib object: {f}\n", .{obj});
        unreachable;
    }
};

const fibIntegerBr = struct {
    const included = true;
    const name = "IntegerBr";
    const self = zag.Context.makeVariable(0, 1, .Parameter, &.{});
    const leq = SmallInteger.@"<=".inlined;
    const plus = SmallInteger.@"+".inlined;
    const minus = SmallInteger.@"-".inlined;
    const classes = Object.PackedObject.classes;
    const signature = zag.symbol.signature;
    const nullMethod = zag.dispatch.nullMethod;
    var fib =
        compileMethod(Sym.fibonacci, 0, .SmallInteger, .{
            tf.push,                  self,
            tf.pushLiteral,           2,
            tf.inlinePrimitive,       leq,
            tf.branchFalse,
            "false",                  tf.returnSelf,
            ":false",                 tf.push,
            self,                     tf.pushLiteral,
            1,                        tf.inlinePrimitive,
            minus,                    tf.send0,
            signature(.fibonacci, 0), &nullMethod,
            tf.push,                  self,
            tf.pushLiteral,           2,
            tf.inlinePrimitive,       minus,
            tf.send0,                 signature(.fibonacci, 0),
            &nullMethod,              tf.inlinePrimitive,
            plus,                     tf.returnTop,
        });
    fn init() void {
        fib.resolve(&[_]Object{}) catch unreachable;
        fib.initExecute();
        zag.dispatch.addMethod(@ptrCast(&fib));
        if (zag.config.show_trace) {
            std.debug.print("\n", .{});
            fib.dump();
        } else {
            const threaded = runIt({}, 0);
            const native = fibCheck(fibN);
            if (threaded != native) {
                std.debug.print("threaded={}, native={}\n", .{ threaded, native });
                unreachable;
            }
        }
    }
    fn runIt(comptime _: void, _: usize) usize {
        const obj = Execution.mainSendTo(Sym.fibonacci, Object.from(fibN, null)) catch unreachable;
        if (obj.nativeU()) |result| {
            return result;
        }
        std.debug.print("fib object: {f}\n", .{obj});
        unreachable;
    }
};

const fibFloat = struct {
    const included = true;
    const name = "Float";
    const self = zag.Context.makeVariable(0, 1, .Parameter, &.{});
    const leq = Float.@"<=".inlined;
    const plus = Float.@"+".inlined;
    const minus = Float.@"-".inlined;
    const classes = Object.PackedObject.classes;
    const signature = zag.symbol.signature;
    const nullMethod = zag.dispatch.nullMethod;
    var fib =
        compileMethod(Sym.fibonacci, 0, .Float, .{
            tf.push,                  self,
            tf.pushLiteral,           2.0,
            tf.inlinePrimitive,       leq,
            tf.classCase,             classes(&.{.False}),
            "false",                  tf.returnSelf,
            ":false",                 tf.push,
            self,                     tf.pushLiteral,
            1.0,                      tf.inlinePrimitive,
            minus,                    tf.send,
            signature(.fibonacci, 0), &nullMethod,
            tf.push,                  self,
            tf.pushLiteral,           2.0,
            tf.inlinePrimitive,       minus,
            tf.send,                  signature(.fibonacci, 0),
            &nullMethod,              tf.inlinePrimitive,
            plus,                     tf.returnTop,
        });
    fn init() void {
        fib.resolve(&[_]Object{}) catch unreachable;
        fib.initExecute();
        zag.dispatch.addMethod(@ptrCast(&fib));
        if (zag.config.show_trace) {
            std.debug.print("\n", .{});
            fib.dump();
        } else {
            const obj = Execution.mainSendTo(Sym.fibonacci, Object.from(fibN, null)) catch unreachable;
            if (obj.nativeF()) |threaded| {
                const native: f64 = @floatFromInt(fibCheck(fibN));
                if (threaded != native) {
                    std.debug.print("threaded={}, native={}\n", .{ threaded, native });
                    unreachable;
                }
            }
        }
    }
    fn runIt(comptime _: void, _: usize) usize {
        _ = Execution.mainSendTo(Sym.fibonacci, Object.from(fibN, null)) catch unreachable;
        return 0;
    }
};
const Stats = zag.Stats;
pub fn timing(args: []const []const u8, default: bool) !void {
    const eql = std.mem.eql;
    const print = std.debug.print;
    var stat = Stats(void, void, nRuns, warmups, .milliseconds).init();
    for (args) |arg| {
        if (eql(u8, arg, "Config")) {
            zag.config.printConfig();
        } else if (eql(u8, arg, "Header")) {
            print("for '{} fibonacci'\n", .{fibN});
            print("          Median   Mean   StdDev  SD/Mean ({} run{s}, {} warmup{s})\n", .{ stat.runs, if (stat.runs != 1) "s" else "", stat.warmups, if (stat.warmups != 1) "s" else "" });
        } else {
            var anyRun = false;
            inline for (&.{fibNative, fibNativeFloat, fibInteger, fibInteger0, fibIntegerBr, fibFloat}) |benchmark| {
                if (benchmark.included and eql(u8, arg, benchmark.name)) {
                    anyRun = true;
                    print("{s:>9}", .{benchmark.name});
                    benchmark.init();
                    stat.reset();
                    stat.time(benchmark.runIt, {});
                    print("{?d:5}ms {d:5}ms {d:6.2}ms {?d:5.1}%\n", .{ stat.median(), stat.mean(), stat.stdDev(), stat.stDevPercent() });
                }
            }
            if (!default and !anyRun)
                print("Unknown argument: {s}\n", .{arg});
        }
   }
}
pub fn main() !void {
    const do_all = [_][]const u8{ "Config", "Header", "Native", "NativeF",
        //"Float",
        "Integer",
        "Integer0",
        "IntegerBr",
    };
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
const testRun = zag.config.debugMode or zag.config.show_trace;
const testReps = if (testRun) 1 else 10;
const fibN = if (testRun) 5 else 40;
const nRuns = if (testRun) 1 else 5;
const warmups = if (testRun) 0 else null;
