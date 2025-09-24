const std = @import("std");
const zag = @import("zag");
const Object = zag.Object;
const MainExecutor = zag.execute.Execution.MainExecutor;
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

const Info = struct {
    mean: usize = 0,
    name: []const u8 = "",
    previous: ?*Info = null,
};

const fibNative = struct {
    const included = true;
    var info = Info{ .name = "Native" };
    fn init() void {}
    fn runIt(comptime _: void, proof: usize) usize {
        return fib(fibN) + proof;
    }
    fn fib(n: u64) u64 {
        if (n <= 2) return n;
        return fib(n - 1) + fib(n - 2);
    }
};

const fibNativeFloat = struct {
    const included = true;
    var info = Info{ .name = "NativeF" };
    fn init() void {}
    fn runIt(comptime _: void, proof: usize) usize {
        const result: usize = @intFromFloat(fib(@floatFromInt(fibN)));
        return result + proof;
    }
    fn fib(n: f64) f64 {
        if (n <= 2) return n;
        return fib(n - 1) + fib(n - 2);
    }
};

const codeAlignment = 64;
const fibInteger = struct {
    const included = true;
    var info = Info{ .name = "Integer" };
    const self = zag.Context.makeVariable(0, 1, .Parameter, &.{});
    const leq = SmallInteger.@"<=".inlined;
    const plus = SmallInteger.@"+".inlined;
    const minus = SmallInteger.@"-".inlined;
    const classes = Object.PackedObject.classes;
    const signature = zag.symbol.signature;
    const nullMethod = zag.dispatch.nullMethod;
    var fib align(codeAlignment) =
        compileMethod(Sym.fibonacci, 0, .SmallInteger, .{
            tf.push,                  self,
            tf.pushLiteral,           "1const",
            tf.inlinePrimitive,       leq,
            tf.classCase,             classes(&.{.False}),
            "false",                  tf.returnSelf,
            ":false",                 tf.push,
            self,                     tf.pushLiteral,
            "0const",                 tf.inlinePrimitive,
            minus,                    tf.send,
            signature(.fibonacci, 0), &nullMethod,
            tf.push,                  self,
            tf.pushLiteral,           "1const",
            tf.inlinePrimitive,       minus,
            tf.send,                  signature(.fibonacci, 0),
            &nullMethod,              tf.inlinePrimitive,
            plus,                     tf.returnTop,
        });
    var exe: MainExecutor = undefined;
    fn init() void {
        exe = MainExecutor.new();
        fib.resolve(&[_]Object{ exe.object(1), exe.object(2) }) catch unreachable;
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
    fn runIt(comptime _: void, proof: usize) usize {
        const obj = exe.sendTo(Sym.fibonacci, exe.object(fibN)) catch unreachable;
        if (obj.nativeU()) |result| {
            return result + proof;
        }
        std.debug.print("fib object: {f}\n", .{obj});
        unreachable;
    }
};

const fibInteger0 = struct {
    const included = true;
    var info = Info{ .name = "Integer0" };
    const self = zag.Context.makeVariable(0, 1, .Parameter, &.{});
    const leq = SmallInteger.@"<=".inlined;
    const plus = SmallInteger.@"+".inlined;
    const minus = SmallInteger.@"-".inlined;
    const classes = Object.PackedObject.classes;
    const signature = zag.symbol.signature;
    const nullMethod = zag.dispatch.nullMethod;
    var fib align(codeAlignment) =
        compileMethod(Sym.fibonacci, 0, .SmallInteger, .{
            tf.push,                  self,
            tf.pushLiteral,           "1const",
            tf.inlinePrimitive,       leq,
            tf.classCase,             classes(&.{.False}),
            "false",                  tf.returnSelf,
            ":false",                 tf.push,
            self,                     tf.pushLiteral,
            "0const",                 tf.inlinePrimitive,
            minus,                    tf.send0,
            signature(.fibonacci, 0), &nullMethod,
            tf.push,                  self,
            tf.pushLiteral,           "1const",
            tf.inlinePrimitive,       minus,
            tf.send0,                 signature(.fibonacci, 0),
            &nullMethod,              tf.inlinePrimitive,
            plus,                     tf.returnTop,
        });
    var exe: MainExecutor = undefined;
    fn init() void {
        exe = MainExecutor.new();
        fib.resolve(&[_]Object{ exe.object(1), exe.object(2) }) catch unreachable;
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
    fn runIt(comptime _: void, proof: usize) usize {
        const obj = exe.sendTo(Sym.fibonacci, exe.object(fibN)) catch unreachable;
        if (obj.nativeU()) |result| {
            return result + proof;
        }
        std.debug.print("fib object: {f}\n", .{obj});
        unreachable;
    }
};

const fibIntegerBr = struct {
    const included = true;
    var info = Info{ .name = "IntegerBr" };
    const self = zag.Context.makeVariable(0, 1, .Parameter, &.{});
    const leq = SmallInteger.@"<=".inlined;
    const plus = SmallInteger.@"+".inlined;
    const minus = SmallInteger.@"-".inlined;
    const classes = Object.PackedObject.classes;
    const signature = zag.symbol.signature;
    const nullMethod = zag.dispatch.nullMethod;
    var fib align(codeAlignment) =
        compileMethod(Sym.fibonacci, 0, .SmallInteger, .{
            tf.push,                  self,
            tf.pushLiteral,           "1const",
            tf.inlinePrimitive,       leq,
            tf.branchFalse,           "false",
            tf.returnSelf,            ":false",
            tf.push,                  self,
            tf.pushLiteral,           "0const",
            tf.inlinePrimitive,       minus,
            tf.send,                  signature(.fibonacci, 0),
            &nullMethod,              tf.push,
            self,                     tf.pushLiteral,
            "1const",                 tf.inlinePrimitive,
            minus,                    tf.send,
            signature(.fibonacci, 0), &nullMethod,
            tf.inlinePrimitive,       plus,
            tf.returnTop,
        });
    var exe: MainExecutor = undefined;
    fn init() void {
        exe = MainExecutor.new();
        fib.resolve(&[_]Object{ exe.object(1), exe.object(2) }) catch unreachable;
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
    fn runIt(comptime _: void, proof: usize) usize {
        const obj = exe.sendTo(Sym.fibonacci, exe.object(fibN)) catch unreachable;
        if (obj.nativeU()) |result| {
            return result + proof;
        }
        std.debug.print("fib object: {f}\n", .{obj});
        unreachable;
    }
};

const fibFloat = struct {
    const included = true;
    var info = Info{ .name = "Float" };
    const self = zag.Context.makeVariable(0, 1, .Parameter, &.{});
    const leq = Float.@"<=".inlined;
    const plus = Float.@"+".inlined;
    const minus = Float.@"-".inlined;
    const classes = Object.PackedObject.classes;
    const signature = zag.symbol.signature;
    const nullMethod = zag.dispatch.nullMethod;
    var fib align(codeAlignment) =
        compileMethod(Sym.fibonacci, 0, .Float, .{
            tf.push,                  self,
            tf.pushLiteral,           "1const",
            tf.inlinePrimitive,       leq,
            tf.branchFalse,           "false",
            tf.returnSelf,            ":false",
            tf.push,                  self,
            tf.pushLiteral,           "0const",
            tf.inlinePrimitive,       minus,
            tf.send,                  signature(.fibonacci, 0),
            &nullMethod,              tf.push,
            self,                     tf.pushLiteral,
            "1const",                 tf.inlinePrimitive,
            minus,                    tf.send,
            signature(.fibonacci, 0), &nullMethod,
            tf.inlinePrimitive,       plus,
            tf.returnTop,
        });
    var exe: MainExecutor = undefined;
    fn init() void {
        exe = MainExecutor.new();
        fib.resolve(&[_]Object{ exe.object(1.0), exe.object(2.0) }) catch unreachable;
        fib.initExecute();
        zag.dispatch.addMethod(@ptrCast(&fib));
        if (zag.config.show_trace) {
            std.debug.print("\n", .{});
            fib.dump();
        } else {
            const obj = exe.sendTo(Sym.fibonacci, Object.from(@as(f64, @floatFromInt(fibN)), null)) catch unreachable;
            if (obj.nativeF()) |threaded| {
                const native: f64 = @floatFromInt(fibCheck(fibN));
                if (threaded != native) {
                    std.debug.print("threaded={}, native={}\n", .{ threaded, native });
                    unreachable;
                }
            }
        }
    }
    fn runIt(comptime _: void, proof: usize) usize {
        _ = exe.sendTo(Sym.fibonacci, exe.object(@as(f64, @floatFromInt(fibN)))) catch unreachable;
        return proof;
    }
};
const print = std.debug.print;
fn showDelta(infos: ?*Info, new: u64, target: []const u8) void {
    if (infos) |info| {
        if (std.mem.eql(u8, info.name, target)) {
            print(" ({s}", .{target});
            const delta = (@as(f64, @floatFromInt(new)) / @as(f64, @floatFromInt(info.mean)) - 1) * 100;
            if (delta > 0.0) {
                print("+{d:3.1}%)", .{delta});
            } else if (delta < 0.0) {
                print("-{d:3.1}%)", .{-delta});
            } else {
                print(" no change)", .{});
            }
            return;
        } else showDelta(info.previous, new, target);
    }
}
fn deltaInfo(previous: ?*Info, new: *Info, arg: []const u8) *Info {
    new.previous = previous;
    for (arg, 0..) |c, i| {
        if (c == '?') {
            showDelta(previous, new.mean, name(arg[i + 1 ..]));
        }
    }
    return new;
}
fn name(original: []const u8) []const u8 {
    for (original, 0..) |c, i| {
        if (c == '?') {
            return original[0..i];
        }
    }
    return original;
}
const Stats = zag.Stats;
pub fn timing(args: []const []const u8, default: bool) !void {
    const eql = std.mem.eql;
    var stat = Stats(void, void, nRuns, warmups, .milliseconds).init();
    var saved: ?*Info = null;
    for (args) |arg| {
        if (eql(u8, arg, "Config")) {
            zag.config.printConfig();
        } else if (eql(u8, arg, "Header")) {
            print("for '{} fibonacci'\n", .{fibN});
            print("          Median   Mean   StdDev  SD/Mean ({} run{s}, {} warmup{s})\n", .{ stat.runs, if (stat.runs != 1) "s" else "", stat.warmups, if (stat.warmups != 1) "s" else "" });
        } else {
            var anyRun = false;
            inline for (&.{ fibNative, fibNativeFloat, fibInteger, fibInteger0, fibIntegerBr, fibFloat }) |benchmark| {
                if (benchmark.included and std.mem.eql(u8, name(arg), benchmark.info.name)) {
                    anyRun = true;
                    print("{s:>9}", .{benchmark.info.name});
                    benchmark.init();
                    stat.reset();
                    stat.time(benchmark.runIt, {});
                    print("{?d:5}ms {d:5}ms {d:6.2}ms", .{ stat.median(), stat.mean(), stat.stdDev() });
                    if (stat.stDevPercent()) |percent|
                        print(" {d:5.1}%", .{percent});
                    benchmark.info.mean = stat.mean();
                    saved = deltaInfo(saved, &benchmark.info, arg);
                    print("\n", .{});
                }
            }
            if (!default and !anyRun)
                print("Unknown argument: {s}\n", .{arg});
        }
    }
}
pub fn main() !void {
    const do_all = [_][]const u8{
        "Config",            "Header",
        "Native",            "NativeF",
        //"Integer",
        "IntegerBr?Integer",
        //"Integer0?Integer",
        "Float",
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
const testRun = zag.config.testRun;
const fibN = if (testRun) 5 else 40;
const nRuns = if (testRun) 1 else 5;
const warmups = if (testRun) 0 else null;
