const std = @import("std");
const zag = @import("zag");
const config = zag.config;
const trace = config.trace;
const tailCall = config.tailCall;
const Encoding = @TypeOf(config.objectEncoding);
const Object = zag.Object;
const MainExecutor = zag.execute.Execution.MainExecutor;
const compileMethod = zag.execute.compileMethod;
const tf = zag.threadedFn.Enum;
const Sym = zag.symbol.Symbols;
const SmallInteger = zag.primitives.primitives.SmallInteger;
const Float = zag.primitives.primitives.Float;
const PC = zag.execute.PC;
const Process = zag.Process;
const SP = Process.SP;
const Context = zag.Context;
const Extra = Context.Extra;
const Result = zag.execute.Result;
const object = zag.object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;

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
    const exclude: []const Encoding = &[_]Encoding{};
    var info = Info{ .name = "Native" };
    fn init(_: u32) void {}
    fn runIt(fibN: u32, proof: usize) usize {
        return fib(fibN) + proof;
    }
    fn fib(n: u64) u64 {
        if (n <= 2) return n;
        return fib(n - 1) + fib(n - 2);
    }
};

const fibNativeFloat = struct {
    const exclude: []const Encoding = &[_]Encoding{};
    var info = Info{ .name = "NativeF" };
    fn init(_: u32) void {}
    fn runIt(fibN: u32, proof: usize) usize {
        const result: usize = @intFromFloat(fib(@floatFromInt(fibN)));
        return @as(u64, @bitCast(result)) + proof;
    }
    fn fib(n: f64) f64 {
        if (n <= 2) return n;
        return fib(n - 1) + fib(n - 2);
    }
};

const codeAlignment = 64;
const fibInteger = struct {
    const exclude: []const Encoding = &[_]Encoding{.onlyFloat};
    var info = Info{ .name = "Integer" };
    const self = zag.Context.makeVariable(0, 1, .Parameter, &.{});
    const leq = tf.@"inline<=I";
    const plus = tf.@"inline+I";
    const minus = tf.@"inline-I";
    const classes = Object.PackedObject.classes;
    const signature = zag.symbol.signature;
    const nullMethod = zag.dispatch.nullMethod;
    var fib align(codeAlignment) =
        compileMethod(Sym.fibonacci, 0, .SmallInteger, .{
            tf.push,                  self,
            tf.pushLiteral,           "1const",
            leq,                      tf.fail,
            tf.fail,                  tf.classCase,
            classes(&.{.False}),      "false",
            tf.returnSelf,            ":false",
            tf.push,                  self,
            tf.pushLiteral,           "0const",
            minus,                    tf.fail,
            tf.fail,                  tf.send,
            signature(.fibonacci, 0), &nullMethod,
            tf.push,                  self,
            tf.pushLiteral,           "1const",
            minus,                    tf.fail,
            tf.fail,                  tf.send,
            signature(.fibonacci, 0), &nullMethod,
            plus,                     tf.fail,
            tf.fail,                  tf.returnTop,
        });
    var exe: MainExecutor = undefined;
    var one_: Object.StaticObject = undefined;
    var two_: Object.StaticObject = undefined;
    fn init(_: u32) void {
        exe = MainExecutor.new();
        const one = one_.init(1);
        const two = two_.init(2);
        fib.resolve(&[_]Object{ one, two }) catch @panic("Failed to resolve");
        fib.initExecute();
        zag.dispatch.addMethod(@ptrCast(&fib));
        if (zag.config.show_trace) {
            std.debug.print("\n", .{});
            fib.dump();
        }
    }
    fn runIt(fibN: u32, proof: usize) usize {
        const obj = exe.sendTo(Sym.fibonacci.asObject(), exe.object(fibN)) catch unreachable;
        if (obj.nativeI()) |result| {
            return @as(u64, @bitCast(result)) + proof;
        }
        std.log.err("fib object: {f}\n", .{obj});
        unreachable;
    }
};

const fibInteger0 = struct {
    const exclude: []const Encoding = &[_]Encoding{.onlyFloat};
    var info = Info{ .name = "Integer0" };
    const self = zag.Context.makeVariable(0, 1, .Parameter, &.{});
    const leq = tf.@"inline<=I";
    const plus = tf.@"inline+I";
    const minus = tf.@"inline-I";
    const classes = Object.PackedObject.classes;
    const signature = zag.symbol.signature;
    const nullMethod = zag.dispatch.nullMethod;
    var fib align(codeAlignment) =
        compileMethod(Sym.fibonacci, 0, .SmallInteger, .{
            tf.push,                  self,
            tf.pushLiteral,           "1const",
            leq,                      tf.fail,
            tf.fail,                  tf.classCase,
            classes(&.{.False}),      "false",
            tf.returnSelf,            ":false",
            tf.push,                  self,
            tf.pushLiteral,           "0const",
            minus,                    tf.fail,
            tf.fail,                  tf.send0,
            signature(.fibonacci, 0), &nullMethod,
            tf.push,                  self,
            tf.pushLiteral,           "1const",
            minus,                    tf.fail,
            tf.fail,                  tf.send0,
            signature(.fibonacci, 0), &nullMethod,
            plus,                     tf.fail,
            tf.fail,                  tf.returnTop,
        });
    var exe: MainExecutor = undefined;
    var one_: Object.StaticObject = undefined;
    var two_: Object.StaticObject = undefined;
    fn init(_: u32) void {
        exe = MainExecutor.new();
        const one = one_.init(1);
        const two = two_.init(2);
        fib.resolve(&[_]Object{ one, two }) catch @panic("Failed to resolve");
        fib.initExecute();
        zag.dispatch.addMethod(@ptrCast(&fib));
        if (zag.config.show_trace) {
            std.debug.print("\n", .{});
            fib.dump();
        }
    }
    fn runIt(fibN: u32, proof: usize) usize {
        const obj = exe.sendTo(Sym.fibonacci.asObject(), exe.object(fibN)) catch unreachable;
        if (obj.nativeI()) |result| {
            return @as(u64, @bitCast(result)) + proof;
        }
        std.log.err("fib object: {f}\n", .{obj});
        unreachable;
    }
};

const fibIntegerBr = struct {
    const exclude: []const Encoding = &[_]Encoding{.onlyFloat};
    var info = Info{ .name = "IntegerBr" };
    const self = zag.Context.makeVariable(0, 1, .Parameter, &.{});
    const leq = tf.@"inline<=I";
    const plus = tf.@"inline+I";
    const minus = tf.@"inline-I";
    const classes = Object.PackedObject.classes;
    const signature = zag.symbol.signature;
    const nullMethod = zag.dispatch.nullMethod;
    var fib align(codeAlignment) =
        compileMethod(Sym.fibonacci, 0, .SmallInteger, .{
            //            tf.debug,
            tf.push,        self,
            tf.pushLiteral, "1const",
            leq,            tf.fail,
            tf.fail,        tf.branchFalse,
            "false",        tf.returnSelf,
            ":false",       tf.push,
            self,           tf.pushLiteral,
            "0const",       minus,
            tf.fail,        tf.fail,
            tf.send,        signature(.fibonacci, 0),
            &nullMethod,    tf.push,
            self,           tf.pushLiteral,
            "1const",       minus,
            tf.fail,        tf.fail,
            tf.send,        signature(.fibonacci, 0),
            &nullMethod,    plus,
            tf.fail,        tf.fail,
            //            tf.enddebug,
            tf.returnTop,
        });
    var exe: MainExecutor = undefined;
    var one_: Object.StaticObject = undefined;
    var two_: Object.StaticObject = undefined;
    fn init(_: anytype) void {
        exe = MainExecutor.new();
        const one = one_.init(1);
        const two = two_.init(2);
        fib.resolve(&[_]Object{ one, two }) catch @panic("Failed to resolve");
        fib.initExecute();
        zag.dispatch.addMethod(@ptrCast(&fib));
        if (zag.config.show_trace) {
            std.debug.print("\n", .{});
            fib.dump();
        }
        // else {
        //     const threaded = runIt({}, 0);
        //     const native = fibCheck(fibN);
        //     if (threaded != native) {
        //         std.log.err("threaded={}, native={}\n", .{ threaded, native });
        //         unreachable;
        //     }
        // }
    }
    fn runIt(fibN: u32, proof: usize) usize {
        const obj = exe.sendTo(Sym.fibonacci.asObject(), exe.object(fibN)) catch unreachable;
        if (obj.nativeI()) |result| {
            return @as(u64, @bitCast(result)) + proof;
        }
        std.log.err("fib object: {f}\n", .{obj});
        unreachable;
    }
};

const fibIntegerCl = struct {
    const exclude: []const Encoding = &[_]Encoding{.onlyFloat};
    var info = Info{ .name = "IntegerCl" };
    const self = zag.Context.makeVariable(0, 1, .Parameter, &.{});
    const leq = tf.@"inline<=I";
    const plus = tf.@"inline+I";
    const minus = tf.@"inline-I";
    const classes = Object.PackedObject.classes;
    const signature = zag.symbol.signature;
    const nullMethod = zag.dispatch.nullMethod;
    const fromClassI8 = zag.execute.Signature.fromClassI8;
    var TifTrue align(codeAlignment) =
        compileMethod(Sym.@"ifTrue:", 0, .True, .{ tf.dup, tf.value, tf.returnTop });
    var FifTrue align(codeAlignment) =
        compileMethod(Sym.@"ifTrue:", 0, .False, .{tf.returnSelf});
    var fib align(codeAlignment) =
        compileMethod(Sym.fibonacci, 0, .SmallInteger, .{
            //            tf.debug,
            tf.push,                   self,
            tf.pushLiteral,            "1const",
            leq,                       tf.fail,
            tf.fail,                   tf.returnLocalClosure,
            "0const",                  tf.send,
            signature(.@"ifTrue:", 0), &nullMethod,
            tf.drop,                   tf.push,
            self,                      tf.pushLiteral,
            "1const",                  minus,
            tf.fail,                   tf.fail,
            tf.send,                   signature(.fibonacci, 0),
            &nullMethod,               tf.push,
            self,                      tf.pushLiteral,
            "2const",                  minus,
            tf.fail,                   tf.fail,
            tf.send,                   signature(.fibonacci, 0),
            &nullMethod,               plus,
            tf.fail,                   tf.fail,
            //            tf.enddebug,
            tf.returnTop,
        });
    var exe: MainExecutor = undefined;
    var one_: Object.StaticObject = undefined;
    var two_: Object.StaticObject = undefined;
    var zero_: Object.StaticObject = undefined;
    fn init(fibN: u32) void {
        exe = MainExecutor.new();
        const one = one_.init(1);
        const two = two_.init(2);
        const zero = zero_.init(0);
        fib.resolve(&[_]Object{ zero, one, two }) catch @panic("Failed to resolve");
        fib.initExecute();
        TifTrue.resolve(Object.empty) catch @panic("Failed to resolve");
        TifTrue.initExecute();
        FifTrue.resolve(Object.empty) catch @panic("Failed to resolve");
        FifTrue.initExecute();
        zag.dispatch.addMethod(@ptrCast(&fib));
        zag.dispatch.addMethod(@ptrCast(&TifTrue));
        zag.dispatch.addMethod(@ptrCast(&FifTrue));
        if (zag.config.show_trace) {
            std.debug.print("\n", .{});
            std.debug.print("address of one {*}\n", .{&one});
            fib.dump();
            TifTrue.dump();
            FifTrue.dump();
            zag.execute.endMethod.dump();
        } else {
            const threaded = runIt(fibN, 0);
            const native = fibCheck(fibN);
            if (threaded != native) {
                std.log.err("threaded={}, native={}\n", .{ threaded, native });
                unreachable;
            }
        }
    }
    fn runIt(fibN: u32, proof: usize) usize {
        const obj = exe.sendTo(Sym.fibonacci.asObject(), exe.object(fibN)) catch unreachable;
        if (obj.nativeI()) |result| {
            return @as(u64, @bitCast(result)) + proof;
        }
        std.log.err("fib object: {f}\n", .{obj});
        unreachable;
    }
};

const fibFloat = struct {
    const exclude: []const Encoding = &[_]Encoding{.onlyInt};
    var info = Info{ .name = "Float" };
    const self = zag.Context.makeVariable(0, 1, .Parameter, &.{});
    const leq = tf.@"inline<=F";
    const plus = tf.@"inline+F";
    const minus = tf.@"inline-F";
    const classes = Object.PackedObject.classes;
    const signature = zag.symbol.signature;
    const nullMethod = zag.dispatch.nullMethod;
    var fib align(codeAlignment) =
        compileMethod(Sym.fibonacci, 0, .Float, .{
            tf.push,        self,
            tf.pushLiteral, "1const",
            leq,            tf.fail,
            tf.fail,        tf.branchFalse,
            "false",        tf.returnSelf,
            ":false",       tf.push,
            self,           tf.pushLiteral,
            "0const",       minus,
            tf.fail,        tf.fail,
            tf.send,        signature(.fibonacci, 0),
            &nullMethod,    tf.push,
            self,           tf.pushLiteral,
            "1const",       minus,
            tf.fail,        tf.fail,
            tf.send,        signature(.fibonacci, 0),
            &nullMethod,    plus,
            tf.fail,        tf.fail,
            tf.returnTop,
        });
    var exe: MainExecutor = undefined;
    var one_: Object.StaticObject = undefined;
    var two_: Object.StaticObject = undefined;
    fn init(fibN: u32) void {
        exe = MainExecutor.new();
        const one = one_.init(1.0);
        const two = two_.init(2.0);
        fib.resolve(&[_]Object{ one, two }) catch @panic("Failed to resolve");
        fib.initExecute();
        zag.dispatch.addMethod(@ptrCast(&fib));
        if (zag.config.show_trace) {
            std.log.err("\n", .{});
            fib.dump();
        } else {
            const obj = exe.sendTo(Sym.fibonacci.asObject(), exe.object(@as(f64, @floatFromInt(fibN)))) catch unreachable;
            if (obj.nativeF()) |threaded| {
                const native: f64 = @floatFromInt(fibCheck(fibN));
                if (threaded != native) {
                    std.log.err("threaded={}, native={}\n", .{ threaded, native });
                    @panic("mismatch");
                }
            }
        }
    }
    fn runIt(fibN: u32, proof: usize) usize {
        const receiver = exe.object(@as(f64, @floatFromInt(fibN)));
        _ = exe.sendTo(Sym.fibonacci.asObject(), receiver) catch @panic("Error sending message");
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
fn includeFor(benchmark: anytype) bool {
    for (benchmark.exclude) |exclude| {
        if (config.objectEncoding == exclude) return false;
    }
    return true;
}
const Stats = zag.Stats;
pub fn timing(args: []const []const u8, nRuns: usize, fibN: u32, default: bool) !void {
    const eql = std.mem.eql;
    var stat = Stats(u32, void, 100, .milliseconds).init(nRuns, warmups);
    var saved: ?*Info = null;
    for (args) |arg| {
        if (eql(u8, arg, "Config")) {
            zag.config.printConfig();
        } else if (eql(u8, arg, "Header")) {
            print("for '{} fibonacci'\n", .{fibN});
            print("          Median   Mean   StdDev  SD/Mean GeomMean({} run{s}, {} warmup{s})\n", .{ stat.runs, if (stat.runs != 1) "s" else "", stat.warmups, if (stat.warmups != 1) "s" else "" });
        } else {
            var anyRun = false;
            inline for (&.{ fibNative, fibNativeFloat, fibInteger, fibInteger0, fibIntegerBr, fibFloat, fibIntegerCl }) |benchmark| {
                if (includeFor(benchmark) and std.mem.eql(u8, name(arg), benchmark.info.name)) {
                    anyRun = true;
                    print("{s:>9}", .{benchmark.info.name});
                    benchmark.init(fibN);
                    stat.reset();
                    stat.time(benchmark.runIt, fibN);
                    print("{?d:5}ms {d:5}ms {d:6.2}ms", .{ stat.median(), stat.mean(), stat.stdDev() });
                    if (stat.stDevPercent()) |percent|
                        print(" {d:5.1}%", .{percent});
                    print(" {d:5.1}ms", .{stat.geometricMean()});
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
        //"IntegerCnP",
        "Float",
        //"IntegerCl",
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
    var start: usize = 1;
    var nRuns: usize = defaultRuns;
    var fibN: u32 = defaultFib;
    while (args.len > start + 1) {
        if (std.mem.eql(u8, args[start], "--runs") or std.mem.eql(u8, args[start], "-r")) {
            if (std.fmt.parseInt(usize, args[start + 1], 10)) |n| {
                start += 2;
                nRuns = n;
            } else |_| {}
        } else if (std.mem.eql(u8, args[start], "--fib") or std.mem.eql(u8, args[start], "-f")) {
            if (std.fmt.parseInt(usize, args[start + 1], 10)) |n| {
                start += 2;
                fibN = @intCast(n);
            } else |_| {}
        } else break;
    }
    const default = args.len <= start;
    try timing(if (default) @constCast(do_all[0..]) else args[start..], nRuns, fibN, default);
}
const testRun = zag.config.testRun;
const defaultFib = if (testRun) 5 else 36;
const defaultRuns = if (testRun) 1 else 10;
const warmups = if (testRun) 0 else null;
