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
const Sym = zag.symbol.symbols;
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

const Info = struct {
    mean: usize = 0,
    name: []const u8 = "",
    previous: ?*Info = null,
};

const codeAlignment = 64;
const cnpThreaded = struct {
    const exclude: []const Encoding = &[_]Encoding{.onlyFloat};
    var info = Info{ .name = "Threaded" };
    const self = zag.Context.makeVariable(0, 1, .Parameter, &.{});
    const leq = SmallInteger.@"<=".inlined;
    const plus = SmallInteger.@"+".inlined;
    const minus = SmallInteger.@"-".inlined;
    const classes = Object.PackedObject.classes;
    const signature = zag.symbol.signature;
    const nullMethod = zag.dispatch.nullMethod;
    var cnp align(codeAlignment) =
        compileMethod(Sym.value, 0, .SmallInteger, .{
            ":start",
            tf.countDown,
            tf.branchTrue,
            "end",
            tf.branch,
            "start",
            ":end",
        });
    var exe: MainExecutor = undefined;
    var one_: Object.StaticObject = undefined;
    var two_: Object.StaticObject = undefined;
    fn init() void {
        exe = MainExecutor.new();
        const one = one_.init(1);
        const two = two_.init(2);
        cnp.resolve(&[_]Object{ one, two }) catch @panic("Failed to resolve");
        cnp.initExecute();
        zag.dispatch.addMethod(@ptrCast(&cnp));
        if (zag.config.show_trace) {
            std.debug.print("\n", .{});
            cnp.dump();
        } else {
            const threaded = runIt({}, 0);
            _ = threaded;
        }
    }
    fn runIt(comptime _: void, proof: usize) usize {
        const obj = exe.sendTo(Sym.value.asObject(), exe.object(cnpN)) catch unreachable;
        if (obj.nativeI()) |result| {
            return @as(u64, @bitCast(result)) + proof;
        }
        std.log.err("fib object: {f}\n", .{obj});
        unreachable;
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
pub fn timing(args: []const []const u8, default: bool) !void {
    const eql = std.mem.eql;
    var stat = Stats(void, void, nRuns, warmups, .milliseconds).init();
    var saved: ?*Info = null;
    for (args) |arg| {
        if (eql(u8, arg, "Config")) {
            zag.config.printConfig();
        } else if (eql(u8, arg, "Header")) {
            print("for '{} fibonacci'\n", .{cnpN});
            print("          Median   Mean   StdDev  SD/Mean ({} run{s}, {} warmup{s})\n", .{ stat.runs, if (stat.runs != 1) "s" else "", stat.warmups, if (stat.warmups != 1) "s" else "" });
        } else {
            var anyRun = false;
            inline for (&.{cnpThreaded}) |benchmark| {
                if (includeFor(benchmark) and std.mem.eql(u8, name(arg), benchmark.info.name)) {
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
        "Config",   "Header",
        "Threaded",
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
const cnpN = if (testRun) 3 else 10;
const nRuns = if (testRun) 1 else 5;
const warmups = if (testRun) 0 else null;
