const std = @import("std");
const zag = @import("zag");
const config = zag.config;
const trace = config.trace;
const tailCall = config.tailCall;
const Encoding = @TypeOf(config.objectEncoding);
const Object = zag.Object;
const MainExecutor = zag.execute.Execution.MainExecutor;
const CompiledMethod = zag.execute.CompiledMethod;
const Signature = zag.execute.Signature;
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
const ClassIndex = object.ClassIndex;
const primitive = zag.execute.Signature.fromPrimitive;

const codeAlignment = 64;
var allMethods = blk: {
    var methods: [maxMethods]CompiledMethod align(codeAlignment) = undefined;
    for (&methods, 1..) |*method, i| {
        method.signature = zag.symbol.signature(@enumFromInt(i));
    }
    break :blk methods;
};
fn runIt(methods: u32, proof: usize) usize {
    var result: usize = proof;
    var rand: usize = 1;
    for (0..loops) |_| {
        for (allMethods[0..methods]) |method| {
            rand *%= 17;
            const ci: ClassIndex = if (rand & 32 != 0) .False else .True;
            const lookup = switch (config.dispatchChoice) {
                else => zag.dispatch.lookupMethodForClass(ci, method.signature),
                .forTest => dummyLookupMethod(ci, method.signature),
            };
            result += @intFromPtr(lookup);
        }
    }
    return result;
}
fn loadMethods() void {
    for (&allMethods) |*method| {
        switch (config.dispatchChoice) {
            else => {
                zag.dispatch.addMethod(ClassIndex.False, method);
                zag.dispatch.addMethod(ClassIndex.True, method);
            },
            .forTest => {},
        }
    }
}
inline fn dummyLookupMethod(ci: ClassIndex, signature: Signature) *const CompiledMethod {
    return @ptrFromInt(@intFromEnum(ci) + signature.fullHash());
}
const print = std.debug.print;
const Stats = zag.Stats;
pub fn timing(nRuns: usize, _: u32) !void {
    var stat = Stats(u32, void, 100, .milliseconds).init(nRuns, warmups);
    var methods: u32 = 4;
    zag.config.printConfig();
    loadMethods();
    print("loops: {}\n", .{loops});
    print("                Median   Mean   StdDev  SD/Mean GeomMean({} run{s}, {} warmup{s})\n", .{ stat.runs, if (stat.runs != 1) "s" else "", stat.warmups, if (stat.warmups != 1) "s" else "" });
    while (methods <= maxMethods) : (methods = methods * 5 / 2) {
        print("{d:>14} ", .{methods});
        stat.reset();
        stat.time(runIt, methods);
        print("{?d:5}ms {d:5}ms {d:6.2}ms", .{ stat.median(), stat.mean(), stat.stdDev() });
        if (stat.stDevPercent()) |percent|
            print(" {d:5.1}%", .{percent});
        print(" {d:5.1}ms", .{stat.geometricMean()});
        print("\n", .{});
    }
}
var loops: u32 = defaultLoops;
pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    //const allocator = std.heap.page_allocator;
    defer {
        const deinit_status = gpa.deinit();
        //fail test; can't try in defer as defer is executed after we return
        if (deinit_status == .leak) @panic("TEST FAIL");
    }
    const args = try std.process.argsAlloc(allocator);
    var start: usize = 1;
    var nRuns: usize = defaultRuns;
    while (args.len > start + 1) {
        if (std.mem.eql(u8, args[start], "--runs") or std.mem.eql(u8, args[start], "-r")) {
            if (std.fmt.parseInt(usize, args[start + 1], 10)) |n| {
                start += 2;
                nRuns = n;
            } else |_| {}
        } else if (std.mem.eql(u8, args[start], "--loops") or std.mem.eql(u8, args[start], "-l")) {
            if (std.fmt.parseInt(usize, args[start + 1], 10)) |n| {
                start += 2;
                loops = @intCast(n);
            } else |_| {}
        } else break;
    }
    try timing(nRuns, loops);
}
const testRun = zag.config.testRun;
const maxMethods = if (testRun) 32 else 100;
const defaultLoops = if (testRun) 1 else 10000000;
const defaultRuns = if (testRun) 1 else 10;
const warmups = if (testRun) 0 else null;
