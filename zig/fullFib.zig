const std = @import("std");
const debug = std.debug;
const math = std.math;
const stdout = std.io.getStdOut().writer();
const object = @import("zag/zobject.zig");
const Object = object.Object;
const ClassIndex = object.ClassIndex;
const Nil = @import("zag/zobject.zig").Nil;
const indexSymbol = @import("zag/zobject.zig").indexSymbol;
const dispatch = @import("zag/dispatch.zig");
const execute = @import("zag/execute.zig");
const tailCall = execute.tailCall;
const Code = execute.Code;
const compileMethod = execute.compileMethod;
const ContextPtr = execute.CodeContextPtr;
const compileByteCodeMethod = @import("zag/byte-interp.zig").compileByteCodeMethod;
const TestExecution = execute.TestExecution;
const primitives = @import("zag/primitives.zig");
const Process = @import("zag/process.zig").Process;
const symbol = @import("zag/symbol.zig");
const heap = @import("zag/heap.zig");

// fibonacci
//	self <= 2 ifTrue: [ ^ 1 ].
//	^ (self - 1) fibonacci + (self - 2) fibonacci
pub fn fibNative(self: i64) i64 {
    if (self <= 2) return 1;
    return fibNative(self - 1) + fibNative(self - 2);
}
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
var @"Integer>>fibonacci" =
    compileMethod(Sym.i_1, 0, 2, .{ // self-0
        &e.verifySelector,
        &e.pushContext,
        "^",
        &e.pushLocal,   0, // self
        &e.pushLiteral, Object.from(2),
        &e.send1,       Sym.@"<=",
        &e.BlockClosure.pushNonlocalBlock_one, // [^ 1]
        &e.send1,       Sym.@"ifTrue:",
        &e.drop,
        &e.pushLocal,   0, // self
        &e.pushLiteral, Object.from(1),
        &e.send1,       Sym.@"-",
        &e.send0,       Sym.i_1,
        &e.pushLocal,   0, // self
        &e.pushLiteral, Object.from(2),
        &e.send1,       Sym.@"-",
        &e.send0,       Sym.i_1,
        &e.send1,       Sym.@"+",
        &e.returnTop,
});
fn initSmalltalk() !void {
    const empty = &[0]Object{};
    dispatch.init();
    primitives.init();
    sym = Sym.init();
    @"Integer>>fibonacci".checkFooter();
    @"Integer>>fibonacci".setLiterals(&[_]Object{sym.fibonacci}, empty);
    @"Integer>>fibonacci".checkFooter();
    @"Integer>>+".checkFooter();
    try dispatch.addMethod(ClassIndex.SmallInteger, @"Integer>>+".asCompiledMethodPtr());
    try dispatch.addMethod(ClassIndex.SmallInteger, @"Integer>>-".asCompiledMethodPtr());
    try dispatch.addMethod(ClassIndex.SmallInteger, @"Integer>><=".asCompiledMethodPtr());
    @"Integer>>fibonacci".checkFooter();
    try dispatch.addMethod(ClassIndex.SmallInteger, @"Integer>>fibonacci".asCompiledMethodPtr());
    @"Integer>>fibonacci".checkFooter();
    @"Integer>>fibonacci".asCompiledMethodPtr().checkFooter();
}
const testReps = 10;
const i = @import("zag/primitives.zig").inlines;
const e = @import("zag/primitives.zig").embedded;
const p = @import("zag/primitives.zig").primitives;
test "fibFull" {
    try initSmalltalk();
    const method =  @"Integer>>fibonacci".asCompiledMethodPtr();
    var n:u32 = 1;
    while (n <= testReps) : (n += 1) {
        var objs = [_]Object{Object.from(n)};
        var te =  TestExecution.new();
        te.init();
        const result = te.run(objs[0..],method);
        std.debug.print("\nfib({}) = {any}",.{n,result});
        try std.testing.expectEqual(result.len,1);
        try std.testing.expectEqual(result[0].toInt(),@as(i51,@truncate(fibNative(n))));
    }
}
fn timeFull(n: i64) void {
    initSmalltalk() catch @panic("init");
    const method =  @"Integer>>fibonacci".asCompiledMethodPtr();
    var objs = [_]Object{Object.from(n)};
    var te = TestExecution.new();
    te.init();
    _ = te.run(objs[0..],method);
}
const ts = std.time.nanoTimestamp;
fn tstart() i128 {
    const t = ts();
    while (true) {
        const newT = ts();
        if (newT != t) return newT;
    }
}
pub fn timing(runs: u6) !void {
    try stdout.print("for '{} fibonacci'\n", .{runs});
    var start = tstart();
    _ = fibNative(runs);
    var base = ts() - start;
    try stdout.print("fibNative: {d:8.3}s\n", .{@as(f64, @floatFromInt(base)) / 1000000000});
    start=tstart();
    _ = timeFull(runs);
    var time = ts()-start;
    try stdout.print("fibFull: {d:8.3}s +{d:6.2}%\n",.{@as(f64,@floatFromInt(time))/1000000000,@as(f64,@floatFromInt(time-base))*100.0/@as(f64,@floatFromInt(base))});
}
pub fn main() !void {
    try initSmalltalk();
    std.debug.print("{} {} {}  {}\n", .{ sym.fibonacci, sym.fibonacci.hash32(), Sym.value, Sym.value.hash32() });
    try timing(40);
}
