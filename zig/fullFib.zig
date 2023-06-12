const std = @import("std");
const debug = std.debug;
const math = std.math;
const stdout = std.io.getStdOut().writer();
const Object = @import("zag/zobject.zig").Object;
const Nil = @import("zag/zobject.zig").Nil;
const indexSymbol = @import("zag/zobject.zig").indexSymbol;
const execute = @import("zag/execute.zig");
const tailCall = execute.tailCall;
const Code = execute.Code;
const compileMethod = execute.compileMethod;
const ContextPtr = execute.CodeContextPtr;
const compileByteCodeMethod = @import("zag/byte-interp.zig").compileByteCodeMethod;
const TestExecution = execute.TestExecution;
const primitives = @import("zag/primitives.zig");
const Process = @import("zag/process.zig").Process;
const symbol =  @import("zag/symbol.zig");
const heap =  @import("zag/heap.zig");

// fibonacci
//	self <= 2 ifTrue: [ ^ 1 ].
//	^ (self - 1) fibonacci + (self - 2) fibonacci
pub fn fibNative(self: i64) i64 {
    if (self <= 2) return 1;
    return fibNative(self-1) + fibNative(self-2);
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
fn initSmalltalk() void {
    primitives.init();
    sym = Sym.init();
    const fibonacci_ = comptime indexSymbol(1);
    var fibonacci =
        compileMethod(sym.fibonacci,1,2,.{
            &e.verifySelector,
            &e.pushContext,"^",
            &e.pushNonlocalBlock_one, // [^ 1]
            &e.popIntoTemp, 0, // block reference
            &e.pushTemp, 1, // self
            &e.pushLiteral, Object.from(2),
            &e.send, Sym.@"<=",
            &e.pushTemp, 0,
            &e.send, Sym.@"ifTrue:",
            
            &e.pushTemp,1, // self
            &e.pushLiteral, Object.from(1),
            &e.send, Sym.@"-",
            &e.send, fibonacci_,
            
            &e.pushTemp,1, // self
            &e.pushLiteral, Object.from(2),
            &e.send, Sym.@"-",
            &e.send, fibonacci_,
            
            &e.send, Sym.@"+",
            &e.returnTop,
    });
    fibonacci.setLiteral(fibonacci_,sym.fibonacci);
}
const i = @import("zag/primitives.zig").inlines;
const e = @import("zag/primitives.zig").embedded;
const p = @import("zag/primitives.zig").primitives;
// test "fibThread" {
//     const method = fibThread.asCompiledMethodPtr();
// //    fibThread.update(fibThreadRef,method);
//     var n:u32 = 1;
//     while (n<10) : (n += 1) {
//         var objs = [_]Object{Object.from(n)};
//         var te =  TestExecution.new();
//         te.init();
//         const result = te.run(objs[0..],method);
//         std.debug.print("\nfib({}) = {any}",.{n,result});
//         try std.testing.expectEqual(result.len,1);
//         try std.testing.expectEqual(result[0].toInt(),@truncate(i51,fibNative(n)));
//     }
// }
// fn timeThread(n: i64) void {
//     const method = fibThread.asCompiledMethodPtr();
//     var objs = [_]Object{Object.from(n)};
//     var te = TestExecution.new();
//     te.init();
//     _ = te.run(objs[0..],method);
// }
const ts=std.time.nanoTimestamp;
fn tstart() i128 {
    const t = ts();
    while (true) {
        const newT = ts();
        if (newT!=t) return newT;
    }
}
pub fn timing(runs: u6) !void {
    try stdout.print("for '{} fibonacci'\n",.{runs});
    var start=tstart();
    _ = fibNative(runs);
    var base = ts()-start;
    try stdout.print("fibNative: {d:8.3}s\n",.{@intToFloat(f64,base)/1000000000});
    // start=tstart();
    // _ = timeThread(runs);
    // time = ts()-start;
    // try stdout.print("fibThread: {d:8.3}s +{d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time-base)*100.0/@intToFloat(f64,base)});
}
pub fn main() !void {
    initSmalltalk();
    std.debug.print("{} {} {}  {}\n",.{sym.fibonacci,sym.fibonacci.hash32(),Sym.value,Sym.value.hash32()});
    try timing(40);
}
