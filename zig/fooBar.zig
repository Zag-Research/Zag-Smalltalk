const std = @import("std");
const debug = std.debug;
const math = std.math;
const stdout = std.io.getStdOut().writer();
const Object = @import("zag/zobject.zig").Object;
const Nil = @import("zag/zobject.zig").Nil;
const indexSymbol = @import("zag/zobject.zig").indexSymbol;
const execute = @import("zag/execute.zig");
const tailCall = config.tailCall;
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
const empty = &[0]Object{};
const Sym = struct {
    @"foo:bar:": Object,
    const ss = heap.compileStrings(.{
        "foo:bar:",
    });
    usingnamespace symbol.symbols;
    fn init() Sym {
        return .{
            .@"foo:bar:" = symbol.intern(ss[0].asObject()),
        };
    }
};
var sym: Sym = undefined;
// foo: p1 bar: p2
//    | l1 l2 l3 |
//    p1 < p2 ifTrue: [ ^ self ].
//    l1 := p2.
//    l2 := p1 \\ p2.
//    l3 := p2 - l2.
//    [ l1 < p1 ] whileTrue: [
//        l1 := l1 + 1.
//        l1 = l3 ifTrue: [ ^ 1 ] ].
//    ^ l1
var @"foo:bar:" =
    compileMethod(Sym.i_1, 5, 2 + 11, .{ // self-7 p1-6 p2-5 l2-4 closureData-3 BCself-2 BC1-1 BC2-0
        &e.verifySelector,
        &e.pushContext,
        "^",
        // define all blocks here
        &e.closureData, 3 + (1 << 12), // local:3 size:1 (offset 1 is l1)
        &e.nonlocalClosure_self, 2, // [^ self] local:2
        &e.blockClosure, "0foo:bar::1", 1 + (1 << 12) + (0 << 20)   + (3 << 32), // local:1, 1 field, no includeContext, closureData at local3
        &e.blockClosure, "1foo:bar::2", 0 + (1 << 12) + (255 << 20) + (3 << 32), // local:0, 1 field, includeContext, closureData at local3
        // all blocks defined by now
        &e.pushLocal, 6, // p1
        &e.popLocalData, 1 + (3 << 12), // p1 (read-only) copy offset 3 in local 1 (field in BC1)
        &e.pushLocal, 6, // p1
        &e.pushLocal, 5, // p2
        &e.send1,      Sym.@"<",
        &e.pushLocal, 2, // [^ self]
        &e.send1,      Sym.@"ifTrue:",
        &e.drop, // discard result from ifTrue: (if it returned)
        &e.pushLocal, 5, // p2
        &e.popLocalData, 3 + (1 << 12), // l1
        &e.pushLocal, 6, // p1
        &e.pushLocal, 5, // p2
        &e.send1,      Sym.@"\\",
        &e.popLocal, 4, // l2
        &e.pushLocal, 5, // p2
        &e.pushLocal, 4, // l2
        &e.send1,      Sym.@"-",
        &e.popLocalData, 0 + (4 << 12), // l3 offset 4 in local 0
        &e.pushLocal, 1, // BC1 [ l1 < p1 ]
        &e.pushLocal, 0, // BC2 [ l1 := ... ]
        &e.send1,      Sym.@"whileTrue:",
        &e.drop,
        &e.pushLocalData, 3 + (1 << 12), // l1
        &e.returnTop,
});
var @"foo:bar::1" =
    // [ l1 < p1 ]
    compileMethod(sym.value, 0, 2, .{ // self-0
    &e.verifySelector,
    &e.pushContext,
    "^",
    &e.pushLocalDataData, 0 + (2 << 12) + (1 << 24), // l1 offset 1 in offset 2 in local 0
    &e.pushLocalData, 0 + (3 << 12), // p1 offset 3 in local 0
    &e.send,          Sym.@"<",
    &e.returnTop,
        //        &e.classCase, "4:falseBranch,5:trueBranch"
        // fall through if not False or True
        //            &e.send, Sym....
        // special case for Booleans
        //        &e.ifTrue, "trueBranch"
        //        &e.ifFalse, "falseBranch"
        // other inlines instead of
        //        &e.send2, Sym.@"ifTrue:ifFalse:"
        //   do
        //        &e.Boolean.@"ifTrue:ifFalse:" // faster way to do theses sends
        // all the ifs, all the whiles, simple values
        // also for +, - , etc. if receiver is known to be SmallInteger
        
});
var @"foo:bar::2" =
    // [ l1 := l1 + 1.
    //   l1 = l3 ifTrue: [ ^ 1 ] ]
    compileMethod(sym.value, @"foo:bar::2-arity", 2 + 4, .{ // self-1 BCone-0
    &e.verifySelector,
    &e.pushContext,
    "^",
    &e.nonlocalClosure_one, 0 + (1 << 12) + (2 << 24), // [^ 1] local:0 context at offset 2 in local 1
    &e.pushLocalDataData, 1 + (3 << 12) + (1 << 24), // l1 offset 1 in offset 3 in local 1
    &e.pushLiteral,       Object.from(1),
    &e.send,              Sym.@"+",
    &e.popLocalDataData, 1 + (3 << 12) + (1 << 24), // l1 offset 1 in offset 3 in local 1
    &e.pushLocalDataData, 1 + (3 << 12) + (1 << 24), // l1 offset 1 in offset 3 in local 1
    &e.pushLocalData, 1 + (4 << 12), // l3 offset 4 in local 1
    &e.send,          Sym.@"=",
    &e.pushLocal, 0, // [^ 1]
    &e.send,      Sym.@"ifTrue:",
    &e.returnTop,
});
const @"foo:bar::2-arity" = 1;
fn initSmalltalk() void {
    primitives.init();
    sym = Sym.init();
    @"foo:bar:".setLiterals(&[_]Object{ sym.@"foo:bar:"}, &[_]Object{ @"foo:bar::1", @"foo:bar::2" });
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
    // start=tstart();
    // _ = timeThread(runs);
    // time = ts()-start;
    // try stdout.print("fibThread: {d:8.3}s +{d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time-base)*100.0/@intToFloat(f64,base)});
}
pub fn main() !void {
    initSmalltalk();
    try timing(40);
}
