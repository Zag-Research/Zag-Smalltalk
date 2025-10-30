const std = @import("std");
const config = @import("zag/config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const debug = std.debug;
const math = std.math;
// const stdout = std.io.getStdOut().writer(); // outside of a functions, stdout causes error on Windows
const object = @import("zag/zobject.zig");
const Object = object.Object;
const ClassIndex = object.ClassIndex;
const Nil = @import("zag/zobject.zig").Nil;
const execute = @import("zag/execute.zig");
const Code = execute.Code;
const PC = execute.PC;
const SP = execute.SP;
const compileMethod = execute.compileMethod;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const ContextPtr = execute.CodeContextPtr;
const compileByteCodeMethod = @import("zag/byte-interp.zig").compileByteCodeMethod;
const TestExecution = execute.Execution;
const Process = @import("zag/process.zig").Process;
const symbol = @import("zag/symbol.zig");
const heap = @import("zag/heap.zig");
const empty = &[0]Object{};
const primitives = @import("zag/primitives.zig");
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
const i = primitives.inlines;
const e = primitives.embedded;
const p = primitives.primitives;
const testReps = 7;

// fibonacci
//    self <= 2 ifTrue: [ ^ 1 ].
//    ^ (self - 1) fibonacci + (self - 2) fibonacci

const callsToFib40 = 204_668_309;
pub fn fibNative(self: i64) i64 {
    // count += 1;
    if (self <= 2) return 1;
    return fibNative(self - 1) + fibNative(self - 2);
}
const one = Object.from(1);
const two = Object.from(2);
pub fn fibObject(self: Object) Object {
    if (i.p5N(self, two)) return one;
    const m1 = i.p2L(self, 1) catch @panic("int subtract failed in fibObject");
    const fm1 = fibObject(m1);
    const m2 = i.p2L(self, 2) catch @panic("int subtract failed in fibObject");
    const fm2 = fibObject(m2);
    return i.p1(fm1, fm2) catch @panic("int add failed in fibObject");
}
test "fibObject" {
    var n: i32 = 1;
    while (n <= testReps) : (n += 1) {
        const result = fibObject(Object.from(n));
        std.log.err("\nfib({}) = {any}", .{ n, result });
        try std.testing.expectEqual(result.toInt(), @as(i51, @truncate(fibNative(n))));
    }
}
fn runObject(_: usize) void {
    _ = fibObject(Object.from(runs)); // convert int 40 to a Zag object first
}

var fibCPSM = compileMethod(Sym.i_0, 0, 0, .{&fibCPS});
const fibCPST = PC.init(&fibCPSM.code[0]);
pub fn fibCPS(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object) callconv(stdCall) SP {
    if (!fibCPSM.selector.selectorEquals(selector)) {
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector });
    }
    return @call(tailCall, fibCPS0, .{ fibCPST.next(), sp, process, context, selector });
}
pub fn fibCPS0(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object) callconv(stdCall) SP {
    if (i.p5N(sp.top, two)) {
        sp.top = one;
        return @call(tailCall, context.npc, .{ context.tpc, sp, process, context, selector });
    }
    const newContext = context.push(sp, process, fibThreadMethod, 0, 2, 0);
    const newSp = newContext.asNewSp().push(i.p2L(sp.top, 1) catch return @call(tailCall, pc.skip(10).prim(), .{ pc.skip(11), newContext.asNewSp().drop(), process, context, selector }));
    newContext.setReturnBoth(fibCPS1, pc.skip(13)); // after first callRecursive
    return @call(tailCall, fibCPS0, .{ fibCPST.next(), newSp, process, newContext, selector });
}
fn fibCPS1(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object) callconv(stdCall) SP {
    const newSp = sp.push(i.p2L(context.getLocal(0), 2) catch return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector }));
    context.setReturnBoth(fibCPS2, pc.skip(3)); // after 2nd callRecursive
    return @call(tailCall, fibCPS0, .{ fibCPST.next(), newSp, process, context, selector });
}
fn fibCPS2(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object) callconv(stdCall) SP {
    const sum = i.p1(sp.next, sp.top) catch return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector });
    const result = context.pop(process);
    const newSp = result.sp;
    newSp.top = sum;
    const callerContext = result.ctxt;
    return @call(tailCall, callerContext.npc, .{ callerContext.tpc, newSp, process, callerContext, selector });
}
fn runCPS(_: usize) void {
    fibThreadMethod = fibThread.asCompiledMethodPtr();
    fibThread.setLiterals(&[_]Object{sym.fibonacci}, empty);
    const method = fibCPSM.asCompiledMethodPtr();
    fibCPSM.setLiterals(&[_]Object{sym.fibonacci}, empty);
    var objs = [_]Object{Object.from(runs)};
    var te = TestExecution.new();
    te.init();
    _ = te.run(objs[0..], method);
}
var fibThread =
    compileMethod(Sym.i_0, 0, 2, .{
        ":recurse",
        &e.dup, // self
        &e.pushLiteral2, //&e.pushLiteral, two,
        &e.SmallInteger.@"<=_N", // <= know that self and 2 are definitely integers
        &e.ifFalse,
        "label3",
        &e.replaceLiteral1, // self
        &e.returnNoContext,
        ":label3",
        &e.pushContext,
        "^",
        &e.pushLocal0,
        &e.SmallInteger.@"-_L1", // -1 &e.pushLiteral1,&e.p2,
        &e.callRecursive,
        "recurse",
        &e.pushLocal0,
        &e.SmallInteger.@"-_L2", // -2
        &e.callRecursive,
        "recurse",
        &e.SmallInteger.@"+", // +
        &e.returnTop,
    });
var fibThreadMethod: CompiledMethodPtr = undefined;

pub fn main() !void {
    runCPS(0);
}
const runs: u6 = 40;
