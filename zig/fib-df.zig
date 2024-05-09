const std = @import("std");
const config = @import("zag/config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const heap = @import("zag/heap.zig");
const symbol = @import("zag/symbol.zig");
const primitives = @import("zag/primitives.zig");
const object = @import("zag/zobject.zig");
const Object = object.Object;
const Context = @import("zag/context.zig").Context;
const execute = @import("zag/execute.zig");
const PC = execute.PC;
const SP = execute.SP;
const TFProcess = execute.TFProcess;
const tfAsProcess = execute.tfAsProcess;
const TFContext = execute.TFContext;
const tfAsContext = execute.tfAsContext;
const MethodSignature = execute.MethodSignature;
const compileMethod = execute.compileMethod;
var Mfibonacci = compileMethod(Sym.fibonacci,0,4,.SmallInteger,.{
    &e.pushContext,"^",
    ":Mfibonacci:1",
    &e.pushLocal,0,
    &e.pushLiteral,Object.from(2),
    &e.setupSend,Sym.@"<=",
    &e.dynamicDispatch,
    ":Mfibonacci:2",
    &e.BlockClosure.fullClosure,"1mref",
    &e.setupSend,Sym.@"ifTrue:",
    &e.dynamicDispatch,
    ":Mfibonacci:3",
    &e.drop,
    &e.pushLocal,0,
    &e.pushLiteral,Object.from(1),
    &e.setupSend,Sym.@"-",
    &e.dynamicDispatch,
    ":Mfibonacci:4",
    &e.setupSend,Sym.fibonacci,
    &e.dynamicDispatch,
    ":Mfibonacci:5",
    &e.pushLocal,0,
    &e.pushLiteral,Object.from(2),
    &e.setupSend,Sym.@"-",
    &e.dynamicDispatch,
    ":Mfibonacci:6",
    &e.setupSend,Sym.fibonacci,
    &e.dynamicDispatch,
    ":Mfibonacci:7",
    &e.setupSend,Sym.@"+",
    &e.dynamicDispatch,
    &e.returnTop,
});
var Mfibonacci_1 = compileMethod(Sym.value,0,2,.BlockClosure,.{
    ":Mvalue:1",
    &e.pushLiteral,Object.from(1),
    &e.returnNonLocal,
});
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
const e = primitives.embedded;
const p = primitives.primitives;
fn initSmalltalk() void {
    primitives.init();
    sym = Sym.init();
    Mfibonacci.setLiterals(&[_]Object{sym.fibonacci},&[_]Object{@bitCast(@intFromPtr(&Mfibonacci_1))});
    Mfibonacci_1.setLiterals(&[_]Object{},&[_]Object{});
}
pub fn main() !void {
    initSmalltalk();
}
