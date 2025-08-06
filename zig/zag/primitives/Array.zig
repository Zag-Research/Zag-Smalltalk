const std = @import("std");
const zag = @import("../zag.zig");
const config = zag.config;
const tailCall = config.tailCall;
const trace = config.trace;
const execute = zag.execute;
const Code = execute.Code;
const PC = execute.PC;
const Context = zag.Context;
const Extra = Context.Extra;
const Result = execute.Result;
const compileMethod = execute.compileMethod;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const Process = zag.Process;
const SP = Process.SP;
const object = zag.object;
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const Sym = zag.symbol.symbols;
const heap = zag.heap;
const tf = zag.threadedFn.Enum;
const stringOf = zag.heap.CompileTimeString;
const HeapHeader = zag.heap.HeapHeader;
const expectEqual = std.testing.expectEqual;

pub fn moduleInit() void {}
pub const moduleName = "Array";
pub fn init() void {}

pub const threadedFns = struct {
    pub const array = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            const size = pc.object();
            // allocate array of that size and copy elements from the top of the stack on reverse
            _ = .{ size, sp, process, context, extra, unreachable };
        }
    };
};
pub const @"new:" = struct {
    pub const number = 71;
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // Array class>>#new:
        const size = sp.top;
        _ = size;
        return @call(tailCall, process.check(context.npc), .{ pc, sp, process, context, extra });
    }
};
