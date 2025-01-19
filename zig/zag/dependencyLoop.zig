const std = @import("std");

const execute = struct {
    const ThreadedFn = *const fn (
//        programCounter: PC, stackPointer: SP,
        process: *Process,
        context: *Context,
        signature: MethodSignature
    ) SP;
    const CompiledMethodPtr = *CompiledMethod;
    const CompiledMethod = struct {
//        executeFn: ThreadedFn,
//        jitted: ThreadedFn,
    };
    const PC = struct {
        code: *const Code,
    };
    const Code = union {
//        prim: ThreadedFn,
        int: i64,
    };
    const Stack = extern struct {
        top: u64,
    };
    const SP = *Stack;
    const MethodSignature = extern struct {
        int: u64,
    };
};
const Process = extern struct {
    int: u64,
    debugFn: ?ThreadedFn,
    sp: SP,
    const ThreadedFn = execute.ThreadedFn;
    const SP = execute.SP;
};
const Context = struct {
    method: CompiledMethodPtr,
    npc: ThreadedFn,
    prevCtxt: ?ContextPtr,
    const ThreadedFn = execute.ThreadedFn;
    const CompiledMethodPtr = execute.CompiledMethodPtr;
    const ContextPtr = *Context;
};

test "die" {
    _ = Process{
        .int=42,
        .debugFn=null,
        .sp=undefined,
    };
}
