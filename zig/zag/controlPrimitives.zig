const std = @import("std");
const config = @import("config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const checkEqual = @import("utilities.zig").checkEqual;
const object = @import("zobject.zig");
const Object = object.Object;
const ClassIndex = object.ClassIndex;
const Nil = object.Nil;
const NotAnObject = object.NotAnObject;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const indexSymbol0 = object.Object.indexSymbol0;
const indexSymbol1 = object.Object.indexSymbol1;
const symbol = @import("symbol.zig");
const Sym = symbol.symbols;
const Process = @import("process.zig");
const Context = @import("context.zig");
const execute = @import("execute.zig");
const ThreadedFn = execute.ThreadedFn;
const PC = execute.PC;
const SP = execute.SP;
const Extra = execute.Extra;
const compileMethod = execute.compileMethod;
const Execution = execute.Execution;
pub fn branch(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    const target = pc.targetPC();
    if (process.needsCheck()) return @call(tailCall, Process.check, .{ target, sp, process, context, undefined });
    return @call(tailCall, target.prim(), .{ target.next(), sp, process.checkBump(), context, undefined });
}
fn call(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    context.setReturn(pc.next2());
    const method = pc.method();
    const newPc = PC.init(method.codePtr());
    if (process.needsCheck()) return @call(tailCall, Process.check, .{ newPc, sp, process, context, undefined });
    return @call(tailCall, method.executeFn, .{ newPc.next(), sp, process, context, Extra.from(method) });
}
pub fn classCase(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
    var newPc = pc;
    const top = sp.top;
    const match = @intFromEnum(top.get_class());
    const newSp = sp.drop();
    while (true) {
        var classes = pc.object().to(u64);
        newPc = newPc.next();
        for (0..4) |_| {
            const class: u14 = @truncate(classes);
            if (class == match) {
                newPc = newPc.targetPC();
                return @call(tailCall, newPc.prim(), .{ newPc.next(), newSp, process, context, extra });
            }
            if (class == 0)
                return @call(tailCall, newPc.prim(), .{ newPc.next(), newSp, process, context, extra });
            if (class == 0x3FFF)
                return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, extra });
            classes >>= 14;
            newPc = newPc.next();
        }
    }
}
fn classCase24(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
fn cullColon(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
fn drop(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    const newSp = sp.drop();
    if (process.needsCheck()) return @call(tailCall, Process.check, .{ pc, newSp, process, context, undefined });
    return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
}
fn dropNext(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    const newSp = sp.dropPut(sp.top);
    if (process.needsCheck()) return @call(tailCall, Process.check, .{ pc, newSp, process, context, undefined });
    return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
}
fn dup(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    const newSp = sp.dropPut(sp.top);
    if (process.needsCheck()) return @call(tailCall, Process.check, .{ pc, newSp, process, context, undefined });
    return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
}
fn label(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, signature });
}
fn makeImmediateClosure(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
fn over(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    const newSp = sp.push(sp.next);
    if (process.needsCheck()) return @call(tailCall, Process.check, .{ pc, newSp, process, context, undefined });
    return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
}
fn popAssociationValue(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
fn popIndirect(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
fn popIndirectLocal(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
fn popInstVar(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
pub fn popLocal(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
    context.setLocal(pc.uint(), sp.top);
    const newSp = sp.drop();
    return @call(tailCall, pc.prim2(), .{ pc.skip(2), newSp, process, context, extra });
}
fn popLocalData(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    const ref = pc.uint();
    const local = context.getLocal(ref & 0xff);
    trace("\npopLocalData: {} {}", .{ ref, sp.top });
    local.setField(ref >> 12, sp.top);
    return @call(tailCall, pc.prim2(), .{ pc.next2(), sp.drop(), process, context, undefined });
}
fn popLocalField(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    const ref = pc.uint();
    const local = context.getLocal(ref & 0xfff);
    trace("\npopLocalField: {} {}", .{ ref, sp.top });
    local.setField(ref >> 12, sp.top);
    return @call(tailCall, pc.prim2(), .{ pc.next2(), sp.drop(), process, context, undefined });
}
fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
fn primitiveError(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
fn primitiveModule(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
fn primitiveModuleError(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
fn pushAssociationValue(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
pub fn pushContext(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    const method = pc.method();
    const stackStructure = method.stackStructure.hash56();
    const locals: u8 = @truncate(stackStructure);
    const maxStackNeeded: u16 = @truncate(stackStructure >> 16);
    const selfOffset: u16 = @truncate(stackStructure >> 32);
    trace("\npushContext: locals={} maxStack={} selfOffset={} signature={}", .{ locals, maxStackNeeded, selfOffset, method.signature });
    const ctxt = context.push(sp, process, method, locals, maxStackNeeded, selfOffset);
    const newSp = ctxt.asNewSp();
    trace("\npushContext: {any} {} {} {} 0x{x} 0x{x}", .{ process.getStack(sp), locals, method.signature, selfOffset, @intFromPtr(ctxt), @intFromPtr(sp) });
    return @call(tailCall, pc.prim2(), .{ pc.next2(), newSp, process, ctxt, undefined });
}
fn pushIndirect(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
fn pushIndirectLocal(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
fn pushInstVar(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
pub fn pushLocal(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    const newSp = sp.push(context.getLocal(pc.uint()));
    trace("\npushLocal: {any} {any}", .{ context.stack(newSp, process), context.allLocals(process) });
    return @call(tailCall, pc.next().prim(), .{ pc.skip(2), newSp, process, context, undefined });
}
fn pushLocalData(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    const ref = pc.uint();
    const local = context.getLocal(ref & 0xfff);
    const newSp = sp.push(local.getField(ref >> 12));
    trace("\npushLocalData: {} {} {any} {any}", .{ ref, local, context.stack(newSp, process), context.allLocals(process) });
    return @call(tailCall, pc.prim2(), .{ pc.next2(), newSp, process, context, undefined });
}
fn pushLocalField(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    const ref = pc.uint();
    const local = context.getLocal(ref & 0xff);
    const newSp = sp.push(local.getField(ref >> 12));
    trace("\npushLocalField: {} {} {any} {any}", .{ ref, local, context.stack(newSp, process), context.allLocals(process) });
    return @call(tailCall, pc.prim2(), .{ pc.next2(), newSp, process, context, undefined });
}
fn pushThisContext(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    const newSp = sp.push(Object.from(context));
    return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
}
fn pushThisProcess(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
    _ = .{ pc, sp, process, context, extra, unreachable };
}
pub fn returnSelf(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
    _ = .{ pc, sp, process, context, extra, unreachable };
}
pub fn returnSelfNoContext(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
    _ = .{ pc, sp, process, context, extra, unreachable };
}
pub fn returnTop(_: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    trace("\nreturnTop: {} ", .{sp.top});
    trace("{any} ", .{context.stack(sp, process)});
    const top = sp.top;
    const result = context.pop(process);
    const newSp = result.sp;
    newSp.top = top;
    const callerContext = result.ctxt;
    trace("-> {x}", .{@intFromPtr(newSp)});
    trace("-> {any}", .{callerContext.stack(newSp, process)});
    return @call(tailCall, callerContext.getNPc(), .{ callerContext.getTPc(), newSp, process, @constCast(callerContext), undefined });
}
pub fn returnTopNoContext(_: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    _ = .{ sp, process, context, unreachable };
}
fn returnTopNonLocal(_: PC, _: SP, _: *Process, _: *Context, _: Extra) SP {
    unreachable;
}
fn returnTopNonLocalNoContext(_: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    trace("\nreturnNoContext: {any} N={} T={}", .{ context.stack(sp, process), context.getNPc(), context.getTPc() });
    return @call(tailCall, context.getNPc(), .{ context.getTPc(), sp, process, context, undefined });
}
pub fn send(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
fn storeLocal(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    context.setLocal(pc.uint(), sp.top);
    if (process.needsCheck()) return @call(tailCall, Process.check, .{ pc.next(), sp, process, context, undefined });
    return @call(tailCall, pc.prim2(), .{ pc.skip(2), sp, process, context, undefined });
}
fn swap(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    const saved = sp.top;
    sp.top = sp.next;
    sp.next = saved;
    return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined });
}
fn tailCallMethod(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
fn tailSend(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
pub fn tailSendNoContext(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
    _ = .{ pc, sp, process, context, extra, unreachable };
}
fn value(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
fn valueColon(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}

pub const references = convertFn(.{
    &branch,
    &call,
    &classCase,
    &classCase24,
    &cullColon,
    &drop,
    &dropNext,
    &dup,
    &label,
    &makeImmediateClosure,
    &over,
    &popAssociationValue,
    &popIndirect,
    &popIndirectLocal,
    &popInstVar,
    &popLocal,
    &popLocalData,
    &popLocalField,
    &primitive,
    &primitiveError,
    &primitiveModule,
    &primitiveModuleError,
    &pushAssociationValue,
    &pushContext,
    &pushIndirect,
    &pushIndirectLocal,
    &pushInstVar, // encode offset (if any) in the object in higher bits, and the index to self in the low bits
    &pushLiteral,
    &pushLocal,
    &pushLocalData,
    &pushLocalField,
    &pushStack, // encode offset (if any) in the object in higher bits, and index to self in the low bits
    &pushThisContext,
    &pushThisProcess,
    &returnSelf,
    &returnSelfNoContext,
    &returnTop,
    &returnTopNoContext,
    &returnTopNonLocal,
    &returnTopNonLocalNoContext,
    &send,
    &storeLocal,
    &swap,
    &tailCallMethod,
    &tailSend,
    &tailSendNoContext,
    &value,
    &valueColon,
});
fn convertFn(comptime source: anytype) [source.len]ThreadedFn {
    var result: [source.len]ThreadedFn = undefined;
    inline for (source, &result) |src, *dst|
        dst.* = .{ .f = src };
    return result;
}

// following not in the table
fn primitiveFailed(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
    _ = .{ pc, sp, process, context, signature, unreachable };
}
fn returnWithContext(_: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    trace("\nreturnWithContext: {any} -> ", .{context.stack(sp, process)});
    const result = context.pop(process);
    const newSp = result.sp;
    var callerContext = result.ctxt;
    const stack = callerContext.stack(newSp, process);
    if (stack.len < 20) {
        trace("{any}", .{stack});
    } else trace("{}", .{stack.len});
    trace("\nrWC: sp={*} newSp={*}\n", .{ sp, newSp });
    return @call(tailCall, callerContext.getNPc(), .{ callerContext.getTPc(), newSp, process, @constCast(callerContext), undefined });
}

test "definitions" {
    _ = references;
}

fn push42(_: PC, sp: SP, _: *Process, _: *Context, _: Extra) SP {
    const newSp = sp.push(Object.from(42));
    return newSp;
}
test "send with dispatch direct" {
    if (true) return error.XSkipZigTest;
    std.debug.print("Test: send with dispatch direct\n", .{});
    const expectEqual = std.testing.expectEqual;
    Process.resetForTest();
    const method = compileMethod(Sym.yourself, 0, 0, .none, .{
        &pushContext,       "^",
        &send,              Sym.value,
        &returnWithContext,
    });
    const methodV = compileMethod(Sym.value, 0, 0, .none, .{
        &push42,
        &returnTopNoContext,
        1,
    });
    execute.init();
    methodV.asCompiledMethodPtr().forDispatch(.UndefinedObject);
    var te = Execution.new();
    te.init();
    var objs = [_]Object{ Nil, True };
    method.asCompiledMethodPtr().forDispatch(.UndefinedObject);
    const result = te.run(objs[0..], &method);
    try expectEqual(result.len, 3);
    try expectEqual(result[0], Object.from(42));
}
test "simple return via Execution" {
    std.debug.print("Test: simple return via Execution\n", .{});
    const expectEqual = std.testing.expectEqual;
    Process.resetForTest();
    var method = compileMethod(Sym.yourself, 0, 0, .none, .{
        &pushLiteral,        42,
        &returnTopNoContext, 1,
    });
    var te = Execution.new();
    te.init(null);
    var objs = [_]Object{ Nil, True };
    const result = te.run(objs[0..], &method);
    try expectEqual(result.len, 3);
    try expectEqual(result[0], Object.from(42));
    try expectEqual(result[1], Nil);
    try expectEqual(result[2], True);
}
test "context return via Execution" {
    std.debug.print("Test: context return via Execution\n", .{});
    const expectEqual = std.testing.expectEqual;
    Process.resetForTest();
    var method = compileMethod(Sym.@"at:", 0, 0, .none, .{
        &pushContext,       "^",
        &pushLiteral,       42,
        &returnWithContext,
    });
    var te = Execution.new();
    te.init(null);
    var objs = [_]Object{ Nil, True };
    const result = te.run(objs[0..], &method);
    try expectEqual(result.len, 1);
    try expectEqual(result[0], True);
}
test "context returnTop via Execution" {
    std.debug.print("Test: context returnTop via Execution\n", .{});
    const expectEqual = std.testing.expectEqual;
    Process.resetForTest();
    var method = compileMethod(Sym.yourself, 3, 0, .none, .{
        &pushContext, "^",
        &pushLiteral, 42,
        &returnTop,
    });
    var te = Execution.new();
    te.init(null);
    var objs = [_]Object{ Nil, True };
    const result = te.run(objs[0..], &method);
    try expectEqual(result.len, 2);
    try expectEqual(result[0], Object.from(42));
}
// test "context returnTop twice via Execution" {
// std.debug.print("Test: context returnTop twice via Execution\n",.{});
//     const expectEqual = std.testing.expectEqual;
//     Process.resetForTest();
//     const empty = Object.empty;
//     var method1 = compileMethod(Sym.yourself, 3, 0, .{
//         &pushContext, "^",
//         &pushLiteral, 1,
//         &call,        "0Obj",
//         &returnTop,
//     });
//     var method2 = compileMethod(Sym.name, 3, 0, .{
//         &pushContext, "^",
//         &pushLiteral, 42,
//         &returnTop,
//     });
//     method1.setLiterals(empty, &[_]Object{Object.from(&method2)});
//     var te = Execution.new();
//     te.init();
//     var objs = [_]Object{ Nil, True };
//     const result = te.run(objs[0..], &method1);
//     try expectEqual(result.len, 2);
//     try expectEqual(result[0], Object.from(42));
// }
test "simple executable" {
    std.debug.print("Test: simple executable\n", .{});
    const expectEqual = std.testing.expectEqual;
    Process.resetForTest();
    var method = compileMethod(Sym.yourself, 1, 0, .none, .{
        &pushContext,             "^",
        ":label1",                &pushLiteral,
        comptime Object.from(42), &popLocal,
        0,                        &pushLocal,
        0,                        &pushLiteral,
        0,                        &pushLiteral,
        true,                     &classCase,
        ClassIndex.False,         "label3",
        &branch,                  "label2",
        ":label3",                &pushLocal,
        0,                        ":label4",
        &returnTop,               ":label2",
        &pushLiteral,             0,
        &branch,                  "label4",
    });
    var objs = [_]Object{Nil};
    var te = Execution.new();
    te.init(null);
    const result = te.run(objs[0..], &method);
    try expectEqual(result.len, 1);
    try expectEqual(result[0], Object.from(0));
}
pub const controlPrimitivesX = struct {
    const ContextPtr = CodeContextPtr;

    pub fn verifyMethod(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) SP {
        const method = pc.method();
        trace("\nverifyMethod: {*} {} {}", .{ method, signature, method.signature });
        if (!method.signature.equals(signature)) {
            trace(" failed match", .{});
            return @call(tailCall, pc.prim2(), .{ pc.next2(), sp, process, context, signature });
        }
        const newPc = PC.init(@ptrCast(&method.code));
        trace(" newPc={} {}", .{ newPc, newPc.prim() });
        return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, undefined });
    }
    // pub fn ifTrue(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    //     const process = tfAsProcess(_process);
    //     const context = tfAsContext(_context);
    //     trace("\nifTrue: {any}", .{context.stack(sp, process)});
    //     const v = sp.top;
    //     if (True.equals(v)) return @call(tailCall, branch, .{ pc, sp.drop(), process, context, undefined });
    //     if (False.equals(v)) return @call(tailCall, pc.prim2(), .{ pc.next2(), sp.drop(), process, context, undefined });
    //     @panic("non boolean");
    // }
    // pub fn ifFalse(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    //     const process = tfAsProcess(_process);
    //     const context = tfAsContext(_context);
    //     trace("\nifFalse: {any}", .{context.stack(sp, process)});
    //     const v = sp.top;
    //     if (False.equals(v)) return @call(tailCall, branch, .{ pc, sp.drop(), process, context, undefined });
    //     if (True.equals(v)) return @call(tailCall, pc.next().prim(), .{ pc.skip(2), sp.drop(), process, context, undefined });
    //     @panic("non boolean");
    // }
    // pub fn ifNil(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    //     const v = sp.top;
    //     if (Nil.equals(v)) return @call(tailCall, branch, .{ pc, sp.drop(), process, context, undefined });
    //     return @call(tailCall, pc.next().prim(), .{ pc.skip(2), sp.drop(), process, context, undefined });
    // }
    // pub fn ifNotNil(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
    //     const v = sp.top;
    //     if (Nil.equals(v)) return @call(tailCall, pc.next().prim(), .{ pc.skip(2), sp.drop(), process, context, undefined });
    //     return @call(tailCall, branch, .{ pc, sp.drop(), process, context, undefined });
    // }
    pub fn primFailure(_: PC, _: SP, _: *Process, _: *Context, _: Extra) SP {
        @panic("primFailure");
    }
    pub fn replaceLiteral(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
        sp.top = pc.object();
        trace("\nreplaceLiteral: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.prim2(), .{ pc.next2(), sp, process, context, undefined });
    }
    pub fn replaceLiteral0(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
        sp.top = Object.from(0);
        trace("\nreplaceLiteral0: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined });
    }
    pub fn replaceLiteral1(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
        sp.top = Object.from(1);
        trace("\nreplaceLiteral0: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined });
    }
    pub fn pushLiteral0(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
        const newSp = sp.push(Object.from(0));
        trace("\npushLiteral0: {any}", .{context.stack(newSp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteral1(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
        const newSp = sp.push(Object.from(1));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteral2(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
        const newSp = sp.push(Object.from(2));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteral_1(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
        const newSp = sp.push(Object.from(-1));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteralIndirect(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
        const newSp = sp.push(pc.literalIndirect());
        return @call(tailCall, pc.prim2(), .{ pc.next2(), newSp, process, context, undefined });
    }
    pub fn pushLiteralNil(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
        const newSp = sp.push(Nil);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteralTrue(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
        const newSp = sp.push(True);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteralFalse(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
        const newSp = sp.push(False);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn printStack(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
        trace("\nstack: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined });
    }
    pub fn returnNoContextSwitchToThreaded(_: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
        trace("\nreturnNoContext: {any} N={} T={}", .{ context.stack(sp, process), context.getNPc(), context.getTPc() });
        const tPc = context.getTPc();
        const nPc = tPc.prev().prim();
        return @call(tailCall, nPc, .{ tPc, sp, process, context, undefined });
    }
    pub fn isCallerInThreadedMode(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) SP {
        trace("\nreturnNoContext: {any} N={} T={}", .{ context.stack(sp, process), context.getNPc(), context.getTPc() });
        const tPc = context.getTPc();
        const nPc = tPc.prev().prim();
        const newSp = sp.push(if (nPc == context.getNPc()) True else False);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
};
