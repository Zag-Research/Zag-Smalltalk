const std = @import("std");
const zag = @import("../zag.zig");
const config = zag.config;
const tailCall = config.tailCall;
const trace = config.trace;
const execute = zag.execute;
const ContextPtr = execute.CodeContextPtr;
const Code = execute.Code;
const PC = execute.PC;
const SP = execute.SP;
const compileMethod = execute.compileMethod;
const CompiledMethod = execute.CompiledMethod;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const Extra = execute.Extra;
const Result = execute.Result;
const Process = zag.Process;
const Context = zag.Context;
const object = zag.object;
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const Sym = zag.symbol.symbols;
const heap = zag.heap;
const tf = zag.threadedFn.Enum;
const MinSmallInteger: i64 = object.MinSmallInteger;
const MaxSmallInteger: i64 = object.MaxSmallInteger;

pub fn init() void {}
pub const moduleName = "BlockClosure";

pub const inlines = struct {
    pub inline fn p201(self: Object, other: Object) !Object { // value
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub inline fn p202(self: Object, other: Object) !Object { // value:
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub inline fn p203(self: Object, other: Object) !Object { // value:value:
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub inline fn p204(self: Object, other: Object) !Object { // value:value:value:
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub inline fn p205(self: Object, other: Object) !Object { // value:value:value:value:
        _ = self;
        _ = other;
        return error.primitiveError;
    }
    pub fn immutableClosure(sp: SP, process: *Process) SP {
        const val = sp.top;
        var newSp = sp;
        if (true) @panic("immutableClosure");
        if (val.isInt() and val.rawU() <= Object.from(0x3fff_ffff_ffff).rawU() and val.rawU() >= Object.from(-0x4000_0000_0000).rawU()) {
            sp.top = Object.makeGroup(.numericThunk, @as(u47, @truncate(val.u())));
        } else if (val.isDouble() and (val.u() & 0x1ffff) == 0) {
            sp.top = Object.makeGroup(.numericThunk, @as(u48, 1) << 47 | @as(u48, @truncate(val.u() >> 17)));
        } else if (val.isImmediate()) {
            sp.top.tag = .immediateThunk;
        } else if (val.isHeapObject()) {
            sp.top.tag = .heapThunk;
        } else {
            newSp = generalClosure(sp.drop(), process, val);
        }
        return newSp;
    }
    pub inline fn generalClosure(oldSp: SP, process: *Process, val: Object) SP {
        const sp = process.allocStack(oldSp, .BlockClosure, 1, null, Object) catch unreachable; // can't fail because preallocated
        sp.third = val;
        return sp;
    }
    var valueClosureMethod = CompiledMethod.init2(Sym.value, pushValue, tf.returnNoContext);
    pub inline fn fullClosure(oldSp: SP, process: *Process, block: CompiledMethodPtr, _: ContextPtr, _: Extra) SP {
        const flags = block.stackStructure.f1 >> 8;
        const fields = flags & 63;
        const sp = process.allocStackSpace(oldSp, fields + 2 - (flags >> 7)) catch @panic("no stack");
        sp.top = sp.at(fields + 1);
        // sp.top.tag = .nonLocalThunk;
        // sp.atPut(fields, Object.from(block));
        // var f = fields;
        // if (flags & 64 != 0) {
        //     f = f - 1;
        //     sp.atPut(f, Object.from(context));
        // }
        // if (flags & 128 != 0) {
        //     f = f - 1;
        //     sp.atPut(f, oldSp.top);
        // }
        // for (sp[1..f]) |*op|
        //     op.* = Nil;
        // sp[fields + 1] = heap.HeapObject.simpleStackObject(object.BlockClosure_C, fields, block.selector.hash24()).o();
        // return sp;
        @panic("fullClosure");
    }
    fn pushValue(_: PC, sp: SP, _: *Process, _: *Context, _: Object) SP {
        const closure = sp.top.to(heap.HeapObjectPtr);
        sp.top = closure.prevPrev();
        @panic("unfinished");
    }
    fn nonLocalReturn(_: PC, sp: SP, process: *Process, targetContext: *Context, _: Object) SP {
        const val = sp.top;
        const result = targetContext.pop(process);
        const newSp = result.sp;
        if (!val.equals(object.NotAnObject))
            newSp.top = val;
        const callerContext = result.ctxt;
        trace("-> {any}", .{callerContext.stack(newSp, process)});
        return @call(tailCall, process.check(callerContext.getNPc()), .{ callerContext.getTPc(), newSp, process, @constCast(callerContext), undefined, undefined });
    }
};
const fallback = execute.fallback;
pub const value = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const val = sp.top;
        trace("\nvalue: {}", .{val});
        var result: Object = undefined;

        if (val.isImmediate()) {
            sw: switch (val.class) {
                .ThunkReturnLocal, .ThunkReturnInstance, .ThunkReturnSmallInteger, .ThunkReturnImmediate, .ThunkReturnCharacter, .ThunkReturnFloat, .reserved => { // this is the common part for ThunkReturns
                    const targetContext: Context = @ptrFromInt(val.rawU() >> 16);
                    const newSp, const callerContext = context.popTargetContext(targetContext, process);
                    newSp.top = result;
                    return @call(tailCall, process.check(callerContext.getNPc()), .{ callerContext.getTPc(), newSp, process, @constCast(callerContext), undefined });
                },
                .ThunkImmediate => {
                    result = @bitCast(val.rawI() >> 8);
                    continue :sw .none;
                },
                .ThunkHeap, .ThunkLocal, .ThunkInstance, .BlockAssignLocal, .BlockAssignInstance, .ThunkFloat, .none => { // this is the common part for other immediate BlockClosures
                    sp.top = result;
                    return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp, process, context, extra });
                },
                else => {},
            }
        }
        // .heapThunk => sp.top.tag = .heap,
        // .nonLocalThunk => {
        //     const targetContext = @as(ContextPtr, @ptrFromInt(val.rawWordAddress()));
        //     const index = val.u() & 7;
        //     sp.top = nonLocalValues[index];
        //     trace(" {*} {}", .{ targetContext, index });
        //     return @call(tailCall, process.check(inlines.nonLocalReturn), .{ pc, sp, process, targetContext, undefined, undefined });
        // },
        // .numericThunk => {
        //     if (((val.u() >> 47) & 1) == 0) {
        //         sp.top = Object.from(@as(i64, @bitCast(val.u() << 17)) >> 17);
        //     } else {
        //         sp.top = @as(Object, @bitCast(val.u() << 17));
        //     }
        // },
        // .immediateThunk => sp.top.tag = .immediates,
        // .heapClosure, .nonLocalClosure => {
        //     const closure = val.to(heap.HeapObjectPtr);
        //     const method = closure.prev().to(CompiledMethodPtr);
        //     if (method != &inlines.valueClosureMethod) {
        //         const newPc = PC.init(method.codePtr());
        //         context.setReturn(pc);
        //         return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), sp, process, context, Sym.value});
        //     }
        //     if (!Sym.value.selectorEquals(method.selector)) @panic("wrong selector");
        //     sp.top = closure.prevPrev();
        // },
        @panic("fallback");
    }
};
pub const @"value:" = struct {
    fn threadedFn(pc: PC, sp: SP, process: *Process, context: Context, _: Extra) Result {
        const val = sp.next;
        switch (val.tag) {
            .heap => {
                const closure = val.to(heap.HeapObjectPtr);
                const method = closure.prev().to(CompiledMethodPtr);
                //                if (!Sym.@"value:".selectorEquals(method.selector)) @panic("wrong selector"); //return @call(tailCall,eprocess.check(.dnu),.{pc,sp,process,context,selector});
                const newPc = method.codePtr();
                context.setReturn(pc);
                if (true) @panic("unfinished");
                return @call(tailCall, process.check(newPc[0].prim), .{ newPc + 1, sp, process, context, Sym.value });
            },
            else => @panic("not closure"),
        }
        return @call(tailCall, process.check(pc[0].prim), .{ pc + 1, sp, process, context, undefined, undefined });
    }
};
pub fn immutableClosure(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
    const newSp = inlines.immutableClosure(sp, process);
    return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, context, undefined });
}
pub fn generalClosureX(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
    const newSp = inlines.generalClosure(sp.drop(), process, sp.top);
    return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, undefined });
}
pub fn fullClosure(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
    const block = pc.object();
    const newSp = inlines.fullClosure(sp, process, @ptrFromInt(block.rawU()), context, extra);
    return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, undefined });
}
pub fn closureData(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
    const newSp = process.allocStack(sp, .BlockClosure, @truncate(pc.uint() + 3), null, Object) catch @panic("closureData");
    return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, undefined });
}

// fn testImmutableClosure(process: *Process, value: Extra) !object.Group {
//     const ee = std.testing.expectEqual;
//     var context = Context.init();
//     const sp = process.endOfStack().push(value);
//     var cache = execute.SendCacheStruct.init();
//     const newSp = embedded.immutableClosure(Code.endThread, sp, process, &context, Nil, cache.dontCache());
//     if (newSp != sp) {
//         try ee(value.u(), newSp.next.u());
//     }
//     const tag = newSp.top.tag;
//     const newerSp = embedded.value(Code.endThread, newSp, process, &context, Nil, cache.dontCache());
//     try ee(value.u(), newerSp.top.u());
//     return tag;
// }
// test "immutableClosures" {
//     const ee = std.testing.expectEqual;
//     var process = Process.new();
//     process.init();
//     try ee(try testImmutableClosure(&process, Object.from(1)), .numericThunk);
//     try ee(try testImmutableClosure(&process, Object.from(-1)), .numericThunk);
//     try ee(try testImmutableClosure(&process, Object.from(0x3fff_ffff_ffff)), .numericThunk);
//     try ee(try testImmutableClosure(&process, Object.from(-0x4000_0000_0000)), .numericThunk);
//     try ee(try testImmutableClosure(&process, Object.from(1000.75)), .numericThunk);
//     try ee(try testImmutableClosure(&process, Object.from(-1000.75)), .numericThunk);
//     try ee(try testImmutableClosure(&process, Nil), .immediateThunk);
//     try ee(try testImmutableClosure(&process, Object.from(&process)), .heapThunk);
//     try ee(try testImmutableClosure(&process, Object.from(0x4000_0000_0000)), .heapClosure);
//     try ee(try testImmutableClosure(&process, Object.from(-0x4000_0000_0001)), .heapClosure);
//     try ee(try testImmutableClosure(&process, Object.from(1000.3)), .heapClosure);
// }
// fn testNonlocalClosure(process: *Process, value: Extra) !object.Group {
//     const ee = std.testing.expectEqual;
//     var context = Context.init();
//     const sp = process.endOfStack().push(value);
//     var cache = execute.SendCacheStruct.init();
//     const newSp = embedded.immutableClosure(Code.endThread, sp, process, &context, Nil, cache.dontCache());
//     if (newSp != sp) {
//         try ee(value.u(), newSp.next.u());
//     }
//     const tag = newSp.top.tag;
//     const newerSp = embedded.value(Code.endThread, newSp, process, &context, Nil, cache.dontCache());
//     try ee(value.u(), newerSp.top.u());
//     return tag;
// }
// test "nonlocalClosures" {
//     const ee = std.testing.expectEqual;
//     var process = Process.new();
//     process.init();
//         // [^self] [^true] [^false] [^nil] [^-1] [^0] [^1] [^2]
//     try ee(try testNonlocalClosure(&process, True), .nonLocalThunk);
//     try ee(try testNonlocalClosure(&process, False), .nonLocalThunk);
//     try ee(try testNonlocalClosure(&process, Nil), .nonLocalThunk);
//     try ee(try testNonlocalClosure(&process, Object.from(0)), .nonLocalThunk);
//     try ee(try testNonlocalClosure(&process, Object.from(1)), .nonLocalThunk);
//     try ee(try testNonlocalClosure(&process, Object.from(2)), .nonLocalThunk);
//     try ee(try testNonlocalClosure(&process, Object.from(-1)), .nonLocalThunk);
//     try ee(try testNonlocalClosure(&process, object.SelfObject), .nonLocalThunk);
//     try ee(try testNonlocalClosure(&process, Object.from(0x3fff_ffff_ffff)), .nonLocalClosure);
//     try ee(try testNonlocalClosure(&process, Object.from(-0x4000_0000_0000)), .nonLocalClosure);
//     try ee(try testNonlocalClosure(&process, Object.from(1000.75)), .nonLocalClosure);
//     try ee(try testNonlocalClosure(&process, Object.from(-1000.75)), .nonLocalClosure);
//     try ee(try testNonlocalClosure(&process, Object.from(&process)), .nonLocalClosure);
//     try ee(try testNonlocalClosure(&process, Object.from(0x4000_0000_0000)), .nonLocalClosure);
//     try ee(try testNonlocalClosure(&process, Object.from(-0x4000_0000_0001)), .nonLocalClosure);
//     try ee(try testNonlocalClosure(&process, Object.from(1000.3)), .nonLocalClosure);
// }
pub const primitives = struct {
    pub fn p201(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) Result { // value
        _ = .{ pc, sp, process, context, signature, @panic("prim201") };
    }
    pub fn p202(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) Result { // value:
        _ = .{ pc, sp, process, context, signature, @panic("prim202") };
    }
    pub fn p203(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) Result { // value:value:
        _ = .{ pc, sp, process, context, signature, @panic("prim203") };
    }
    pub fn p204(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) Result { // value:value:value:
        _ = .{ pc, sp, process, context, signature, @panic("prim204") };
    }
    pub fn p205(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) Result { // value:value:value:value:
        _ = .{ pc, sp, process, context, signature, @panic("prim205") };
    }
};
