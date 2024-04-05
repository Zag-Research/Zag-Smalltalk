const std = @import("std");
const config = @import("../config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const Context = @import("../context.zig").Context;
const execute = @import("../execute.zig");
const SendCache = execute.SendCache;
const ContextPtr = execute.CodeContextPtr;
const Code = execute.Code;
const PC = execute.PC;
const SP = execute.SP;
const compileMethod = execute.compileMethod;
const CompiledMethod = execute.CompiledMethod;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const Process = @import("../process.zig").Process;
const object = @import("../zobject.zig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const Sym = @import("../symbol.zig").symbols;
const heap = @import("../heap.zig");
const MinSmallInteger: i64 = object.MinSmallInteger;
const MaxSmallInteger: i64 = object.MaxSmallInteger;

pub fn init() void {}

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
        if (val.isInt() and val.u() <= Object.from(0x3fff_ffff_ffff).u() and val.u() >= Object.from(-0x4000_0000_0000).u()) {
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
    pub inline fn generalClosure(oldSp: SP, process: *Process, value: Object) SP {
        const sp = process.allocStack(oldSp, 4) catch unreachable; // can't fail because preallocated
        sp.top = Object.from(sp.unreserve(3));
        sp.top.tag = .heapClosure;
        sp.next = value;
        sp.third = Object.from(&valueClosureMethod);
        sp.fourth = heap.HeapObject.simpleStackObject(object.ClassIndex.BlockClosure, 2, Sym.value.hash24()).o();
        return sp;
    }
    var valueClosureMethod = CompiledMethod.init2(Sym.value, pushValue, e.returnNoContext);
    pub inline fn fullClosure(oldSp: SP, process: *Process, block: CompiledMethodPtr, context: ContextPtr) SP {
        const flags = block.stackStructure.h0 >> 8;
        const fields = flags & 63;
        const sp = process.allocStack(oldSp, fields + 2 - (flags >> 7));
        sp.top = Object.from(&sp[fields + 1]);
        sp.top.tag = .heapClosure;
        sp[fields] = Object.from(block);
        var f = fields;
        if (flags & 64 != 0) {
            f = f - 1;
            sp[f] = Object.from(context);
        }
        if (flags & 128 != 0) {
            f = f - 1;
            sp[f] = oldSp.top;
        }
        for (sp[1..f]) |*op|
            op.* = Nil;
        sp[fields + 1] = heap.HeapObject.simpleStackObject(object.BlockClosure_C, fields, block.selector.hash24()).o();
        return sp;
    }
    pub inline fn closureData(oldSp: SP, process: *Process, fields: usize) SP {
        const sp = process.allocStack(oldSp, fields + 3);
        const ptr = Object.from(&sp[fields + 1]);
        sp.top = ptr;
        sp.next = ptr;
        for (sp[2 .. fields + 2]) |*op|
            op.* = Nil;
        sp[fields + 2] = heap.HeapObject.simpleStackObject(fields, object.ClosureData_C, 0).o();
        return sp;
    }
    fn pushValue(_: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        if (!Sym.value.selectorEquals(selector)) {
            const dPc = cache.current();
            return @call(tailCall, dPc.prim(), .{ dPc.next(), sp, process, context, selector, cache.next() });
        }
        const closure = sp.top.to(heap.HeapObjectPtr);
        sp.top = closure.prevPrev();
        @panic("unfinished");
    }
    fn nonLocalReturn(_: PC, sp: SP, process: *Process, targetContext: ContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
        const val = sp.top;
        const result = targetContext.pop(process);
        const newSp = result.sp;
        if (!val.equals(object.NotAnObject))
            newSp.top = val;
        const callerContext = result.ctxt;
        trace("-> {any}", .{callerContext.stack(newSp, process)});
        return @call(tailCall, callerContext.getNPc(), .{ callerContext.getTPc(), newSp, process, @constCast(callerContext), undefined, undefined });
    }
};
pub const embedded = struct {
    const fallback = execute.fallback;
    const literalNonLocalReturn = enum(u3) { self = 0, true_, false_, nil, minusOne, zero, one, two };
    const nonLocalValues = [_]Object{ object.NotAnObject, True, False, Nil, Object.from(-1), Object.from(0), Object.from(1), Object.from(2) };
    pub fn value(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
        const val = sp.top;
        trace("\nvalue: {}", .{val});
        switch (val.immediate_class()) {
            // .heapThunk => sp.top.tag = .heap,
            // .nonLocalThunk => {
            //     const targetContext = @as(ContextPtr, @ptrFromInt(val.rawWordAddress()));
            //     const index = val.u() & 7;
            //     sp.top = nonLocalValues[index];
            //     trace(" {*} {}", .{ targetContext, index });
            //     return @call(tailCall, inlines.nonLocalReturn, .{ pc, sp, process, targetContext, undefined, undefined });
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
            //         return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, Sym.value, cache });
            //     }
            //     if (!Sym.value.selectorEquals(method.selector)) @panic("wrong selector");
            //     sp.top = closure.prevPrev();
            // },
            else => {
                std.debug.print("\nvalue of 0x{x}", .{val});
                @panic("unknown block type");
            },
        }
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined, undefined });
    }
    pub fn @"value:"(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
        const val = sp.next;
        switch (val.tag) {
            .numericThunk, .immediateThunk, .heapThunk.nonLocalThunk => @panic("wrong number of parameters"),
            .heapClosure, .nonLocalClosure => {
                const closure = val.to(heap.HeapObjectPtr);
                const method = closure.prev().to(CompiledMethodPtr);
                if (!Sym.@"value:".selectorEquals(method.selector)) @panic("wrong selector"); //return @call(tailCall,e.dnu,.{pc,sp,process,context,selector});
                const newPc = method.codePtr();
                context.setReturn(pc);
                if (true) @panic("unfinished");
                return @call(tailCall, newPc[0].prim, .{ newPc + 1, sp, process, context, Sym.value });
            },
            else => @panic("not closure"),
        }
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, undefined, undefined });
    }
    pub fn immutableClosure(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
        const newSp = inlines.immutableClosure(sp, process);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined, undefined });
    }
    pub fn generalClosure(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
        var mutableContext = context;
        const newSp = inlines.generalClosure(sp.drop(), process, sp.top, &mutableContext);
        return @call(tailCall, pc.prim2(), .{ pc.next2(), newSp, process, mutableContext, undefined, undefined });
    }
    pub fn fullClosure(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
        var mutableContext = context;
        const block = pc.indirectLiteral();
        const newSp = inlines.fullClosure(sp, process, block, &mutableContext);
        return @call(tailCall, pc.prim2(), .{ pc.next2(), newSp, process, mutableContext, undefined, undefined });
    }
    pub fn closureData(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
        var mutableContext = context;
        const newSp = inlines.closureData(sp, process, pc[0].uint, &mutableContext);
        return @call(tailCall, pc.prim2(), .{ pc.next2(), newSp, process, mutableContext, undefined, undefined });
    }

    inline fn nonLocalBlock(sp: SP, tag: literalNonLocalReturn, context: ContextPtr) SP {
        // [^self] [^true] [^false] [^nil] [^-1] [^0] [^1] [^2]
        return sp.push(Object.tagged(.nonLocalThunk, @intFromEnum(tag), context.cleanAddress()));
    }
    pub fn pushNonlocalBlock_self(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
        return @call(tailCall, pc[0].prim, .{ pc + 1, nonLocalBlock(sp, .self, context), process, context, undefined, undefined });
    }
    pub fn pushNonlocalBlock_true(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
        return @call(tailCall, pc[0].prim, .{ pc + 1, nonLocalBlock(sp, .true_, context), process, context, undefined, undefined });
    }
    pub fn pushNonlocalBlock_false(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
        return @call(tailCall, pc[0].prim, .{ pc + 1, nonLocalBlock(sp, .false_, context), process, context, undefined, undefined });
    }
    pub fn pushNonlocalBlock_nil(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
        return @call(tailCall, pc[0].prim, .{ pc + 1, nonLocalBlock(sp, .nil, context), process, context, undefined, undefined });
    }
    pub fn pushNonlocalBlock_minusOne(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
        return @call(tailCall, pc[0].prim, .{ pc + 1, nonLocalBlock(sp, .minusOne, context), process, context, undefined, undefined });
    }
    pub fn pushNonlocalBlock_zero(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
        return @call(tailCall, pc[0].prim(), .{ pc + 1, nonLocalBlock(sp, .zero, context), process, context, undefined, undefined });
    }
    pub fn pushNonlocalBlock_one(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
        trace("\npushNonLocalBlock_one: {} {x} {x} {}", .{ sp.top, @intFromPtr(sp), @intFromPtr(nonLocalBlock(sp, .one, context)), nonLocalBlock(sp, .one, context).top });
        return @call(tailCall, pc.prim(), .{ pc.next(), nonLocalBlock(sp, .one, context), process, context, undefined, undefined });
    }
    pub fn pushNonlocalBlock_two(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
        return @call(tailCall, pc[0].prim(), .{ pc + 1, nonLocalBlock(sp, .two, context), process, context, undefined, undefined });
    }
};
// fn testImmutableClosure(process: *Process, value: Object) !object.Group {
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
// fn testNonlocalClosure(process: *Process, value: Object) !object.Group {
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
    pub fn p201(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, _: SendCache) callconv(stdCall) SP { // value
        if (!Sym.value.selectorEquals(selector)) return @call(tailCall, execute.dnu, .{ pc, sp, process, context, undefined, undefined });
        unreachable;
    }
    pub fn p202(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP { // value:
        _ = .{ pc, sp, process, context, selector, cache };
        unreachable;
    }
    pub fn p203(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP { // value:value:
        _ = .{ pc, sp, process, context, selector, cache };
        unreachable;
    }
    pub fn p204(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP { // value:value:value:
        _ = .{ pc, sp, process, context, selector, cache };
        unreachable;
    }
    pub fn p205(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP { // value:value:value:value:
        _ = .{ pc, sp, process, context, selector, cache };
        unreachable;
    }
};
const e = struct {
    usingnamespace @import("../execute.zig").controlPrimitives;
    usingnamespace embedded;
};
