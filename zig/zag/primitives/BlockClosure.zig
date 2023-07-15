const std = @import("std");
const Context = @import("../context.zig").Context;
const execute = @import("../execute.zig");
const trace = execute.trace;
const ContextPtr = execute.CodeContextPtr;
const Code = execute.Code;
const tailCall = execute.tailCall;
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
const sym = @import("../symbol.zig").symbols;
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
    pub fn immutableClosure(sp: [*]Object, process: *Process, contextMutable: *ContextPtr) [*]Object {
        const val = sp[0];
        var newSp = sp;
        if (val.isInt() and val.u() <= Object.from(0x3fff_ffff_ffff).u() and val.u() >= Object.from(-0x4000_0000_0000).u()) {
            sp[0] = Object.makeGroup(.numericThunk, @as(u47, @truncate(val.u())));
        } else if (val.isDouble() and (val.u() & 0x1ffff) == 0) {
            sp[0] = Object.makeGroup(.numericThunk, @as(u48, 1) << 47 | @as(u48, @truncate(val.u() >> 17)));
        } else if (val.isImmediate()) {
            sp[0].tag = .immediateThunk;
        } else if (val.isHeapObject()) {
            sp[0].tag = .heapThunk;
        } else {
            newSp = generalClosure(sp + 1, process, val, contextMutable);
        }
        return newSp;
    }
    pub inline fn generalClosure(oldSp: [*]Object, process: *Process, value: Object, contextMutable: *ContextPtr) [*]Object {
        const sp = process.allocStack(oldSp, 4, contextMutable);
        sp[0] = Object.from(&sp[3]);
        sp[0].tag = .heapClosure;
        sp[1] = value;
        sp[2] = Object.from(&valueClosureMethod);
        sp[3] = heap.HeapObject.simpleStackObject(object.ClassIndex.BlockClosure, 2, sym.value.hash24()).o();
        return sp;
    }
    var valueClosureMethod = CompiledMethod.init2(sym.value, pushValue, e.returnNoContext);
    pub inline fn fullClosure(oldSp: [*]Object, process: *Process, block: CompiledMethodPtr, contextMutable: *ContextPtr) [*]Object {
        const flags = block.stackStructure.h0 >> 8;
        const fields = flags & 63;
        const sp = process.allocStack(oldSp, fields + 2 - (flags >> 7), contextMutable);
        sp[0] = Object.from(&sp[fields + 1]);
        sp[0].tag = .heapClosure;
        sp[fields] = Object.from(block);
        var f = fields;
        if (flags & 64 != 0) {
            f = f - 1;
            sp[f] = Object.from(contextMutable.*);
        }
        if (flags & 128 != 0) {
            f = f - 1;
            sp[f] = oldSp[0];
        }
        for (sp[1..f]) |*op|
            op.* = Nil;
        sp[fields + 1] = heap.HeapObject.simpleStackObject(object.BlockClosure_C, fields, block.selector.hash24()).o();
        return sp;
    }
    pub inline fn closureData(oldSp: [*]Object, process: *Process, fields: usize, contextMutable: *ContextPtr) [*]Object {
        const sp = process.allocStack(oldSp, fields + 3, contextMutable);
        const ptr = Object.from(&sp[fields + 1]);
        sp[0] = ptr;
        sp[1] = ptr;
        for (sp[2 .. fields + 2]) |*op|
            op.* = Nil;
        sp[fields + 2] = heap.HeapObject.simpleStackObject(fields, object.ClosureData_C, 0).o();
        return sp;
    }
    fn pushValue(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        if (!sym.value.hashEquals(selector)) return @call(tailCall, e.dnu, .{ pc, sp, process, context, selector });
        const closure = sp[0].to(heap.HeapObjectPtr);
        sp[0] = closure.prevPrev();
        @panic("unfinished");
    }
    fn nonLocalReturn(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const val = sp[0];
        if (true) _ = .{ val, process, context, selector, @panic("nonLocalReturn") };
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector });
    }
};
pub const embedded = struct {
    const nonLocalValues = [_]Object{ object.NotAnObject, True, False, Nil, Object.from(-1), Object.from(0), Object.from(1), Object.from(2) };
    pub fn nonLocalReturn(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const self = context.getSelf();
        _ = .{ pc, sp, self, process, selector };
        //        return @call(tailCall,inlines.nonLocalReturn,.{pc,sp,process,targetContext,selector});
        @panic("nonLocalReturn");
    }
    pub fn value(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const val = sp[0];
        switch (val.tag) {
            .numericThunk => {
                if (((val.u() >> 47) & 1) == 0) {
                    sp[0] = Object.from(@as(i64, @bitCast(val.u() << 17)) >> 17);
                } else {
                    sp[0] = @as(Object, @bitCast(val.u() << 17));
                }
            },
            .immediateThunk => sp[0].tag = .immediates,
            .heapThunk => sp[0].tag = .heap,
            .nonLocalThunk => {
                const targetContext = @as(ContextPtr, @ptrFromInt(val.rawWordAddress()));
                sp[0] = nonLocalValues[val.u() & 7];
                return @call(tailCall, inlines.nonLocalReturn, .{ pc, sp, process, targetContext, selector });
            },
            .heapClosure => {
                const closure = val.to(heap.HeapObjectPtr);
                const method = closure.prev().to(CompiledMethodPtr);
                if (method != &inlines.valueClosureMethod) {
                    const newPc = method.codePtr();
                    context.setReturn(pc);
                    return @call(tailCall, newPc[0].prim, .{ newPc + 1, sp, process, context, sym.value });
                }
                if (!sym.value.hashEquals(method.selector)) @panic("wrong selector"); //return @call(tailCall,e.dnu,.{pc,sp,process,context,selector});
                sp[0] = closure.prevPrev();
            },
            else => @panic("not closure"),
        }
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector });
    }
    pub fn @"value:"(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const val = sp[1];
        switch (val.tag) {
            .numericThunk, .immediateThunk, .heapThunk.nonLocalThunk => @panic("wrong number of parameters"),
            .heapClosure => {
                const closure = val.to(heap.HeapObjectPtr);
                const method = closure.prev().to(CompiledMethodPtr);
                if (!sym.@"value:".hashEquals(method.selector)) @panic("wrong selector"); //return @call(tailCall,e.dnu,.{pc,sp,process,context,selector});
                const newPc = method.codePtr();
                context.setReturn(pc);
                if (true) @panic("unfinished");
                return @call(tailCall, newPc[0].prim, .{ newPc + 1, sp, process, context, sym.value });
            },
            else => @panic("not closure"),
        }
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector });
    }
    pub fn immutableClosure(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        var mutableContext = context;
        const newSp = inlines.immutableClosure(sp, process, &mutableContext);
        return @call(tailCall, pc[1].prim, .{ pc + 2, newSp, process, mutableContext, selector });
    }
    pub fn generalClosure(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        var mutableContext = context;
        const newSp = inlines.generalClosure(sp + 1, process, sp[0], &mutableContext);
        return @call(tailCall, pc[1].prim, .{ pc + 2, newSp, process, mutableContext, selector });
    }
    pub fn fullClosure(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        var mutableContext = context;
        const block = pc.indirectLiteral();
        const newSp = inlines.fullClosure(sp, process, block, &mutableContext);
        return @call(tailCall, pc[1].prim, .{ pc + 2, newSp, process, mutableContext, selector });
    }
    pub fn closureData(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        var mutableContext = context;
        const newSp = inlines.closureData(sp, process, pc[0].uint, &mutableContext);
        return @call(tailCall, pc[1].prim, .{ pc + 2, newSp, process, mutableContext, selector });
    }

    const literalNonLocalReturn = enum(u3) { self = 0, true_, false_, nil, minusOne, zero, one, two };
    inline fn nonLocalBlock(sp: [*]Object, tag: literalNonLocalReturn) [*]Object {
        // [^self] [^true] [^false] [^nil] [^-1] [^0] [^1] [^2]
        const newSp = sp + 1;
        if (true) unreachable;
        sp[0] = nonLocalValues[@intFromEnum(tag)];
        return newSp;
    }
    pub fn pushNonlocalBlock_self(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        return @call(tailCall, pc[1].prim, .{ pc + 2, nonLocalBlock(sp, .self), process, context, selector });
    }
    pub fn pushNonlocalBlock_true(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        return @call(tailCall, pc[1].prim, .{ pc + 2, nonLocalBlock(sp, .true_), process, context, selector });
    }
    pub fn pushNonlocalBlock_false(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        return @call(tailCall, pc[1].prim, .{ pc + 2, nonLocalBlock(sp, .false), process, context, selector });
    }
    pub fn pushNonlocalBlock_nil(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        return @call(tailCall, pc[1].prim, .{ pc + 2, nonLocalBlock(sp, .nil), process, context, selector });
    }
    pub fn pushNonlocalBlock_minusOne(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        return @call(tailCall, pc[1].prim, .{ pc + 2, nonLocalBlock(sp, .minusOne), process, context, selector });
    }
    pub fn pushNonlocalBlock_zero(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        return @call(tailCall, pc[1].prim, .{ pc + 2, nonLocalBlock(sp, .zero), process, context, selector });
    }
    pub fn pushNonlocalBlock_one(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        return @call(tailCall, pc[1].prim, .{ pc + 2, nonLocalBlock(sp, .one), process, context, selector });
    }
    pub fn pushNonlocalBlock_two(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        return @call(tailCall, pc[1].prim, .{ pc + 2, nonLocalBlock(sp, .two), process, context, selector });
    }
};
fn testImmutableClosure(process: *Process, value: Object) !object.Group {
    const ee = std.testing.expectEqual;
    var context = Context.init();
    const sp = process.endOfStack() - 1;
    sp[0] = value;
    const newSp = embedded.immutableClosure(Code.endThread, sp, process, &context, Nil);
    if (newSp != sp) {
        try ee(value.u(), newSp[1].u());
    }
    const tag = newSp[0].tag;
    const newerSp = embedded.value(Code.endThread, newSp, process, &context, Nil);
    try ee(value.u(), newerSp[0].u());
    return tag;
}
test "immutableClosures" {
    const ee = std.testing.expectEqual;
    var process = Process.new();
    process.init();
    try ee(try testImmutableClosure(&process, Object.from(1)), .numericThunk);
    try ee(try testImmutableClosure(&process, Object.from(-1)), .numericThunk);
    try ee(try testImmutableClosure(&process, Object.from(0x3fff_ffff_ffff)), .numericThunk);
    try ee(try testImmutableClosure(&process, Object.from(-0x4000_0000_0000)), .numericThunk);
    try ee(try testImmutableClosure(&process, Object.from(1000.75)), .numericThunk);
    try ee(try testImmutableClosure(&process, Object.from(-1000.75)), .numericThunk);
    try ee(try testImmutableClosure(&process, Nil), .immediateThunk);
    try ee(try testImmutableClosure(&process, Object.from(&process)), .heapThunk);
    try ee(try testImmutableClosure(&process, Object.from(0x4000_0000_0000)), .heapClosure);
    try ee(try testImmutableClosure(&process, Object.from(-0x4000_0000_0001)), .heapClosure);
    try ee(try testImmutableClosure(&process, Object.from(1000.3)), .heapClosure);
}
pub const primitives = struct {
    pub fn p201(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // value
        if (!sym.value.hashEquals(selector)) return @call(tailCall, execute.dnu, .{ pc, sp, process, context, selector });
        unreachable;
    }
    pub fn p202(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // value:
        _ = .{ pc, sp, process, context, selector };
        unreachable;
    }
    pub fn p203(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // value:value:
        _ = .{ pc, sp, process, context, selector };
        unreachable;
    }
    pub fn p204(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // value:value:value:
        _ = .{ pc, sp, process, context, selector };
        unreachable;
    }
    pub fn p205(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // value:value:value:value:
        _ = .{ pc, sp, process, context, selector };
        unreachable;
    }
};
const e = struct {
    usingnamespace @import("../execute.zig").controlPrimitives;
    usingnamespace embedded;
};
