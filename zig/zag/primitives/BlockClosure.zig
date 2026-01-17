const std = @import("std");
const zag = @import("../zag.zig");
const config = zag.config;
const objectEncoding = config.objectEncoding;
const tailCall = config.tailCall;
const trace = config.trace;
const execute = zag.execute;
const Code = execute.Code;
const PC = execute.PC;
const CompiledMethod = execute.CompiledMethod;
const Result = execute.Result;
const Execution = execute.Execution;
const failed = Execution.failed;
const Process = zag.Process;
const SP = Process.SP;
const Context = zag.Context;
const Extra = Context.Extra;
const object = zag.object;
const Object = object.Object;
const ClassIndex = object.ClassIndex;
const Compact = ClassIndex.Compact;
const True = object.True;
const False = object.False;
const Nil = object.Nil;
const PackedObject = object.PackedObject;
const object14 = object.PackedObject.object14;
const Sym = zag.symbol.symbols;
const heap = zag.heap;
const tf = zag.threadedFn.Enum;
const stringOf = zag.heap.CompileTimeString;
const HeapHeader = zag.heap.HeapHeader;
const HeapObject = zag.heap.HeapObject;
const Signature = zag.execute.Signature;
const expectEqual = std.testing.expectEqual;

pub fn moduleInit() void {}
pub const moduleName = "BlockClosure";
const zModuleName = stringOf(moduleName).init().obj();
pub const ThunkReturnObject = struct {
    pub fn primitive(_: PC, sp: SP, process: *Process, _: *Context, _: Extra) Result {
        if (true) @panic("unreachable");
        const val = sp.top;
        const result = Object.from(@as(i50, val.extraI()), null);
        const targetContext = val.highPointer(*Context).?;
        const newSp, const callerContext = targetContext.popTargetContext(process, result);
        return @call(tailCall, process.check(callerContext.getNPc()), .{ callerContext.getTPc(), newSp, process, callerContext, Extra.fromContextData(callerContext.contextData) });
    }
    const name = stringOf("ThunkReturnObject").init().obj();
    test "ThunkReturnObject" {
        if (true) return error.SkipZigTest;
        var exe = zag.execute.Execution.initTest("ThunkReturnObject", .{});
        try exe.resolve(&[_]Object{ name.asObject(), zModuleName.asObject(), unreachable });
    }
};

pub const threadedFns = struct {
    /// codeword: asThunk
    /// converts the value on top of the stack to a thunk that will evaluate to that value
    /// most values will become ThunkHeap, ThunkImmediate, or ThunkFloat
    /// however, values that don't fit will allocate a 1 element array on the heap and return a ThunkInstance that references it
    pub const asThunk = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            const result = blk: {
                if (switch (objectEncoding) {
                    .zag => encodeZag(sp.top),
                    .nan => encodeNan(sp.top),
                    else => unreachable,
                }) |encoded| {
                    break :blk encoded;
                } else {
                    const ar = process.allocArray(&[_]Object{sp.top}, sp, context);
                    break :blk Object.makeThunk(.ThunkInstance, ar, 1);
                }
            };
            sp.top = result;
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp, process, context, extra });
        }
        inline fn encodeZag(obj: Object) ?Object {
            switch (obj.tag) {
                .heap => return Object.makeThunk(.ThunkHeap, obj.to(*HeapObject), 0),
                .immediates => {
                    const original: i64 = @bitCast(obj);
                    const signExtended: i56 = @truncate(original << 8 >> 8);
                    if (signExtended == original)
                        return Object.makeThunkNoArg(.ThunkImmediate, @bitCast(signExtended));
                },
                else => { // float
                    const raw: u64 = @bitCast(obj);
                    trace(
                        \\float: raw = 0x{x}
                        \\   encoded = 0x{x}
                        \\   thunk   = 0x{x}
                        \\
                    , .{
                        raw,
                        ((raw & 0xffff_ffff_ffff_f000) >> 8) | (raw & 0xf),
                        Object.makeThunkNoArg(.ThunkFloat, @truncate(((raw & 0xffff_ffff_ffff_f000) >> 8) | (raw & 0xf))).testU(),
                    });
                    if (raw & 0xff0 == 0)
                        return Object.makeThunkNoArg(.ThunkFloat, @truncate(((raw & 0xffff_ffff_ffff_f000) >> 8) | (raw & 0xf)));
                },
            }
            return null;
        }
        inline fn encodeNan(obj: Object) ?Object {
            _ = obj;
            return failed;
            // switch (obj.tag) {
            //     .heap => _ = unreachable,
            //     .immediates => unreachable,
            //     else => unreachable,
            // }
            // return null;
        }
        test "asThunk int" {
            try config.skipNotZag();
            var exe = Execution.initTest("asThunk int", .{tf.asThunk});
            try exe.runTestWithValidator(
                @ptrCast(&validateInt),
                &[_]Object{exe.object(2)},
                Object.empty,
            );
        }
        fn validateInt(exe: anytype, _: []const Object) !void {
            switch (objectEncoding) {
                .zag => try std.testing.expectEqualSlices(Object, &[_]Object{Object.makeImmediate(.ThunkImmediate, @truncate(exe.object(2).testU()))}, exe.stack()),
                else => return error.TestAborted,
            }
        }

        test "asThunk ptr" {
            try config.skipNotZag();
            const obj = Object.fromAddress(&ThunkReturnObject.primitive);
            var exe = Execution.initTest("asThunk ptr", .{tf.asThunk});
            try exe.runTestWithValidator(
                @ptrCast(&validatePtr),
                &[_]Object{obj},
                &[_]Object{obj},
            );
        }
        fn validatePtr(exe: anytype, expected: []const Object) !void {
            const obj = expected[0];
            switch (objectEncoding) {
                .zag => try std.testing.expectEqualSlices(Object, &[_]Object{Object.makeImmediate(.ThunkHeap, @truncate(obj.testU() << 8))}, exe.stack()),
                else => return error.TestAborted,
            }
        }

        test "asThunk True" {
            try config.skipNotZag();
            var exe = Execution.initTest("asThunk True", .{tf.asThunk});
            try exe.runTestWithValidator(
                @ptrCast(&validateTrue),
                &[_]Object{True()},
                Object.empty,
            );
        }
        fn validateTrue(exe: anytype, _: []const Object) !void {
            switch (objectEncoding) {
                .zag => try std.testing.expectEqualSlices(Object, &[_]Object{Object.makeImmediate(.ThunkImmediate, @truncate(True().testU()))}, exe.stack()),
                else => return error.TestAborted,
            }
        }

        test "asThunk float" {
            try config.skipNotZag();
            var exe = Execution.initTest("asThunk float", .{tf.asThunk});
            try exe.runTestWithValidator(
                @ptrCast(&validateFloat),
                &[_]Object{exe.object(-32767.75)},
                Object.empty,
            );
        }
        fn validateFloat(exe: anytype, _: []const Object) !void {
            switch (objectEncoding) {
                .zag => try std.testing.expectEqualSlices(Object, &[_]Object{@bitCast(@as(u64, 0x0dffff0000000e69))}, exe.stack()),
                else => return error.TestAborted,
            }
        }

        test "asThunk doesn't fit" {
            try config.skipNotZag();
            var exe = Execution.initTest("asThunk doesn't fit", .{tf.asThunk});
            const obj = exe.object(1.0 / 5.0);
            try exe.runTestWithValidator(
                @ptrCast(&validateNone),
                &[_]Object{obj},
                &[_]Object{obj},
            );
        }
        fn validateNone(exe: anytype, expected: []const Object) !void {
            const obj = expected[0];
            const result = exe.stack()[0];
            const exeheap = exe.getHeap();
            switch (objectEncoding) {
                .zag => {
                    try expectEqual(.ThunkInstance, result.which_class());
                    try expectEqual(2, exeheap.len);
                    try expectEqual(obj, exeheap[1].asObjectValue());
                },
                else => return error.TestAborted,
            }
        }
    };
    pub const pushClosure = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            const structure: PackedObject = pc.packedObject();
            const stackedFields = structure.f1;
            const stackOffset = structure.f2;
            const stackReserve = structure.f3;
            _ = stackReserve;
            const includeContext = structure.f4;
            const size = stackedFields + 1 + includeContext;
            var tempBuffer: [10]Object = undefined;
            const temp = tempBuffer[0..stackedFields]; // ToDo: verify that fits
            for (temp, sp.slice(stackedFields)) |*t, s|
                t.* = s;
            const newSp, const newContext, const newExtra =
                if (sp.reserve(3 + includeContext)) |anSp| blk: {
                    break :blk .{ anSp, context, extra };
                } else sp.spillStackAndReserve(3 + includeContext, context, extra);
            const copySize = stackOffset - stackedFields;
            for (newSp.unreserve(1).slice(copySize), sp.unreserve(stackedFields).slice(copySize)) |*d, s|
                d.* = s;
            const closure = newSp.unreserve(copySize + 1).slice(size + 1);
            for (closure[2 + includeContext ..], temp) |*d, s|
                d.* = s;
            closure[0] = (HeapHeader.calc(.BlockClosure, @truncate(size), @truncate(@intFromPtr(sp)), .onStack, null, Object, false) catch unreachable).o();
            closure[1] = pc.next().object();
            if (includeContext != 0) closure[2] = Object.fromAddress(context);
            newSp.top = Object.fromAddress(closure.ptr);
            return @call(tailCall, process.check(pc.skip(2).prim()), .{ pc.skip(2).next(), newSp, process, newContext, newExtra });
        }
        test "pushClosure" {
            if (true) return error.NotImplemented;
            const exe1 = Execution.init(.{});
            const testMethod = exe1.method;
            var exe = Execution.initTest("pushClosure", .{
                tf.pushLiteral,
                Object.tests[0],
                tf.pushLiteral,
                "1True",
                tf.pushLiteral,
                "2Nil",
                tf.pushLiteral,
                1,
                tf.pushClosure,
                comptime object14(.{ 3, 4, 0 }),
                "0block",
            });
            try exe.resolve(&[_]Object{ exe.object(&testMethod), True(), Nil() });
            exe.execute(&[_]Object{
                exe.object(17),
            });
            const stack = exe.stack();
            try expectEqual(exe.object(&stack[2]), stack[0]);
            try expectEqual(exe.object(42), stack[1]);
            const header: HeapHeader = @bitCast(stack[2]);
            try expectEqual(.onStack, header.age);
            try expectEqual(4, header.length);
            try expectEqual(.BlockClosure, header.classIndex);
            try expectEqual(exe.object(&testMethod), stack[3]);
            try expectEqual(exe.object(1), stack[4]);
            try expectEqual(Nil(), stack[5]);
            try expectEqual(True(), stack[6]);
            try expectEqual(exe.object(17), stack[7]);
        }
    };
    pub const createClosure = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            // at several places where the context could move we re-call this function rather than handling all the cases inline
            // this has a bit of cost, but it's worth it for the simplicity and is very rare
            if (extra.installContextIfNone(sp, process, context)) |new| {
                return @call(tailCall, threadedFn, .{ pc, new.sp, process, new.context, new.extra });
            }
            const signature = pc.signature();
            if (Object.immediateClosure(signature, sp, context)) |closure| {
                if (sp.push(closure)) |newSp| {
                    return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
                } else {
                    const newSp, const newContext, const newExtra = sp.spillStack(context, extra);
                    return @call(tailCall, threadedFn, .{ pc, newSp, process, newContext, newExtra });
                }
            }
            if (context.initStackClosure(sp, 2, signature.getClass())) |closureAddress| {
                if (sp.push(initMemoryClosure(closureAddress, signature, sp, context))) |newSp| {
                    return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, context, extra });
                }
            }
            if (context.initHeapClosure(sp, 2, signature.getClass())) |closureAddress| {
                if (sp.push(initMemoryClosure(closureAddress, signature, sp, context))) |newSp| {
                    return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, context, extra });
                }
            }
            const newSp, const newContext, const newExtra = sp.spillStack(context, extra);
            return @call(tailCall, threadedFn, .{ pc, newSp, process, newContext, newExtra });
        }
    };
    fn initMemoryClosure(closureAddress: *HeapObject, signature: Signature, sp: SP, context: *Context) Object {
        switch (signature.getClass()) {
            .ThunkReturnLocal, .ThunkReturnInstance, .ThunkLocal, .BlockAssignLocal, .ThunkInstance, .BlockAssignInstance, .ThunkReturnCharacter, .ThunkReturnFloat => {
                closureAddress.atSet(0, Object.fromAddress(context));
                closureAddress.atSet(1, Object.from(signature.primitive(), sp, context));
            },
            .ThunkReturnObject => {
                closureAddress.atSet(0, Object.fromAddress(context));
                closureAddress.atSet(1, Object.from(@as(i8, @bitCast(signature.primitive())), sp, context));
            },
            .ThunkReturnImmediate => {
                closureAddress.atSet(0, Object.fromAddress(context));
                closureAddress.atSet(1, extraToObject(signature.primitive()));
            },
            else => @panic("incomplete"),
        }
        return Object.fromAddress(closureAddress);
    }
    inline fn extraToObject(extra: u8) Object {
        return switch (@as(Compact,@enumFromInt(extra >> 3))) {
            .True => True(),
            .False => False(),
            .none => Nil(),
            else => unreachable,
        };
    }
    pub const value = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            sp.traceStack("value", context, extra);
            const val = sp.top;
            const class = val.which_class();
            if (nonLocalReturning(val, class, sp, context)) |result| {
                const newSp, const newContext = val.highPointer(*Context).?.pop(sp);
                const newExtra = Extra.fromContextData(newContext.contextDataPtr(newSp));
                newSp.top = result;
                newSp.traceStack("returnSelf after", newContext, newExtra);
                return @call(tailCall, process.check(newContext.npc), .{ newContext.tpc, newSp, process, newContext, newExtra });
            }
            _ = .{ pc, @panic("Unimplemented class")};
        }
        inline fn nonLocalReturning(val: Object, class: ClassIndex, sp: SP, context: *Context) ?Object {
            switch (class) {
            .ThunkReturnObject => {
                return Object.from(val.extraI(), sp, context);
            },
            // .ThunkReturnImmediate => {
            //     result = @bitCast(val.rawU() << 48 >> 56);
            //     continue :sw .reserved;
            // },
            // .ThunkReturnCharacter => {
            //     result = Object.makeImmediate(.Character, val.rawU() << 48 >> 56);
            //     continue :sw .reserved;
            // },
            // .ThunkReturnFloat => {
            //     const sign_exponent = (val.rawU() & 0xc000) << 48;
            //     const exponent_mantissa = @as(u64, @bitCast(@as(i64, @bitCast((val.rawU() & 0x3f00) << 50)) >> 6)) >> 2;
            //     result = Object.from(@as(f64, @bitCast(sign_exponent | exponent_mantissa)));
            //     continue :sw .reserved;
            // },
            // .ThunkReturnLocal,
            // .ThunkReturnInstance,
            // => { // this is the common part for ThunkReturns
            //     const targetContext: Context = @ptrFromInt(val >> 16);
            //     switch (val.class) {
            //         .ThunkReturnLocal => {
            //             result = targetContext.getLocal((val.rawU() >> 8) and 0xFF);
            //         },
            //         .ThunkReturnInstance => {
            //             result = targetContext.getSelfInstVar((val.rawU() >> 8) and 0xFF);
            //         },
            //         .ThunkReturnHeap => {
            //             result = @bitCast(val.rawI() >> 16);
            //         },
            //         .ThunkLocal, .ThunkInstance, .ThunkFloat, .none => { // this is the common part for other immediate BlockClosures
            //             sp.top = result;
            //             return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp, process, context, extra });
            //         },
            //         .BlockAssignLocal, .BlockAssignInstance => {
            //             @panic("unreachable");
            //         },
                else => {
                    return null;
                },
            }
        }
    };
    pub const cullColon = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            _ = .{ pc, sp, process, context, extra, unreachable };
        }
    };
    pub const valueColon = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
            const val = sp.next;
            if (false and val.isImmediate()) {
                switch (val.class) {
                    .BlockAssignLocal, .BlockAssignInstance => {
                        const closure = val.to(*HeapObject);
                        const method = closure.prev().to(*CompiledMethod);
                        //                if (!Sym.@"value:".selectorEquals(method.selector)) @panic("wrong selector"); //return @call(tailCall,eprocess.check(.dnu),.{pc,sp,process,context,selector});
                        const newPc = method.codePtr();
                        context.setReturn(pc);
                        if (true) @panic("unfinished");
                        return @call(tailCall, process.check(newPc[0].prim), .{ newPc + 1, sp, process, context, Sym.value });
                    },
                    .ThunkReturnObject, .ThunkReturnImmediate, .ThunkReturnCharacter, .ThunkReturnFloat, .ThunkReturnLocal, .ThunkReturnInstance, .ThunkImmediate, .ThunkHeap, .ThunkLocal, .ThunkInstance, .ThunkFloat => @panic("wrong # arguments"),
                    else => {},
                }
            }
            @panic("not implemented");
        }
    };
};
pub const ThunkImmediate = struct {
    pub fn primitive(_: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
        const result = sp.top.extraValue();
        const newSp, const callerContext = context.pop(sp);
        newSp.top = result;
        return @call(tailCall, process.check(callerContext.getNPc()), .{ callerContext.getTPc(), newSp, process, callerContext, undefined });
    }
    const name = stringOf("ThunkImmediate").init().obj();
    test "ThunkImmediate" {
        if (true) return error.SkipZigTest;
        var exe = zag.execute.Execution.initTest("ThunkImmediate", .{});
        try exe.resolve(&[_]Object{ name.asObject(), zModuleName.asObject(), unreachable });
    }
};
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
        } else if (val.ifHeapObject()) |_| {
            sp.top.tag = .heapThunk;
        } else {
            newSp = generalClosure(sp.drop(), process, val);
        }
        return newSp;
    }
    pub inline fn generalClosure(oldSp: SP, process: *Process, val: Object) SP {
        // const sp = process.allocStack(oldSp, .BlockClosure, 1, null, Object) catch @panic("unreachable"); // can't fail because preallocated
        // sp.third = val;
        // return sp;
        _ = .{ oldSp, process, val, unreachable };
    }
    var valueClosureMethod = CompiledMethod.init2(Sym.value, pushValue, tf.returnNoContext);
    pub inline fn fullClosure(oldSp: SP, process: *Process, block: *CompiledMethod, context: *Context, extra: Extra) Result {
        // const flags = block.stackStructure.locals; // TODO: wrong
        // const fields = flags & 63;
        // const sp = process.allocStackSpace(oldSp, fields + 2 - (flags >> 7)) catch @panic("no stack");
        // sp.top = sp.at(fields + 1);
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
        _ = .{ oldSp, process, block, context, extra, @panic("fullClosure") };
    }
    fn pushValue(_: PC, sp: SP, _: *Process, _: *Context, _: Object) SP {
        const closure = sp.top.to(*HeapObject);
        sp.top = closure.prevPrev();
        @panic("unfinished");
    }
    fn nonLocalReturn(_: PC, sp: SP, process: *Process, targetContext: *Context, _: Object) SP {
        const val = sp.top;
        const result = targetContext.pop(process);
        const newSp = result.sp;
        if (!val.equals(unreachable))
            newSp.top = val;
        const callerContext = result.ctxt;
        return @call(tailCall, process.check(callerContext.getNPc()), .{ callerContext.getTPc(), newSp, process, @constCast(callerContext), undefined, undefined });
    }
};
const fallback = execute.fallback;
pub fn immutableClosure(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
    const newSp = inlines.immutableClosure(sp, process);
    return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, context, extra });
}
pub fn generalClosureX(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
    const newSp = inlines.generalClosure(sp.drop(), process, sp.top);
    return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
}
// pub fn fullClosure(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
//     const block = pc.object();
//     const newSp = inlines.fullClosure(sp, process, @ptrFromInt(block.nativeU_noCheck()), context, extra);
//     return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, undefined });
// }
// pub fn closureData(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
//     const newSp = process.allocStack(sp, .BlockClosure, @truncate(pc.uint() + 3), null, Object) catch @panic("closureData");
//     return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, undefined });
// }

// fn testImmutableClosure(process: *Process, value: Extra) !object.Group {
//     const ee = std.testing.expectEqual;
//     var context = Context.init();
//     const sp = process.endOfStack().push(value);
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
//     var process: Process align(Process.alignment) = undefined;
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
//     var process: Process align(Process.alignment) = undefined;
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
    pub fn p201(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // value
        _ = .{ pc, sp, process, context, extra, @panic("prim201") };
    }
    pub fn p202(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // value:
        _ = .{ pc, sp, process, context, extra, @panic("prim202") };
    }
    pub fn p203(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // value:value:
        _ = .{ pc, sp, process, context, extra, @panic("prim203") };
    }
    pub fn p204(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // value:value:value:
        _ = .{ pc, sp, process, context, extra, @panic("prim204") };
    }
    pub fn p205(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // value:value:value:value:
        _ = .{ pc, sp, process, context, extra, @panic("prim205") };
    }
};
