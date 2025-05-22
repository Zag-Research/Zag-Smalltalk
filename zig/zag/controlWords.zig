const std = @import("std");
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
const expectEqualSlices = std.testing.expectEqualSlices;
const zag = @import("zag.zig");
const config = zag.config;
const trace = config.trace;
const tailCall = config.tailCall;
const object = zag.object;
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const class = object.ClassIndex;
const PackedObject = object.PackedObject;
//const primitives = zag.primitives;
//const globalArena = zag.globalArena;
const symbol = zag.symbol;
const Sym = symbol.symbols;
const Process = zag.Process;
const Context = zag.Context;
const execute = zag.execute;
const ThreadedFn = execute.ThreadedFn;
const PC = execute.PC;
const SP = execute.SP;
const Extra = execute.Extra;
const Result = execute.Result;
const compileMethod = execute.compileMethod;
const compileObject = execute.compileObject;
const Execution = execute.Execution;
const heap = zag.heap;
const HeapHeader = heap.HeapHeader;
const object14 = object.PackedObject.object14;
const tf = zag.threadedFn.Enum;
const c = object.ClassIndex;
pub fn init() void {}
pub const module = struct {
    pub const moduleName = "zag";
    pub const unknown = struct {
        //pub const number = 60;
        pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            // if (inlines.@"basicAt:"(sp.next, sp.top)) |result| {
            //     const newSp = sp.dropPut(result);
            //     return @call(tailCall, process.check(context.npc.f), .{ context.tpc, newSp, process, context, undefined });
            // } else |_| {}
            // return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
            _ = .{ pc, sp, process, context, extra, unreachable };
        }
    };
};
pub const branch = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const target = pc.targetPC();
        return @call(tailCall, process.check(target.prim()), .{ target.next(), sp, process.checkBump(), context, extra });
    }
    test "branch" {
        var exe = Execution.initTest("branch", .{
            tf.branch,
            "label",
            tf.pushLocal,
            17,
            ":label",
        });
        assert(@alignOf(@TypeOf(exe)) > 50);
        try exe.execute(Object.empty);
        try exe.matchStack(Object.empty);
    }
};
pub const classCase = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        var newPc = pc;
        const top = sp.top;
        const match = @intFromEnum(top.get_class());
        const newSp = sp.drop();
        while (true) {
            var classes = pc.object().to(u64);
            newPc = newPc.next();
            for (0..4) |_| {
                const currentClass: u14 = @truncate(classes);
                if (currentClass == match) {
                    newPc = newPc.targetPC();
                    return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), newSp, process, context, extra });
                }
                if (currentClass == 0)
                    return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), newSp, process, context, extra });
                if (currentClass == 0x3FFF)
                    return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), sp, process, context, extra });
                classes >>= 14;
                newPc = newPc.next();
            }
        }
    }
    test "classCase match" {
        try Execution.runTest(
            "classCase match",
            .{
                tf.classCase,
                comptime object14(.{class.True}),
                "true",
                tf.pushLiteral,
                17,
                tf.branch,
                "end",
                ":true",
                tf.pushLiteral,
                42,
                ":end",
            },
            &[_]Object{True},
            &[_]Object{Object.from(42)},
        );
    }
    test "classCase no match" {
        try Execution.runTest(
            "classCase no match",
            .{
                tf.classCase,
                comptime object14(.{class.True}),
                "true",
                tf.pushLiteral,
                42,
                tf.branch,
                "end",
                ":true",
                tf.pushLiteral,
                17,
                ":end",
            },
            &[_]Object{False},
            &[_]Object{Object.from(42)},
        );
    }
    test "classCase no match - leave" {
        try Execution.runTest(
            "classCase no match - leave",
            .{
                tf.classCase,
                comptime object14(.{ class.True, 0x3fff }),
                "true",
                tf.branch,
                "end",
                ":true",
                tf.pushLiteral,
                17,
                ":end",
            },
            &[_]Object{False},
            &[_]Object{False},
        );
    }
};
pub const drop = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const newSp = sp.drop();
        return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, context, extra });
    }
    test "drop" {
        try Execution.runTest(
            "drop",
            .{tf.drop},
            &[_]Object{
                Object.from(17),
                Object.from(42),
            },
            &[_]Object{
                Object.from(42),
            },
        );
    }
};
pub const dropNext = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const newSp = sp.dropPut(sp.top);
        return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, context, extra });
    }
    test "dropNext" {
        try Execution.runTest(
            "dropNext",
            .{tf.dropNext},
            &[_]Object{
                Object.from(42),
                Object.from(17),
            },
            &[_]Object{
                Object.from(42),
            },
        );
    }
};
pub const dup = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const newSp = sp.push(sp.top);
        return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, context, extra });
    }
    test "dup" {
        try Execution.runTest(
            "dup",
            .{tf.dup},
            &[_]Object{
                Object.from(42),
                Object.from(17),
            },
            &[_]Object{
                Object.from(42),
                Object.from(42),
                Object.from(17),
            },
        );
    }
};
pub const over = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const newSp = sp.push(sp.next);
        return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, context, extra });
    }
    test "over" {
        try Execution.runTest(
            "over",
            .{tf.over},
            &[_]Object{
                Object.from(17),
                Object.from(42),
            },
            &[_]Object{
                Object.from(42),
                Object.from(17),
                Object.from(42),
            },
        );
    }
};
pub const popAssociationValue = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        pc.object().setField(2, sp.top);
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), sp.drop(), process, context, extra });
    }
    test "popAssociationValue" {
        var association = compileObject(.{
            ":def",
            c.Association,
            Nil,
            0,
        });
        try Execution.runTestWithObjects(
            "popAssociationValue",
            .{
                tf.popAssociationValue,
                "0object",
            },
            &.{
                association.asObject(),
            },
            &[_]Object{
                Object.from(42),
            },
            &[_]Object{},
        );
        try expectEqual(Object.from(42), association.objects[2]);
    }
};
pub const pushAssociationValue = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const value = pc.object().getField(2);
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), sp.push(value), process, context, extra });
    }
    test "pushAssociationValue" {
        var association = compileObject(.{
            ":def",
            c.Association,
            Nil,
            42,
        });
        try Execution.runTestWithObjects(
            "pushAssociationValue",
            .{
                tf.pushAssociationValue,
                "0object",
            },
            &.{
                association.asObject(),
            },
            &[_]Object{},
            &[_]Object{
                Object.from(42),
            },
        );
    }
};
pub const pushLiteral = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const newSp = sp.push(pc.object());
        trace("\npushLiteral: {any}", .{context.stack(newSp, process)});
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
    }
    test "pushLiteral" {
        try Execution.runTest(
            "pushLiteral",
            .{
                tf.pushLiteral,
                17,
                tf.pushLiteral,
                42,
            },
            &[_]Object{},
            &[_]Object{ Object.from(42), Object.from(17) },
        );
    }
};
pub const pushClosure = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const structure = pc.object().to(PackedObject);
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
        const newSp = sp.reserve(3 + includeContext);
        const copySize = stackOffset - stackedFields;
        for (newSp.unreserve(1).slice(copySize), sp.unreserve(stackedFields).slice(copySize)) |*d, s|
            d.* = s;
        const closure = newSp.unreserve(copySize + 1).slice(size + 1);
        for (closure[2 + includeContext ..], temp) |*d, s|
            d.* = s;
        closure[0] = (HeapHeader.calc(.BlockClosure, @truncate(size), @truncate(@intFromPtr(sp)), .onStack, null, Object, false) catch unreachable).o();
        closure[1] = pc.next().object();
        if (includeContext != 0) closure[2] = Object.from(context);
        newSp.top = Object.from(closure.ptr);
        return @call(tailCall, process.check(pc.skip(2).prim()), .{ pc.skip(2).next(), newSp, process, context, extra });
    }
    const testMethod = compileMethod(Sym.yourself, 0, 0, .BlockClosure, .{});
    test "pushClosure" {
        var exe = Execution.initTest("pushClosure", .{
            tf.pushLiteral,
            42,
            tf.pushLiteral,
            True,
            tf.pushLiteral,
            Nil,
            tf.pushLiteral,
            1,
            tf.pushClosure,
            comptime object14(.{ 3, 4, 0 }),
            "0block",
        });
        try exe.resolve(&[_]Object{Object.from(&testMethod)});
        try exe.execute(&[_]Object{
            Object.from(17),
        });
        const stack = exe.stack();
        try expectEqual(Object.from(&stack[2]), stack[0]);
        try expectEqual(Object.from(42), stack[1]);
        const header: HeapHeader = @bitCast(stack[2]);
        try expectEqual(.onStack, header.age);
        try expectEqual(4, header.length);
        try expectEqual(.BlockClosure, header.classIndex);
        try expectEqual(Object.from(&testMethod), stack[3]);
        try expectEqual(Object.from(1), stack[4]);
        try expectEqual(Nil, stack[5]);
        try expectEqual(True, stack[6]);
        try expectEqual(Object.from(17), stack[7]);
    }
};
pub const pushStack = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const offset = pc.object().to(u64);
        const newSp = sp.push(sp.at(offset));
        trace("\npushStack: {any}", .{context.stack(newSp, process)});
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
    }
    test "pushStack" {
        try Execution.runTest(
            "pushStack",
            .{ tf.pushStack, 1, tf.pushStack, 4 },
            &[_]Object{
                Object.from(42),
                Object.from(17),
                Object.from(2),
                Object.from(3),
            },
            &[_]Object{
                Object.from(3),
                Object.from(17),
                Object.from(42),
                Object.from(17),
                Object.from(2),
                Object.from(3),
            },
        );
    }
};
pub const swap = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const saved = sp.top;
        sp.top = sp.next;
        sp.next = saved;
        return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp, process, context, extra });
    }
    test "swap" {
        try Execution.runTest(
            "swap",
            .{tf.swap},
            &[_]Object{
                Object.from(17),
                Object.from(42),
            },
            &[_]Object{
                Object.from(42),
                Object.from(17),
            },
        );
    }
};
pub const testFunctions = if (zag.config.is_test) struct {
    pub const callLabel = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
            const target = pc.targetPC();
            // skip the structure word
            context.setReturn(pc.next().next());
            return @call(tailCall, process.check(target.prim()), .{ target.next(), sp, process, context, Extra{ .method = @constCast(@ptrCast(pc.asCodePtr())) } });
        }
    };
} else struct {};
