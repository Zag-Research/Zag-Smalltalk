const std = @import("std");
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;
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
const combine14asObject = object.PackedObject.combine14asObject;
const classes14 = object.PackedObject.classes14;
const tf = zag.threadedFn.Enum;
const c = object.ClassIndex;
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
        std.debug.print("Alignment of exe: {} {}\n", .{ @alignOf(@TypeOf(exe)), @TypeOf(exe) });
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
                comptime combine14asObject(.{class.True}),
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
                comptime classes14(.{class.True}),
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
                comptime combine14asObject(.{ class.True, 0x3fff }),
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
pub const pushStack = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const offset = pc.object().to(u64);
        const newSp = sp.push(sp.at(offset));
        trace("\npushStack: {any}", .{context.stack(newSp, process)});
        return @call(tailCall, process.check(pc.prim2()), .{ pc.skip(2), newSp, process, context, extra });
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
