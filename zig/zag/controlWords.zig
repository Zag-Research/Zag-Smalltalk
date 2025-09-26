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
const ClassIndex = object.ClassIndex;
const PackedObject = object.PackedObject;
//const primitives = zag.primitives;
//const globalArena = zag.globalArena;
const symbol = zag.symbol;
const Sym = symbol.symbols;
const Process = zag.Process;
const Context = zag.Context;
const execute = zag.execute;
const PC = execute.PC;
const SP = Process.SP;
const Extra = Context.Extra;
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
            tf.push,
            Object.tests[0],
            ":label",
        });
        assert(@alignOf(@TypeOf(exe)) > 50);
        exe.execute(Object.empty);
        try exe.matchStack(Object.empty);
    }
};
pub const classCase = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        process.dumpStack(sp, "classCase");
        var newPc = pc;
        const top = sp.top;
        const match = @intFromEnum(top.get_class());
        while (true) {
            var classes = newPc.packedObject().asU64();
            newPc = newPc.next();
            for (0..4) |_| {
                const currentClass: u14 = @truncate(classes);
                if (currentClass == match) {
                    newPc = newPc.targetPC();
                    trace("classCase: match {*} extra: {f}\n", .{ sp, extra });
                    return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), sp.drop(), process, context, extra });
                }
                if (currentClass == 0) {
                    trace("classCase: 0 {*} extra: {f}\n", .{ sp, extra });
                    return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), sp.drop(), process, context, extra });
                }
                if (currentClass == 0x3FFF) {
                    trace("classCase: 3fff {*} extra: {f}\n", .{ sp, extra });
                    return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), sp, process, context, extra });
                }
                classes >>= 14;
                newPc = newPc.next();
            }
        }
    }
    test "classCase match" {
        const classes = Object.PackedObject.classes;
        var exe = Execution.initTest(
            "classCase match",
            .{
                tf.classCase,
                comptime classes(&.{.True}),
                "true",
                tf.pushLiteral,
                Object.tests[1],
                tf.branch,
                "end",
                ":true",
                tf.pushLiteral,
                Object.tests[0],
                ":end",
            });
        try exe.runTest(
            &[_]Object{True()},
            &[_]Object{Object.tests[0]},
        );
    }
    test "classCase no match" {
        const classes = Object.PackedObject.classes;
        var exe = Execution.initTest(
            "classCase no match",
            .{
                tf.classCase,
                comptime classes(&.{ .True, .False }),
                "true",
                tf.pushLiteral,
                Object.tests[0],
                tf.branch,
                "end",
                ":true",
                tf.pushLiteral,
                Object.tests[1],
                ":end",
            });
        try exe.runTest(
            &[_]Object{False()},
            &[_]Object{Object.tests[0]},
        );
    }
    test "classCase no match - leave" {
        const classes = Object.PackedObject.classes;
        var exe = Execution.initTest(
            "classCase no match - leave",
            .{
                tf.classCase,
                comptime classes(&.{ .True, .leaveObjectOnStack }),
                "true",
                tf.branch,
                "end",
                ":true",
                tf.pushLiteral,
                Object.tests[0],
                ":end",
            });
        try exe.runTest(
            &[_]Object{False()},
            &[_]Object{False()},
        );
    }
};
pub const drop = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const newSp = sp.drop();
        return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, context, extra });
    }
    test "drop" {
        var exe = Execution.initTest(
            "drop",
            .{tf.drop});
        try exe.runTest(
            &[_]Object{
                exe.object(17),
                exe.object(42),
            },
            &[_]Object{
                exe.object(42),
            },
        );
    }
};
pub const dup = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const value = sp.top;
        if (sp.push(value)) |newSp| {
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, context, extra });
        } else {
            const newSp, const newContext, const newExtra = process.spillStackAndPush(value, sp, context, extra);
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, newContext, newExtra });
        }
    }
    test "dup" {
        var exe = Execution.initTest(
            "dup",
            .{tf.dup});
        try exe.runTest(
            &[_]Object{
                exe.object(42),
                exe.object(17),
            },
            &[_]Object{
                exe.object(42),
                exe.object(42),
                exe.object(17),
            },
        );
    }
};
pub const over = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const value = sp.next;
        if (sp.push(value)) |newSp| {
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, context, extra });
        } else {
            const newSp, const newContext, const newExtra = process.spillStackAndPush(value, sp, context, extra);
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, newContext, newExtra });
        }
    }
    test "over" {
        var exe = Execution.initTest(
            "over",
            .{tf.over});
        try exe.runTest(
            &[_]Object{
                exe.object(17),
                exe.object(42),
            },
            &[_]Object{
                exe.object(42),
                exe.object(17),
                exe.object(42),
            },
        );
    }
};
pub const pop = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const variable = pc.variable();
        const result = sp.top;
        if (extra.installContextIfNone(sp.drop(), process, context)) |new| {
            const newSp = new.sp;
            const newExtra = new.extra;
            variable.getAddress(newSp, newExtra)[0] = result;
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, new.context, newExtra });
        }
        variable.getAddress(sp, extra)[0] = result;
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), sp.drop(), process, context, extra });
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
            "0Nil",
            0,
        });
        association.setLiterals(&.{Nil()}, &.{});
        if (true) return error.SkipZigTest;
        var exe = Execution.initTest(
            "popAssociationValue",
            .{
                tf.popAssociationValue,
                "0object",
            });
        try exe.runTestWithObjects(
            &.{
                association.asObject(),
            },
            &[_]Object{
                exe.object(42),
            },
            &[_]Object{},
        );
        try expectEqual(exe.object(42), association.objects[2]);
    }
};
pub const push = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        process.dumpStack(sp, "push");
        const variable = pc.variable();
        trace("Pushing variable...{*} {f} {f} {f}\n", .{ sp, pc, extra, variable });
        if (variable.isLocal and extra.noContext()) {
            if (sp.push(Nil())) |newSp| {
                return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
            } else {
                const newSp, const newContext, const newExtra = process.spillStackAndPush(Nil(), sp, context, extra);
                return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, newContext, newExtra });
            }
        }
        const address = variable.getAddress(sp, extra);
        const value = address[0];
        trace(" .... {*} {f}\n", .{ address, value });
        if (sp.push(value)) |newSp| {
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
        } else {
            const newSp, const newContext, const newExtra = process.spillStackAndPush(value, sp, context, extra);
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, newContext, newExtra });
        }
    }
    test "push" {
        if (true) return error.UnimplementedTest;
        var exe = Execution.initTest(
            "push",
            .{ tf.pushLocal, 1, tf.pushLocal, 4 });
        try exe.runTest(
            &[_]Object{
                exe.object(42),
                exe.object(17),
                exe.object(2),
                exe.object(3),
            },
            &[_]Object{
                exe.object(3),
                exe.object(17),
                exe.object(42),
                exe.object(17),
                exe.object(2),
                exe.object(3),
            },
        );
    }
};
pub const pushAssociationValue = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const value = pc.object().getField(2);
        if (sp.push(value)) |newSp| {
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
        } else {
            const newSp, const newContext, const newExtra = process.spillStackAndPush(value, sp, context, extra);
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, newContext, newExtra });
        }
    }
    test "pushAssociationValue" {
        var association = compileObject(.{
            ":def",
            c.Association,
            "0Nil",
            Object.tests[0],
        });
        var exe = Execution.initTest(
            "pushAssociationValue",
            .{
                tf.pushAssociationValue,
                "0object",
            });
        try exe.runTestWithObjects(
            &.{
                association.asObject(),
            },
            &[_]Object{},
            &[_]Object{
                exe.object(42),
            },
        );
    }
};
pub const pushLiteral = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        process.dumpStack(sp, "pushLiteral");
        const value = pc.object();
        if (sp.push(value)) |newSp| {
            trace("pushLiteral: {*} {f}\n", .{ newSp, extra });
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
        } else {
            const newSp, const newContext, const newExtra = process.spillStackAndPush(value, sp, context, extra);
            trace("pushLiteral: {*} {f} {f}\n", .{ newSp, extra, newExtra });
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, newContext, newExtra });
        }
    }
    test "pushLiteral" {
        var exe = Execution.initTest(
            "pushLiteral",
            .{
                tf.pushLiteral,
                Object.tests[1],
                tf.pushLiteral,
                Object.tests[0],
            });
        try exe.runTest(
            &[_]Object{},
            &[_]Object{ Object.tests[0], Object.tests[1] },
        );
    }
};
pub const store = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const variable = pc.variable();
        const result = sp.top;
        if (extra.installContextIfNone(sp, process, context)) |new| {
            const newSp = new.sp;
            const newExtra = new.extra;
            variable.getAddress(newSp, newExtra)[0] = result;
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, new.context, newExtra });
        }
        variable.getAddress(sp, extra)[0] = result;
        return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), sp, process, context, extra });
    }
};
pub const callLabel = if (zag.config.is_test) struct {
    pub const hidden = true;
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
        if (true) unreachable;
        const target = pc.targetPC();
        // skip the structure word
        context.setReturn(pc.next().next());
        return @call(tailCall, process.check(target.prim()), .{ target.next(), sp, process, context, Extra.forMethod(@ptrCast(pc.asCodePtr()), sp) });
    }
} else struct {};
