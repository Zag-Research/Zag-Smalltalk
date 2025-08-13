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
        while (true) {
            var classes = newPc.packedObject().asU64();
            newPc = newPc.next();
            for (0..4) |_| {
                const currentClass: u14 = @truncate(classes);
                trace("currentClass: {}, match: {}, extra: {f}\n", .{ currentClass, match, extra });
                if (currentClass == match) {
                    newPc = newPc.targetPC();
                    trace("currentClass: extra: {f}\n", .{extra});
                    return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), sp.drop(), process, context, extra });
                }
                if (currentClass == 0) {
                    trace("currentClass: extra: {f}\n", .{extra});
                    return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), sp.drop(), process, context, extra });
                }
                if (currentClass == 0x3FFF) {
                    trace("currentClass: extra: {f}\n", .{extra});
                    return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), sp, process, context, extra });
                }
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
                classCase(.{.True}),
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
            &[_]Object{True()},
            &[_]Object{Object.from(42, null)},
        );
    }
    test "classCase no match" {
        try Execution.runTest(
            "classCase no match",
            .{
                tf.classCase,
                classCase(.{.True}),
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
            &[_]Object{False()},
            &[_]Object{Object.from(42, null)},
        );
    }
    test "classCase no match - leave" {
        try Execution.runTest(
            "classCase no match - leave",
            .{
                tf.classCase,
                object14(.{ ClassIndex.True, 0x3fff }),
                "true",
                tf.branch,
                "end",
                ":true",
                tf.pushLiteral,
                17,
                ":end",
            },
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
        try Execution.runTest(
            "drop",
            .{tf.drop},
            &[_]Object{
                Object.from(17, null),
                Object.from(42, null),
            },
            &[_]Object{
                Object.from(42, null),
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
        try Execution.runTest(
            "dup",
            .{tf.dup},
            &[_]Object{
                Object.from(42, null),
                Object.from(17, null),
            },
            &[_]Object{
                Object.from(42, null),
                Object.from(42, null),
                Object.from(17, null),
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
        try Execution.runTest(
            "over",
            .{tf.over},
            &[_]Object{
                Object.from(17, null),
                Object.from(42, null),
            },
            &[_]Object{
                Object.from(42, null),
                Object.from(17, null),
                Object.from(42, null),
            },
        );
    }
};
pub const pop = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const variable = pc.object().asVariable();
        const result = sp.top;
        if (extra.installContextIfNone(sp.drop(), process, context)) |new| {
            const newSp = new.sp;
            const newExtra = new.extra;
            variable.getAddress(newSp, newExtra).* = result;
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, new.context, newExtra });
        }
        variable.getAddress(sp, extra).* = result;
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
                Object.from(42, null),
            },
            &[_]Object{},
        );
        try expectEqual(Object.from(42, null), association.objects[2]);
    }
};
pub const push = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        trace("Pushing variable...{f} {f}\n", .{pc, extra});
        const variable = pc.object().asVariable();
        if (variable.isLocal and extra.noContext()) {
            if (sp.push(Nil())) |newSp| {
                return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
            } else {
                const newSp, const newContext, const newExtra = process.spillStackAndPush(Nil(), sp, context, extra);
                return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, newContext, newExtra });
            }
        }
        const value = variable.getAddress(sp, extra).*;
        if (sp.push(value)) |newSp| {
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
        } else {
            const newSp, const newContext, const newExtra = process.spillStackAndPush(value, sp, context, extra);
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, newContext, newExtra });
        }
    }
    test "push" {
        if (true) return error.UnimplementedTest;
        try Execution.runTest(
            "push",
            .{ tf.pushLocal, 1, tf.pushLocal, 4 },
            &[_]Object{
                Object.from(42, null),
                Object.from(17, null),
                Object.from(2, null),
                Object.from(3, null),
            },
            &[_]Object{
                Object.from(3, null),
                Object.from(17, null),
                Object.from(42, null),
                Object.from(17, null),
                Object.from(2, null),
                Object.from(3, null),
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
                Object.from(42, null),
            },
        );
    }
};
pub const pushLiteral = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const value = pc.object();
        if (sp.push(value)) |newSp| {
            trace("pushLiteral: {f}\n", .{extra});
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
        } else {
            const newSp, const newContext, const newExtra = process.spillStackAndPush(value, sp, context, extra);
            trace("pushLiteral: {f} {f}\n", .{extra, newExtra});
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, newContext, newExtra });
        }
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
            &[_]Object{ Object.from(42, null), Object.from(17, null) },
        );
    }
};
pub const store = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const variable = pc.object().asVariable();
        const result = sp.top;
        if (extra.installContextIfNone(sp, process, context)) |new| {
            const newSp = new.sp;
            const newExtra = new.extra;
            variable.getAddress(newSp, newExtra).* = result;
            return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, new.context, newExtra });
        }
        variable.getAddress(sp, extra).* = result;
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
