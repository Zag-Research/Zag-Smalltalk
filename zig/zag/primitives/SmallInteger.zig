const std = @import("std");
const zag = @import("../zag.zig");
const config = zag.config;
const tailCall = config.tailCall;
const trace = config.trace;
const execute = zag.execute;
const Context = zag.Context;
const Code = execute.Code;
const PC = execute.PC;
const SP = execute.SP;
const Extra = execute.Extra;
const Result = execute.Result;
const Execution = execute.Execution;
const CompiledMethod = execute.CompiledMethod;
const Process = zag.Process;
const object = zag.object;
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const Sym = zag.symbol.symbols;
const heap = zag.heap;
const empty = &[0]Object{};
const tf = zag.threadedFn.Enum;

pub const moduleName = "SmallInteger";
pub fn init() void {}
pub const inlines = struct {
    pub fn p1(self: Object, other: Object) !Object { // INLINED - Add
        if (other.isInt()) {
            switch (config.objectEncoding) {
                .nan => {
                    const result = @as(Object, @bitCast(self.rawI() +% other.toUnchecked(i64)));
                    if (result.isInt()) return result;
                },
                .tag => {
                    const result, const overflow = @addWithOverflow(self.rawI(), other.untaggedI());
                    if (overflow == 0) return @bitCast(result);
                },
            }
        }
        return error.primitiveError;
    }
    pub inline fn p_negated(self: Object) !Object { // Negate
        switch (config.objectEncoding) {
            .nan => {
                const result = @as(Object, @bitCast(object.Object.u64_ZERO2 -% self.rawU()));
                if (result.isInt()) return result;
            },
            .tag => {
                const result, const overflow = @addWithOverflow(@as(u64, 256), self.rawU() ^ 0xffff_ffff_ffff_ff00);
                if (overflow == 0) {
                    const obj: Object = @bitCast(result);
                    if (obj != self)
                        return obj;
                } else if (self == Object.makeImmediate(.SmallInteger, 0))
                    return self;
            },
        }
        return error.primitiveError;
    }
    pub inline fn p2(self: Object, other: Object) !Object { // Subtract
        if (other.isInt()) {
            const result, const overflow = @subWithOverflow(self.rawI(), other.untaggedI());
            if (overflow == 0) return @bitCast(result);
        }
        return error.primitiveError;
    }
    pub inline fn p3(self: Object, other: Object) !bool { // LessThan
        if (!other.isInt()) return error.primitiveError;
        return self.rawI() < other.rawI();
    }
    pub inline fn p4(self: Object, other: Object) !bool { // GreaterThan
        if (!other.isInt()) return error.primitiveError;
        return self.rawI() > other.rawI();
    }
    pub inline fn p5(self: Object, other: Object) !bool { // LessOrEqual
        if (!other.isInt()) return error.primitiveError;
        return self.rawI() <= other.rawI();
    }
    pub fn p5N(self: Object, other: Object) bool { // INLINED - LessOrEqual when both known SmallIntegers
        return switch (config.objectEncoding) {
            .nan => self.rawU() <= other.rawU(),
            .tag => self.rawI() <= other.rawI(),
        };
    }
    pub inline fn p6(self: Object, other: Object) !bool { // GreaterOrEqual
        if (!other.isInt()) return error.primitiveError;
        return self.rawI() >= other.rawI();
    }
    pub inline fn p7(self: Object, other: Object) !bool { // Equal
        if (!other.isInt()) return error.primitiveError;
        return self.rawI() == other.rawI();
    }
    pub inline fn p8(self: Object, other: Object) !bool { // NotEqual
        if (!other.isInt()) return error.primitiveError;
        return self.rawI() != other.rawI();
    }
    inline fn unsafeAbs(x: i64) u64 {
        @setRuntimeSafety(false);
        return @as(u64, @intCast(if (x < 0) -x else x));
    }
    pub inline fn p9(self: Object, other: Object) !Object { // Multiply
        if (other.isInt()) {
            const s = @as(i56, @truncate(self.toUnchecked(i64)));
            const o = @as(i56, @truncate(other.toUnchecked(i64)));
            const result, const overflow = @mulWithOverflow(s, o);
            if (overflow == 0) return Object.from(result);
        }
        return error.primitiveError;
    }
    pub inline fn p9Orig(self: Object, other: Object) !Object { // Multiply
        if (other.isInt()) {
            const s = self.toUnchecked(i64);
            const sBits = @clz(unsafeAbs(s));
            const o = other.toUnchecked(i64);
            const oBits = @clz(unsafeAbs(o));
            if (sBits + oBits > 13) return Object.from(s * o);
            if (sBits + oBits == 13) {
                const result = s * o;
                if (@ctz(result) == 50) return Object.from(result);
            }
        }
        return error.primitiveError;
    }
};

test "inline primitives" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(Object.from(12), inlines.p9(Object.from(3), Object.from(4)));
    try expectEqual(error.primitiveError, inlines.p9(Object.from(0x1_0000_0000), Object.from(0x100_0000)));
    try expectEqual(error.primitiveError, inlines.p9(Object.from(0x1_0000_0000), Object.from(0x80_0000)));
    try expectEqual(Object.from(-0x80_0000_0000_0000), inlines.p9(Object.from(0x1_0000_0000), Object.from(-0x80_0000)));
    try expectEqual(Object.from(0x20_0000_0000_0000), inlines.p9(Object.from(0x1_0000_0000), Object.from(0x20_0000)));
    try expectEqual(Object.from(0x3f_ffff_0000_0000), inlines.p9(Object.from(0x1_0000_0000), Object.from(0x3f_ffff)));
    try expectEqual(Object.from(0), inlines.p_negated(Object.from(0)));
    try expectEqual(Object.from(-42), inlines.p_negated(Object.from(42)));
    try expectEqual(Object.from(0x7f_ffff_ffff_ffff), inlines.p_negated(Object.from(-0x7f_ffff_ffff_ffff)));
    try expectEqual(Object.from(-0x7f_ffff_ffff_ffff), inlines.p_negated(Object.from(0x7f_ffff_ffff_ffff)));
    try expectEqual(error.primitiveError, inlines.p_negated(Object.from(-0x80_0000_0000_0000)));
    try expectEqual(true, try inlines.p5(Object.from(0), Object.from(0)));
    try expectEqual(true, try inlines.p5(Object.from(0), Object.from(1)));
    try expectEqual(false, try inlines.p5(Object.from(1), Object.from(0)));
    try expectEqual(true, inlines.p5N(Object.from(0), Object.from(0)));
    try expectEqual(true, inlines.p5N(Object.from(0), Object.from(1)));
    try expectEqual(false, inlines.p5N(Object.from(1), Object.from(0)));
    try expectEqual(true, try inlines.p6(Object.from(0), Object.from(0)));
    try expectEqual(false, try inlines.p6(Object.from(0), Object.from(1)));
    try expectEqual(true, try inlines.p6(Object.from(1), Object.from(0)));
}
pub const embedded = struct {
    const fallback = execute.fallback;
    const plus = Extra.from(Sym.@"+", .SmallInteger);
    const minus = Extra.from(Sym.@"-", .SmallInteger);
    pub const SmallInteger = struct {
        pub fn @"+"(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
            const newSp = sp.dropPut(inlines.p1(sp.next, sp.top) catch return @call(tailCall, process.check(fallback), .{ pc, sp, process, context, plus }));
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, context, undefined });
        }
        pub fn @"+_L1"(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
            sp.top = inlines.p1L(sp.top, 1) catch {
                const newSp = sp.push(Object.from(1));
                return @call(tailCall, process.check(fallback), .{ pc, newSp, process, context, plus });
            };
            return @call(tailCall, process.check(pc.prim), .{ pc.next(), sp, process, context, undefined });
        }
        pub fn @"-"(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
            sp[1] = inlines.p2(sp[1], sp[0]) catch return @call(tailCall, process.check(fallback), .{ pc, sp, process, context, minus });
            return @call(tailCall, process.check(pc[0].prim), .{ pc, sp + 1, process, context, undefined });
        }
        pub fn @"-_L1"(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
            trace("\n-_L1: {any}", .{context.stack(sp, process)});
            sp.top = inlines.p2L(sp.top, 1) catch {
                const newSp = sp.push(Object.from(1));
                return @call(tailCall, process.check(fallback), .{ pc, newSp, process, context, minus });
            };
            trace(" -> {any}", .{context.stack(sp, process)});
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp, process, context, undefined });
        }
        pub fn @"-_L2"(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
            trace("\n-_L2: {any}", .{context.stack(sp, process)});
            sp.top = inlines.p2L(sp.top, 2) catch {
                const newSp = sp.push(Object.from(2));
                return @call(tailCall, process.check(fallback), .{ pc, newSp, process, context, minus });
            };
            trace(" -> {any}", .{context.stack(sp, process)});
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp, process, context, undefined });
        }
        pub fn @"<="(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
            const newSp = sp.dropPut(Object.from(inlines.p5(sp[1], sp[0]) catch {
                return @call(tailCall, process.check(fallback), .{ pc, sp, process, context, Sym.@"<=", undefined });
            }));
            return @call(tailCall, process.check(pc.prim), .{ pc.next(), newSp, process, context, undefined });
        }
        pub fn @"<=_N"(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp.dropPut(Object.from(inlines.p5N(sp.next, sp.top))), process, context, undefined });
        }
        pub fn @"*"(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
            const newSp = sp.dropPut(inlines.p9Orig(sp[1], sp[0]) catch return @call(tailCall, fallback, .{ pc, sp, process, context, Extra.from(Sym.@"*", .SmallInteger) }));
            return @call(tailCall, process.check(pc.prim), .{ pc.next(), newSp, process, context, undefined });
        }
    };
};
pub const @"+" = struct {
    pub const number = 1;
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#+
        if (inlines.p1(sp.next, sp.top)) |result| {
            const newSp = sp.dropPut(result);
            return @call(tailCall, process.check(context.npc.f), .{ context.tpc, newSp, process, context, undefined });
        } else |_| {}
        return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
    }
    test "simple add" {
        try Execution.runTest(
            "simple add",
            .{ tf.primitive, 1 },
            &[_]Object{
                Object.from(25),
                Object.from(17),
            },
            &[_]Object{
                Object.from(42),
            },
        );
    }
    test "simple add with overflow" {
        try Execution.runTest(
            "simple add with overflow",
            .{ tf.primitive, 1, tf.pushLiteral, 42 },
            &[_]Object{
                Object.from(4),
                Object.from(0x7f_ffff_ffff_ffff),
            },
            &[_]Object{
                Object.from(42),
                Object.from(4),
                Object.from(0x7f_ffff_ffff_ffff),
            },
        );
    }
};
pub const @"-" = struct {
    pub const number = 2;
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#-
        trace("\n-: {any}", .{context.stack(sp, process)});
        trace("\np2: {any}", .{context.stack(sp, process)});
        const newSp = sp.dropPut(inlines.p2(sp.next, sp.top) catch
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp, process, context, extra }));
        return @call(tailCall, process.check(context.npc.f), .{ context.tpc, newSp, process, context, undefined });
    }
};
pub fn p7(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // at:
    _ = .{ pc, sp, process, context, extra, unreachable };
}
pub fn p5(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) Result { // SmallInteger>>#<=
    trace("\np5: {any}", .{context.stack(sp, process)});
    const newSp = sp.dropPut(Object.from(inlines.p5(sp.next, sp.top) catch
        return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp, process, context, signature })));
    return @call(tailCall, process.check(context.npc.f), .{ context.tpc, newSp, process, context, undefined });
}
pub fn p9(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) Result { // SmallInteger>>#*
    const newSp = sp.dropPut(inlines.p9(sp.next, sp.top) catch
        return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp, process, context, signature }));
    return @call(tailCall, process.check(context.npc.f), .{ context.tpc, newSp, process, context, undefined });
}
