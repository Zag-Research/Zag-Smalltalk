const std = @import("std");
const config = @import("../config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const execute = @import("../execute.zig");
const Context = zag.Context;
const Code = execute.Code;
const PC = execute.PC;
const SP = execute.SP;
const compileMethod = execute.compileMethod;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const Extra = execute.Extra;
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
const empty = &[0]Object{};
const symbols = @import("../symbol.zig").symbols;

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
                    const result = @addWithOverflow(self.rawI(), other.untaggedI());
                    if (result[1] == 0) return result[0];
                },
            }
        }
        return error.primitiveError;
    }
    pub inline fn p1L(self: Object, other: i32) !Object { // Add a positive literal
        switch (config.objectEncoding) {
            .nan => {
                const result = @as(Object, @bitCast(self.rawI() +% other));
                if (result.atMostInt()) return result;
            },
            .tag => {
                const result = @addWithOverflow(self.rawI(), Object.shiftI(other));
                if (result[1] == 0) return result[0];
            },
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
                const result = @subWithOverflow(@as(i64, 0), self.rawI());
                if (result[1] == 0) return result[0];
            },
        }
        return error.primitiveError;
    }
    pub inline fn p2(self: Object, other: Object) !Object { // Subtract
        if (other.isInt()) {
            const result = @as(Object, @bitCast(self.rawI() -% other.toUnchecked(i64)));
            if (result.isInt()) return result;
        }
        return error.primitiveError;
    }
    pub fn p2L(self: Object, other: i32) !Object { // INLINED - Subtract a positive literal
        const result = @as(Object, @bitCast(self.rawI() -% other));
        if (result.atLeastInt()) return result;
        return error.primitiveError;
    }
    pub inline fn p3(self: Object, other: Object) !bool { // LessThan
        if (!other.isInt()) return error.primitiveError;
        return self.u() < other.u();
    }
    pub inline fn p4(self: Object, other: Object) !bool { // GreaterThan
        if (!other.isInt()) return error.primitiveError;
        return self.u() > other.u();
    }
    pub inline fn p5(self: Object, other: Object) !bool { // LessOrEqual
        if (!other.isInt()) return error.primitiveError;
        return self.rawU() <= other.rawU();
    }
    pub fn p5N(self: Object, other: Object) bool { // INLINED - LessOrEqual when both known SmallIntegers
        return switch (config.objectEncoding) {
            .nan => self.rawU() <= other.rawU(),
            .tag => self.rawI() <= other.rawI(),
        };
    }
    pub inline fn p6(self: Object, other: Object) !bool { // GreaterOrEqual
        if (!other.isInt()) return error.primitiveError;
        return self.rawU() >= other.rawU();
    }
    pub inline fn p7(self: Object, other: Object) !bool { // Equal
        if (!other.isInt()) return error.primitiveError;
        return self.u() == other.u();
    }
    pub inline fn p8(self: Object, other: Object) !bool { // NotEqual
        if (!other.isInt()) return error.primitiveError;
        return self.u() != other.u();
    }
    inline fn unsafeAbs(x: i64) u64 {
        @setRuntimeSafety(false);
        return @as(u64, @intCast(if (x < 0) -x else x));
    }
    pub inline fn p9(self: Object, other: Object) !Object { // Multiply
        if (other.isInt()) {
            const s = @as(i51, @truncate(self.toUnchecked(i64)));
            const o = @as(i51, @truncate(other.toUnchecked(i64)));
            const result = @mulWithOverflow(s, o);
            if (result.@"1" == 0) return Object.from(result.@"0");
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
    try expectEqual((try inlines.p9(Object.from(3), Object.from(4))).toInt(), 12);
    try expectEqual(inlines.p9(Object.from(0x10000000), Object.from(0x1000000)), error.primitiveError);
    try expectEqual(inlines.p9(Object.from(0x10000000), Object.from(0x800000)), error.primitiveError);
    try expectEqual((try inlines.p9(Object.from(0x1000_0000), Object.from(0x20_0000))).toInt(), 0x2_0000_0000_0000);
    try expectEqual((try inlines.p9(Object.from(0x1000_0000), Object.from(0x3f_ffff))).toInt(), 0x3_ffff_f000_0000);
    try expectEqual((try inlines.p9(Object.from(0x1000_0010), Object.from(0x3f_ffff))).toInt(), 0x3_ffff_f3ff_fff0);
    try expectEqual((try inlines.p9(Object.from(0x1000_0000), Object.from(-0x400_000))).toInt(), -0x4_0000_0000_0000);
    try expectEqual(inlines.p9(Object.from(0x1000_0000), Object.from(0x400_000)), error.primitiveError);
    try expectEqual((try inlines.p_negated(Object.from(42))).toInt(), -42);
    try expectEqual((try inlines.p_negated(Object.from(-0x3_ffff_ffff_ffff))).toInt(), 0x3_ffff_ffff_ffff);
    try expectEqual((try inlines.p_negated(Object.from(0x3_ffff_ffff_ffff))).toInt(), -0x3_ffff_ffff_ffff);
    try expectEqual((try inlines.p_negated(Object.from(0))).toInt(), 0);
    try expectEqual(inlines.p_negated(Object.from(-0x4_0000_0000_0000)), error.primitiveError);
    try expectEqual(try inlines.p5(Object.from(0), Object.from(0)), true);
    try expectEqual(try inlines.p5(Object.from(0), Object.from(1)), true);
    try expectEqual(try inlines.p5(Object.from(1), Object.from(0)), false);
    try expectEqual(inlines.p5N(Object.from(0), Object.from(0)), true);
    try expectEqual(inlines.p5N(Object.from(0), Object.from(1)), true);
    try expectEqual(inlines.p5N(Object.from(1), Object.from(0)), false);
    try expectEqual(try inlines.p6(Object.from(0), Object.from(0)), true);
    try expectEqual(try inlines.p6(Object.from(0), Object.from(1)), false);
    try expectEqual(try inlines.p6(Object.from(1), Object.from(0)), true);
}
pub const embedded = struct {
    const fallback = execute.fallback;
    const plus = Extra.from(symbols.@"+", .SmallInteger);
    const minus = Extra.from(symbols.@"-", .SmallInteger);
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
                return @call(tailCall, process.check(fallback), .{ pc, sp, process, context, symbols.@"<=", undefined });
            }));
            return @call(tailCall, process.check(pc.prim), .{ pc.next(), newSp, process, context, undefined });
        }
        pub fn @"<=_N"(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
            return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp.dropPut(Object.from(inlines.p5N(sp.next, sp.top))), process, context, undefined });
        }
        pub fn @"*"(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result {
            const newSp = sp.dropPut(inlines.p9Orig(sp[1], sp[0]) catch return @call(tailCall, fallback, .{ pc, sp, process, context, Extra.from(symbols.@"*", .SmallInteger) }));
            return @call(tailCall, process.check(pc.prim), .{ pc.next(), newSp, process, context, undefined });
        }
    };
};
pub const primitive1 = struct {
    pub const number = 1;
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#+
        if (inlines.p1(sp.next, sp.top)) | result| {
            const newSp = sp.dropPut(result);
            return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, undefined });
        } else |_| {};
        return @call(tailCall, extra.threadedFn(), .{ pc, sp, process, context, extra.encoded() });
    }
};
pub const primitive2 = struct {
    pub const number = 2;
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result { // SmallInteger>>#-
        trace("\n-: {any}", .{context.stack(sp, process)});
        trace("\np2: {any}", .{context.stack(sp, process)});
        const newSp = sp.dropPut(inlines.p2(sp.next, sp.top) catch
                                     return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp, process, context, signature }));
        return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, undefined });
    }
};
pub fn p7(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) Result { // at:
    if (pc.verifyMethod(signature)) return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp, process, context, signature });
    @panic("p7 unimplemented");
}
pub fn p5(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) Result { // SmallInteger>>#<=
    trace("\n<=: {any} 0x{x} 0x{x}", .{ context.stack(sp, process), Sym.@"<=".withClass(.SmallInteger).u(), signature.u() });
    if (pc.verifyMethod(signature)) return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp, process, context, signature });
    trace("\np5: {any}", .{context.stack(sp, process)});
    const newSp = sp.dropPut(Object.from(inlines.p5(sp.next, sp.top) catch
                                             return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp, process, context, signature })));
    return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, undefined });
}
pub fn p9(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) Result { // SmallInteger>>#*
    if (pc.verifyMethod(signature)) return @call(tailCall, process.check(pc.prim()), .{ pc.next(), sp, process, context, signature });
    const newSp = sp.dropPut(inlines.p9(sp.next, sp.top) catch
                                 return @call(tailCall, process.check(pc.prim), .{ pc.next(), sp, process, context, signature }));
    return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, undefined });
}

const e = struct {
    usingnamespace execute.controlPrimitives;
    usingnamespace embedded;
};
fn testExecute(ptr: anytype) []Object {
    const method: CompiledMethodPtr = @ptrCast(ptr);
    var te = execute.Execution.new();
    std.debug.print("\nbefore te.init", .{});
    te.init();
    std.debug.print("\nbefore te.run", .{});
    const result = te.run(&[_]Object{Nil}, method);
    return result;
}
test "simple add" {
    const expectEqual = std.testing.expectEqual;
    std.debug.print("\nbefore prog", .{});
    var prog = compileMethod(Sym.value, 0, 0, .none, .{
        &e.pushContext, "^",
        &e.pushLiteral, Object.from(3),
        &e.pushLiteral, Object.from(40),
        &e.call,        "0Obj",
        &e.pushLiteral, Object.from(-1),
        &e.call,        "0Obj",
        &e.returnTop,
    });
    var method2 = compileMethod(Sym.@"+", 0, 0, .SmallInteger, .{
        &e.SmallInteger.@"+",
        &e.returnNoContext,
    });
    std.debug.print("\nbefore setliteral", .{});
    prog.setLiterals(empty, &[_]Object{Object.from(&method2)});
    std.debug.print("\nbefore execute", .{});
    const result = testExecute(&prog);
    try expectEqual(result[0].toInt(), 42);
}
test "embedded add" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(Sym.value, 0, 0, .none, .{
        &e.pushContext,       "^",
        &e.pushLiteral,       Object.from(3),
        &e.pushLiteral,       Object.from(40),
        &e.SmallInteger.@"+", &e.pushLiteral,
        Object.from(-1),      &e.SmallInteger.@"+",
        &e.returnTop,
    });
    const result = testExecute(&prog);
    try expectEqual(result[0].toInt(), 42);
}
test "embedded add for LLVM experiment" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(Sym.value, 0, 0, .none, .{
        &e.pushLiteral,        Object.from(3),
        &e.pushLiteral,        Object.from(40),
        &e.SmallInteger.@"+",  &e.pushLiteral,
        Object.from(-1),       &e.SmallInteger.@"+",
        &e.returnTopNoContext, Object.from(0),
    });
    const result = testExecute(&prog);
    try expectEqual(result[0].toInt(), 42);
}
test "simple add with overflow" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(Sym.value, 0, 2, .none, .{
        &e.pushContext, "^",
        &e.pushLiteral, Object.from(4),
        &e.pushLiteral, Object.from(0x3_ffff_ffff_ffff),
        &e.printStack,  &e.SmallInteger.@"+",
        &e.returnTop,
    });
    var prog2 = compileMethod(Sym.@"+", 0, 0, .none, .{
        &e.printStack,
        &e.pushLiteral,
        Sym.noFallback,
        &e.returnNoContext,
    });
    prog2.asCompiledMethodPtr().forDispatch(object.ClassIndex.SmallInteger);
    const result = testExecute(&prog);
    std.debug.print("\nresult = {any}", .{result});
    try expectEqual(result[0], Sym.noFallback);
}

test "dispatch3" {}
pub fn main() void {
    var prog = compileMethod(Sym.value, 0, 0, .{
        &e.pushLiteral,    3,
        &e.pushLiteral,    4,
        &e.p1,             &e.pushLiteral,
        Object.from(-999), &e.returnNoContext,
    });
    _ = testExecute(prog.asCompiledMethodPtr());
}
