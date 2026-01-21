const std = @import("std");
const zag = @import("zag");
const threadedFn = zag.threadedFn;
const tf = threadedFn.Enum;
const Process = zag.Process;
const Context = zag.Context;
const Object = zag.Object;
const Code = zag.execute.Code;
const compileMethod = zag.execute.compileMethod;
const Sym = zag.symbol.symbols;
const SmallInteger = zag.primitives.primitives.SmallInteger;
const False = zag.object.False;
const True = zag.object.True;

const JitMethod = @import("jit_method.zig").JitMethod;
const harness = @import("test_harness.zig");
const initJitTest = harness.initJitTest;
const runCompiled = harness.runCompiled;
const setLiteral = harness.setLiteral;
const reportResult = harness.reportResult;
const opsInfo = harness.opsInfo;

pub const PushTest = struct {
    const self = Context.makeVariable(0, 1, .Parameter, &.{});
    const tup = .{
        tf.push,
        self,
        tf.returnTop,
    };
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops);

    var method: Method = undefined;
    var process: Process align(Process.alignment) = undefined;
    var compiled align(64) = compileMethod(Sym.value, 0, .Object, tup);

    pub fn init() !void {
        try initJitTest(&method, &process, "PushTest: push -> returnTop");
    }

    pub fn deinit() void {
        method.deinit();
    }

    pub fn runWith(value: i56) i64 {
        process.init();
        const context = process.getContext();
        var sp = process.endOfStack().safeReserve(1);
        if ((@intFromPtr(sp) & 0xffff) == 0) {
            sp = sp.safeReserve(1);
        }
        sp.top = Object.from(value, sp, context);
        process.setSp(sp);
        return runCompiled(&method, &compiled, &process, info.positions[0..], sp);
    }

    pub fn run() !void {
        const result = runWith(123);
        try reportResult("Input: 123, Result", result, 123);
    }
};

pub const PushLiteralTest = struct {
    const tup = .{
        tf.pushLiteral,
        "0const",
        tf.returnTop,
    };
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops);

    var method: Method = undefined;
    var process: Process align(Process.alignment) = undefined;
    var compiled align(64) = compileMethod(Sym.value, 0, .Object, tup);
    var literal_: Object.StaticObject = undefined;

    pub fn init() !void {
        try initJitTest(&method, &process, "PushLiteralTest: pushLiteral -> returnTop");
        const literal = literal_.init(7);
        compiled.resolve(&[_]Object{literal}) catch @panic("Failed to resolve");
    }

    pub fn deinit() void {
        method.deinit();
    }

    pub fn runWith(value: i56) i64 {
        const context = process.getContext();
        const sp = process.endOfStack();
        const code = compiled.code[0..];
        setLiteral(code, 1, Object.from(value, sp, context));
        return runCompiled(&method, &compiled, &process, info.positions[0..], null);
    }

    pub fn run() !void {
        const result = runWith(7);
        try reportResult("Input: 7, Result", result, 7);
    }
};

pub const PushLiteralDupDropTest = struct {
    const tup = .{
        tf.pushLiteral,
        "0const",
        tf.dup,
        tf.drop,
        tf.returnTop,
    };
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops);

    var method: Method = undefined;
    var process: Process align(Process.alignment) = undefined;
    var compiled align(64) = compileMethod(Sym.value, 0, .Object, tup);
    var literal_: Object.StaticObject = undefined;

    pub fn init() !void {
        try initJitTest(&method, &process, "PushLiteralDupDropTest: pushLiteral -> dup -> drop -> returnTop");
        const literal = literal_.init(99);
        compiled.resolve(&[_]Object{literal}) catch @panic("Failed to resolve");
    }

    pub fn deinit() void {
        method.deinit();
    }

    pub fn runWith(value: i56) i64 {
        const context = process.getContext();
        const sp = process.endOfStack();
        const code = compiled.code[0..];
        setLiteral(code, 1, Object.from(value, sp, context));
        return runCompiled(&method, &compiled, &process, info.positions[0..], null);
    }

    pub fn run() !void {
        const result = runWith(99);
        try reportResult("Input: 99, Result", result, 99);
    }
};

pub const TailCallPatchTest = struct {
    const tup = .{
        tf.pushLiteral,
        "0const",
        tf.dup,
        tf.drop,
        tf.pushLiteral,
        "1const",
        tf.drop,
        tf.pushLiteral,
        "2const",
        tf.returnTop,
    };
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops);

    var method: Method = undefined;
    var process: Process align(Process.alignment) = undefined;
    var compiled align(64) = compileMethod(Sym.value, 0, .Object, tup);
    var first_: Object.StaticObject = undefined;
    var second_: Object.StaticObject = undefined;
    var third_: Object.StaticObject = undefined;

    pub fn init() !void {
        try initJitTest(&method, &process, "TailCallPatchTest: linear chain with multiple ops");
        const first = first_.init(11);
        const second = second_.init(22);
        const third = third_.init(33);
        compiled.resolve(&[_]Object{ first, second, third }) catch @panic("Failed to resolve");
    }

    pub fn deinit() void {
        method.deinit();
    }

    pub fn runWith(a: i56, b: i56, c: i56) i64 {
        const context = process.getContext();
        const sp = process.endOfStack();
        const code = compiled.code[0..];
        setLiteral(code, 1, Object.from(a, sp, context));
        setLiteral(code, 5, Object.from(b, sp, context));
        setLiteral(code, 9, Object.from(c, sp, context));
        return runCompiled(&method, &compiled, &process, info.positions[0..], null);
    }

    pub fn run() !void {
        const result = runWith(11, 22, 33);
        try reportResult("Result", result, 33);
    }
};

pub const MixedLinearTest = struct {
    const self = Context.makeVariable(0, 1, .Parameter, &.{});
    const tup = .{
        tf.push,
        self,
        tf.pushLiteral,
        "0const",
        tf.dup,
        tf.drop,
        tf.returnTop,
    };
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops);

    var method: Method = undefined;
    var process: Process align(Process.alignment) = undefined;
    var compiled align(64) = compileMethod(Sym.value, 0, .Object, tup);
    var literal_: Object.StaticObject = undefined;

    pub fn init() !void {
        try initJitTest(&method, &process, "MixedLinearTest: push -> pushLiteral -> dup -> drop -> returnTop");
        const literal = literal_.init(77);
        compiled.resolve(&[_]Object{literal}) catch @panic("Failed to resolve");
    }

    pub fn deinit() void {
        method.deinit();
    }

    pub fn runWith(self_value: i56, literal_value: i56) i64 {
        process.init();
        const context = process.getContext();
        var sp = process.endOfStack().safeReserve(1);
        if ((@intFromPtr(sp) & 0xffff) == 0) {
            sp = sp.safeReserve(1);
        }
        sp.top = Object.from(self_value, sp, context);
        process.setSp(sp);

        const code = compiled.code[0..];
        setLiteral(code, 3, Object.from(literal_value, sp, context));
        return runCompiled(&method, &compiled, &process, info.positions[0..], sp);
    }

    pub fn run() !void {
        const result = runWith(5, 77);
        try reportResult("Result", result, 77);
    }
};

pub const ReturnOpsTest = struct {
    const self = Context.makeVariable(0, 1, .Parameter, &.{});
    const tup = .{
        tf.pushLiteral,
        "0const",
        tf.branchFalse,
        "false",
        tf.returnSelf,
        ":false",
        tf.push,
        self,
        tf.returnTop,
    };
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops);

    var method: Method = undefined;
    var process: Process align(Process.alignment) = undefined;
    var compiled align(64) = compileMethod(Sym.value, 0, .Object, tup);
    var literal_: Object.StaticObject = undefined;

    pub fn init() !void {
        try initJitTest(&method, &process, "ReturnOpsTest: returnSelf vs returnTop");
        const literal = literal_.init(1);
        compiled.resolve(&[_]Object{literal}) catch @panic("Failed to resolve");
    }

    pub fn deinit() void {
        method.deinit();
    }

    pub fn runWith(self_value: i56, cond: Object) i64 {
        process.init();
        const context = process.getContext();
        var sp = process.endOfStack().safeReserve(1);
        if ((@intFromPtr(sp) & 0xffff) == 0) {
            sp = sp.safeReserve(1);
        }
        sp.top = Object.from(self_value, sp, context);
        process.setSp(sp);

        const code = compiled.code[0..];
        setLiteral(code, 1, cond);
        return runCompiled(&method, &compiled, &process, info.positions[0..], sp);
    }

    pub fn run() !void {
        const result_true = runWith(42, True());
        try reportResult("Result (True)", result_true, 42);

        const result_false = runWith(42, False());
        try reportResult("Result (False)", result_false, 42);
    }
};

pub const ReturnSelfTest = struct {
    const tup = .{
        tf.returnSelf,
    };
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops);

    var method: Method = undefined;
    var process: Process align(Process.alignment) = undefined;
    var compiled align(64) = compileMethod(Sym.value, 0, .Object, tup);

    pub fn init() !void {
        try initJitTest(&method, &process, "ReturnSelfTest: returnSelf");
    }

    pub fn deinit() void {
        method.deinit();
    }

    pub fn runWith(self_value: i56) i64 {
        process.init();
        const context = process.getContext();
        var sp = process.endOfStack().safeReserve(1);
        if ((@intFromPtr(sp) & 0xffff) == 0) {
            sp = sp.safeReserve(1);
        }
        sp.top = Object.from(self_value, sp, context);
        process.setSp(sp);
        return runCompiled(&method, &compiled, &process, info.positions[0..], sp);
    }

    pub fn run() !void {
        const result = runWith(55);
        try reportResult("Result", result, 55);
    }
};

pub const InlinePrimitiveAddTest = struct {
    const plus = SmallInteger.@"+".inlined;
    const tup = .{
        tf.pushLiteral,
        "0const",
        tf.pushLiteral,
        "1const",
        tf.inlinePrimitive,
        plus,
        tf.returnTop,
    };
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops);

    var method: Method = undefined;
    var process: Process align(Process.alignment) = undefined;
    var compiled align(64) = compileMethod(Sym.value, 0, .Object, tup);
    var first_: Object.StaticObject = undefined;
    var second_: Object.StaticObject = undefined;

    pub fn init() !void {
        try initJitTest(&method, &process, "InlinePrimitiveAddTest: pushLiteral -> pushLiteral -> + -> returnTop");
        const first = first_.init(40);
        const second = second_.init(2);
        compiled.resolve(&[_]Object{ first, second }) catch @panic("Failed to resolve");
    }

    pub fn deinit() void {
        method.deinit();
    }

    pub fn runWith(a: i56, b: i56) i64 {
        const context = process.getContext();
        const sp = process.endOfStack();
        const code = compiled.code[0..];
        setLiteral(code, 1, Object.from(a, sp, context));
        setLiteral(code, 3, Object.from(b, sp, context));
        return runCompiled(&method, &compiled, &process, info.positions[0..], null);
    }

    pub fn run() !void {
        const result = runWith(40, 2);
        try reportResult("Result", result, 42);
    }
};
