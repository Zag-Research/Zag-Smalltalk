const std = @import("std");
const builtin = @import("builtin");
const zag = @import("zag");
const threadedFn = zag.threadedFn;
const tf = threadedFn.Enum;
const Process = zag.Process;
const Context = zag.Context;
const Extra = Context.Extra;
const Object = zag.Object;
const Code = zag.execute.Code;
const PC = zag.execute.PC;
const Signature = zag.execute.Signature;
const compileMethod = zag.execute.compileMethod;
const dispatch = zag.dispatch;
const Sym = zag.symbol.symbols;
const SmallInteger = zag.primitives.primitives.SmallInteger;
const symbol = zag.symbol;

const JitMethod = @import("jit_method.zig").JitMethod;
const harness = @import("test_harness.zig");
const initJitTest = harness.initJitTest;
const runCompiled = harness.runCompiled;
const setLiteral = harness.setLiteral;
const reportResult = harness.reportResult;
const opsInfo = harness.opsInfo;

pub const PushTest = struct {
    const self = Context.makeVariable(0, 1, .Parameter, &.{});
    const tup = .{ tf.push, self, tf.returnTop };
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops, &info.branch_targets, &info.prim_fns);

    var method: Method = undefined;
    var process: Process align(Process.alignment) = undefined;
    var compiled align(64) = compileMethod(Sym.value, 0, .Object, tup);

    pub fn init() !void {
        try initJitTest(&method, &process, "PushTest");
    }

    pub fn deinit() void {
        method.deinit();
    }

    pub fn run() !void {
        process.init();
        const context = process.getContext();
        var sp = process.endOfStack().safeReserve(1);
        sp.top = Object.from(123, sp, context);
        process.setSp(sp);
        const result = runCompiled(&method, &compiled, &process, info.positions[0..], sp);
        try reportResult(result, 123);
    }
};

pub const PushLiteralTest = struct {
    const tup = .{ tf.pushLiteral, "0const", tf.returnTop };
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops, &info.branch_targets, &info.prim_fns);

    var method: Method = undefined;
    var process: Process align(Process.alignment) = undefined;
    var compiled align(64) = compileMethod(Sym.value, 0, .Object, tup);
    var literal_: Object.StaticObject = undefined;

    pub fn init() !void {
        try initJitTest(&method, &process, "PushLiteralTest");
        compiled.resolve(&[_]Object{literal_.init(0)}) catch @panic("Failed to resolve");
    }

    pub fn deinit() void {
        method.deinit();
    }

    pub fn run() !void {
        const context = process.getContext();
        const sp = process.endOfStack();
        setLiteral(compiled.code[0..], 1, Object.from(7, sp, context));
        const result = runCompiled(&method, &compiled, &process, info.positions[0..], null);
        try reportResult(result, 7);
    }
};

pub const TailCallPatchTest = struct {
    const tup = .{
        tf.pushLiteral, "0const",
        tf.dup,         tf.drop,
        tf.pushLiteral, "1const",
        tf.drop,        tf.pushLiteral,
        "2const",       tf.returnTop,
    };
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops, &info.branch_targets, &info.prim_fns);

    var method: Method = undefined;
    var process: Process align(Process.alignment) = undefined;
    var compiled align(64) = compileMethod(Sym.value, 0, .Object, tup);
    var literals_: [3]Object.StaticObject = undefined;

    pub fn init() !void {
        try initJitTest(&method, &process, "TailCallPatchTest");
        compiled.resolve(&[_]Object{
            literals_[0].init(0),
            literals_[1].init(0),
            literals_[2].init(0),
        }) catch @panic("Failed to resolve");
    }

    pub fn deinit() void {
        method.deinit();
    }

    pub fn run() !void {
        const context = process.getContext();
        const sp = process.endOfStack();
        const code = compiled.code[0..];
        setLiteral(code, 1, Object.from(11, sp, context));
        setLiteral(code, 5, Object.from(22, sp, context));
        setLiteral(code, 8, Object.from(33, sp, context));
        const result = runCompiled(&method, &compiled, &process, info.positions[0..], null);
        try reportResult(result, 33);
    }
};

pub const ReturnSelfTest = struct {
    const tup = .{tf.returnSelf};
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops, &info.branch_targets, &info.prim_fns);

    var method: Method = undefined;
    var process: Process align(Process.alignment) = undefined;
    var compiled align(64) = compileMethod(Sym.value, 0, .Object, tup);

    pub fn init() !void {
        try initJitTest(&method, &process, "ReturnSelfTest");
    }

    pub fn deinit() void {
        method.deinit();
    }

    pub fn run() !void {
        process.init();
        const context = process.getContext();
        var sp = process.endOfStack().safeReserve(1);
        sp.top = Object.from(55, sp, context);
        process.setSp(sp);
        const result = runCompiled(&method, &compiled, &process, info.positions[0..], sp);
        try reportResult(result, 55);
    }
};

pub const BranchFalseTest = struct {
    const Boolean = zag.primitives.primitives.Boolean;
    const tup = .{
        tf.pushLiteral,  "0const",    // push test value (True or False)
        tf.branchFalse,  "taken",     // if False, jump to taken
        tf.pushLiteral,  "1const",    // not-taken: push 111
        tf.returnTop,    ":taken",    // label definition
        tf.pushLiteral,  "2const",    // taken: push 222
        tf.returnTop,
    };
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops, &info.branch_targets, &info.prim_fns);

    var method: Method = undefined;
    var process: Process align(Process.alignment) = undefined;
    var compiled align(64) = compileMethod(Sym.value, 0, .Object, tup);
    var literals_: [3]Object.StaticObject = undefined;

    pub fn init() !void {
        try initJitTest(&method, &process, "BranchFalseTest");
        compiled.resolve(&[_]Object{
            literals_[0].init(0),
            literals_[1].init(0),
            literals_[2].init(0),
        }) catch @panic("Failed to resolve");
    }

    pub fn deinit() void {
        method.deinit();
    }

    pub fn runWithValue(value: Object, expected: i64) !void {
        process.init();
        const sp = process.endOfStack();
        const code = compiled.code[0..];
        setLiteral(code, 1, value);
        setLiteral(code, 5, Object.from(111, sp, process.getContext()));
        setLiteral(code, 8, Object.from(222, sp, process.getContext()));
        
        const result = runCompiled(&method, &compiled, &process, info.positions[0..], null);
        try reportResult(result, expected);
    }

    pub fn run() !void {
        std.debug.print("  Test with False:\n", .{});
        try runWithValue(Object.False(), 222);

        std.debug.print("  Test with True:\n", .{});
        try runWithValue(Object.True(), 111);
    }
};

pub const InlinePrimitiveAddTest = struct {
    const plus = SmallInteger.@"+".inlined;
    const tup = .{
        tf.pushLiteral,     "0const",
        tf.pushLiteral,     "1const",
        tf.inlinePrimitive, plus,
        tf.returnTop,
    };
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops, &info.branch_targets, &info.prim_fns);

    var method: Method = undefined;
    var process: Process align(Process.alignment) = undefined;
    var compiled align(64) = compileMethod(Sym.value, 0, .Object, tup);
    var literals_: [2]Object.StaticObject = undefined;

    pub fn init() !void {
        try initJitTest(&method, &process, "InlinePrimitiveAddTest");
        compiled.resolve(&[_]Object{
            literals_[0].init(0),
            literals_[1].init(0),
        }) catch @panic("Failed to resolve");
    }

    pub fn deinit() void {
        method.deinit();
    }

    pub fn run() !void {
        const context = process.getContext();
        const sp = process.endOfStack();
        const code = compiled.code[0..];
        setLiteral(code, 1, Object.from(40, sp, context));
        setLiteral(code, 3, Object.from(2, sp, context));
        const result = runCompiled(&method, &compiled, &process, info.positions[0..], null);
        try reportResult(result, 42);
    }
};

pub const Send0Test = struct {
    const callee_sig = Signature.fromNameClass(Sym.yourself, .SmallInteger);
    var callee align(64) = compileMethod(Sym.yourself, 0, .SmallInteger, .{tf.returnSelf});

    const send_sig = Signature.fromNameClass(Sym.yourself, .none);
    const tup = .{
        tf.pushLiteral, "0const",
        tf.send0,       send_sig,
        null,           tf.returnTop,
    };
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops, &info.branch_targets, &info.prim_fns);

    var method: Method = undefined;
    var process: Process align(Process.alignment) = undefined;
    var compiled align(64) = compileMethod(Sym.value, 0, .Object, tup);
    var literal_: Object.StaticObject = undefined;

    pub fn init() !void {
        dispatch.addMethod(callee.asCompiledMethodPtr());

        try initJitTest(&method, &process, "Send0Test");
        compiled.resolve(&[_]Object{literal_.init(0)}) catch @panic("Failed to resolve");
    }

    pub fn deinit() void {
        method.deinit();
    }

    pub fn run() !void {
        process.init();
        const context = process.getContext();
        const sp = process.endOfStack();
        const code = compiled.code[0..];
        setLiteral(code, 1, Object.from(10, sp, context));

        const result = runCompiled(&method, &compiled, &process, info.positions[0..], null);
        try reportResult(result, 10); 
    }
};

pub const SendTest = struct {
    var callee align(64) = compileMethod(Sym.@"value:", 0, .SmallInteger, .{tf.returnTop});

    const send_sig = Signature.fromNameClass(Sym.@"value:", .none);
    const tup = .{
        tf.pushLiteral, "0const", 
        tf.pushLiteral, "1const", 
        tf.send,        send_sig,
        null,           tf.returnTop,
    };
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops, &info.branch_targets, &info.prim_fns);

    var method: Method = undefined;
    var process: Process align(Process.alignment) = undefined;
    var compiled align(64) = compileMethod(Sym.value, 0, .Object, tup);
    var literals_: [2]Object.StaticObject = undefined;

    pub fn init() !void {
        dispatch.addMethod(callee.asCompiledMethodPtr());

        try initJitTest(&method, &process, "SendTest");
        compiled.resolve(&[_]Object{
            literals_[0].init(0),
            literals_[1].init(0),
        }) catch @panic("Failed to resolve");
    }

    pub fn deinit() void {
        method.deinit();
    }

    pub fn run() !void {
        process.init();
        const context = process.getContext();
        const sp = process.endOfStack();
        const code = compiled.code[0..];
        setLiteral(code, 1, Object.from(10, sp, context));
        setLiteral(code, 3, Object.from(99, sp, context));
        const result = runCompiled(&method, &compiled, &process, info.positions[0..], null);
        try reportResult(result, 99);
    }
};

pub const InlinePrimitiveChainedTest = struct {
    const plus = SmallInteger.@"+".inlined;
    const minus = SmallInteger.@"-".inlined;
    const tup = .{
        tf.pushLiteral,     "0const",  // push a
        tf.pushLiteral,     "1const",  // push b
        tf.inlinePrimitive, plus,      // a + b
        tf.pushLiteral,     "2const",  // push c
        tf.inlinePrimitive, minus,     // (a + b) - c
        tf.returnTop,
    };
    const info = opsInfo(tup);
    const Method = JitMethod(&info.ops, &info.branch_targets, &info.prim_fns);

    var method: Method = undefined;
    var process: Process align(Process.alignment) = undefined;
    var compiled align(64) = compileMethod(Sym.value, 0, .Object, tup);
    var literals_: [3]Object.StaticObject = undefined;

    pub fn init() !void {
        try initJitTest(&method, &process, "InlinePrimitiveChainedTest");
        compiled.resolve(&[_]Object{
            literals_[0].init(0),
            literals_[1].init(0),
            literals_[2].init(0),
        }) catch @panic("Failed to resolve");
    }

    pub fn deinit() void {
        method.deinit();
    }

    pub fn run() !void {
        const context = process.getContext();
        const sp = process.endOfStack();
        const code = compiled.code[0..];
        setLiteral(code, 1, Object.from(40, sp, context));   // a = 40 at code[1]
        setLiteral(code, 3, Object.from(50, sp, context));   // b = 50 at code[3]
        setLiteral(code, 7, Object.from(48, sp, context));   // c = 48 at code[7]
        const result = runCompiled(&method, &compiled, &process, info.positions[0..], null);
        try reportResult(result, 42);  // 40 + 50 - 48 = 42
    }
};
