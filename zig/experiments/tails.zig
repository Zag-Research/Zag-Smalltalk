const std = @import("std");
const debug = std.debug;
const math = std.math;
const tailCall: std.builtin.CallModifier = .always_tail;
const stdout = std.io.getStdOut().writer();

fn loop1(cur: u64, limit: u64) void {
    if (cur % 1000000 == 0) stdout.print("cur: {}\n", .{cur}) catch unreachable;
    return @call(tailCall, loop2, .{ cur, limit });
}
fn loop2(cur: u64, limit: u64) void {
    if (cur == limit) return;
    //    return loop1(cur+1,limit);
    return @call(tailCall, loop1, .{ cur + 1, limit });
}
test "looping" {
    loop1(0, 10000000);
}

const checkCount = 1000;
const ContextPtr = u64;
const Thread = struct {
    doCall: bool,
    count: i64,
    loops: usize,
    junk: i64,
    fn init() Thread {
        return .{ .doCall = false, .count = checkCount, .loops = 0, .junk = undefined };
    }
    fn checkP(pc: [*]const P.Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        thread.doCall = false;
        thread.count = checkCount;
        thread.loops += 1;
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, sp, hp, thread, context });
    }
    fn checkQ(pc: [*]const Q.Code, sp: [*]i64, hp: i64, _: i64, thread: *Thread, context: ContextPtr) void {
        thread.loops += 1;
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, sp, hp, checkCount, thread, context });
    }
    fn checkR(pc: [*]const R.Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        thread.loops += 1;
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, sp, hp, thread, context });
    }
    fn checkN(thread: *Thread) void {
        thread.loops += 1;
    }
};
const P = struct {
    const PrimitivePtr = *const fn (programCounter: [*]const Code, stackPointer: [*]i64, heapPointer: i64, thread: *Thread, context: ContextPtr) void;
    pub const Code = extern union {
        prim: PrimitivePtr,
        int: i64,
        fn prim(pp: PrimitivePtr) Code {
            return Code{ .prim = pp };
        }
        fn int(i: i64) Code {
            return Code{ .int = i };
        }
    };
    const p = Code.prim;
    const c = Code.int;
    fn run(n: i64) usize {
        const pc = @as([*]const Code, @ptrCast(&prog[0]));
        var stack = [_]i64{ 0, 0, n, 0, 0 };
        var thread = Thread.init();
        pc[0].prim.*(pc + 1, @as([*]i64, @ptrCast(&stack[0])) + 2, 0, &thread, 0);
        //        stdout.print("P.run() {any} loops: {}\n",.{stack,thread.loops}) catch @panic("print failed");
        return thread.loops;
    }
    pub fn branch(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const offset = pc[0].int;
        if (offset == -1) {
            //const target = context.getPc();
            @panic("loop to self");
        }
        //        stdout.print("branch offset: {}  tos: {}\n",.{offset,sp[0]}) catch @panic("print failed");
        if (offset >= 0) {
            const target = pc + 1 + @as(u64, @intCast(offset));
            //            stdout.print("branch target: {}\n",.{(@ptrToInt(target)-@ptrToInt(&prog[0]))/@sizeOf(Code)}) catch @panic("print failed");
            if (thread.doCall) return @call(tailCall, Thread.checkP, .{ target, sp, hp, thread, context });
            return @call(tailCall, target[0].prim.*, .{ target + 1, sp, hp, thread, context });
        } else {
            const target = pc + 1 - @as(u64, @intCast(-offset));
            //            stdout.print("branch target: {}\n",.{(@ptrToInt(target)-@ptrToInt(&prog[0]))/@sizeOf(Code)}) catch @panic("print failed");
            return @call(tailCall, Thread.checkP, .{ target, sp, hp, thread, context });
        }
    }
    pub fn ifFalse(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const v = sp[0];
        //        stdout.print("ifFalse tos: {}\n",.{v}) catch @panic("print failed");
        if (thread.doCall) return @call(tailCall, Thread.checkP, .{ pc, sp, hp, thread, context });
        if (v == 0) return @call(tailCall, branch, .{ pc, sp + 1, hp, thread, context });
        if (v == 1) return @call(tailCall, pc[1].prim.*, .{ pc + 2, sp + 1, hp, thread, context });
        @panic("non boolean");
    }
    pub fn result(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        _ = pc;
        _ = sp;
        _ = hp;
        _ = thread;
        _ = context;
        return;
    }
    pub fn nop(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        //        stdout.print("nop tos: {}\n",.{sp[0]}) catch @panic("print failed");
        if (thread.doCall) return @call(tailCall, Thread.checkP, .{ pc, sp, hp, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, sp, hp, thread, context });
    }
    pub fn swap(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const top = sp[0];
        sp[0] = sp[1];
        sp[1] = top;
        //        stdout.print("swap tos: {}\n",.{sp[0]}) catch @panic("print failed");
        if (thread.doCall) return @call(tailCall, Thread.checkP, .{ pc, sp, hp, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, sp, hp, thread, context });
    }
    pub fn rot(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const n3 = sp[0];
        sp[0] = sp[1];
        sp[1] = sp[2];
        sp[2] = n3;
        //        stdout.print("rot tos: {}\n",.{sp[0]}) catch @panic("print failed");
        if (thread.doCall) return @call(tailCall, Thread.checkP, .{ pc, sp, hp, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, sp, hp, thread, context });
    }
    pub fn drop(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp + 1;
        //        stdout.print("drop tos: {}\n",.{sp[0]}) catch @panic("print failed");
        if (thread.doCall) return @call(tailCall, Thread.checkP, .{ pc, newSp, hp, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, newSp, hp, thread, context });
    }
    fn pushLiteral(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp - 1;
        newSp[0] = pc[0].int;
        //        stdout.print("pushLiteral tos: {}\n",.{newSp[0]}) catch @panic("print failed");
        if (thread.doCall) return @call(tailCall, Thread.checkP, .{ pc + 1, newSp, hp, thread, context });
        return @call(tailCall, pc[1].prim.*, .{ pc + 2, newSp, hp, thread, context });
    }
    pub fn add(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp + 1;
        //        stdout.print("add {}+{}\n",.{newSp[0],sp[0]}) catch @panic("print failed");
        newSp[0] +%= sp[0];
        if (thread.doCall) return @call(tailCall, Thread.checkP, .{ pc, newSp, hp, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, newSp, hp, thread, context });
    }
    pub fn sub(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp + 1;
        //        stdout.print("sub {}-{}\n",.{newSp[0],sp[0]}) catch @panic("print failed");
        newSp[0] -%= sp[0];
        if (thread.doCall) return @call(tailCall, Thread.checkP, .{ pc, newSp, hp, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, newSp, hp, thread, context });
    }
    pub fn mul(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp + 1;
        //        stdout.print("mul {}*{}\n",.{newSp[0],sp[0]}) catch @panic("print failed");
        newSp[0] *%= sp[0];
        if (thread.doCall) return @call(tailCall, Thread.checkP, .{ pc, newSp, hp, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, newSp, hp, thread, context });
    }
    pub fn dup(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp - 1;
        //        stdout.print("dup tos: {}\n",.{sp[0]}) catch @panic("print failed");
        newSp[0] = sp[0];
        if (thread.doCall) return @call(tailCall, Thread.checkP, .{ pc, newSp, hp, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, newSp, hp, thread, context });
    }
    pub fn greater(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp + 1;
        //        stdout.print("greater sp+1: {}  tos: {}\n",.{newSp[0],sp[0]}) catch @panic("print failed");
        newSp[0] = (if (newSp[0] > sp[0]) 1 else 0);
        if (thread.doCall) return @call(tailCall, Thread.checkP, .{ pc, newSp, hp, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, newSp, hp, thread, context });
    }
    pub fn even(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        //        stdout.print("even tos: {}\n",.{sp[0]}) catch @panic("print failed");
        sp[0] = if (sp[0] & 1 == 0) 1 else 0;
        if (thread.doCall) return @call(tailCall, Thread.checkP, .{ pc, sp, hp, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, sp, hp, thread, context });
    }
    const prog = [_]Code{
        //        p(&pushLiteral),c(0),
        //        p(&pushLiteral),c(0),
        //        p(&pushLiteral),c(128), // 128 give a huge negative number, anything bigger gives 2

        p(&nop),
        p(&nop),
        p(&nop),
        // "label2:",
        p(&pushLiteral),
        c(1),
        p(&sub),
        p(&dup),
        p(&pushLiteral),
        c(0),
        p(&greater),
        p(&ifFalse), c(24), // "label7",

        // "label3:",
        p(&dup),     p(&even),
        p(&ifFalse),     c(10), // "label5",

        p(&swap),        p(&pushLiteral),
        c(2),            p(&mul),
        p(&pushLiteral), c(3),
        p(&sub),         p(&swap),
        p(&branch),      c(8), // "label6",

        // "label5:",
        p(&swap),        p(&pushLiteral),
        c(3),            p(&mul),
        p(&pushLiteral), c(1),
        p(&sub),
        p(&swap),

        // "label6:",
        // p(&rot),
        // p(&rot),
        // p(&pushLiteral),c(1),
        // p(&add),
        // p(&rot),
        p(&branch), c(-33), // "label2",
        // "label7:",
        p(&result),
    };
};
test "p" {
    const expectEqual = std.testing.expectEqual;
    const runs = 10000000;
    try expectEqual(P.run(runs), runs - 1);
}
const Q = struct {
    const PrimitivePtr = *const fn (programCounter: [*]const Code, stackPointer: [*]i64, heapPointer: i64, flag: i64, thread: *Thread, context: ContextPtr) void;
    pub const Code = packed union {
        prim: PrimitivePtr,
        int: i64,
        fn prim(pp: PrimitivePtr) Code {
            return Code{ .prim = pp };
        }
        fn int(i: i64) Code {
            return Code{ .int = i };
        }
    };
    const p = Code.prim;
    const c = Code.int;
    fn run(n: i64) usize {
        dumpStackPointerAddr("run");
        const pc = @as([*]const Code, @ptrCast(&prog[0]));
        var stack = [_]i64{ 0, 0, n, 0, 0 };
        var thread = Thread.init();
        pc[0].prim.*(pc + 1, @as([*]i64, @ptrCast(&stack[0])) + 2, 0, checkCount, &thread, 0);
        //        stdout.print("Q.run() {any} loops: {}\n",.{stack,thread.loops}) catch @panic("print failed");
        return thread.loops;
    }
    pub fn branch(pc: [*]const Code, sp: [*]i64, hp: i64, flag: i64, thread: *Thread, context: ContextPtr) void {
        dumpStackPointerAddr("branch");
        const offset = pc[0].int;
        if (offset == -1) {
            //const target = context.getPc();
            @panic("loop to self");
        }
        //        stdout.print("branch offset: {}  tos: {}\n",.{offset,sp[0]}) catch @panic("print failed");
        if (offset >= 0) {
            const target = pc + 1 + @as(u64, @intCast(offset));
            //            stdout.print("branch target: {}\n",.{(@ptrToInt(target)-@ptrToInt(&prog[0]))/@sizeOf(Code)}) catch @panic("print failed");
            if (flag <= 0) return @call(tailCall, Thread.checkQ, .{ target, sp, hp, flag, thread, context });
            return @call(tailCall, target[0].prim.*, .{ target + 1, sp, hp, flag, thread, context });
        } else {
            const target = pc + 1 - @as(u64, @intCast(-offset));
            //            stdout.print("branch target: {}\n",.{(@ptrToInt(target)-@ptrToInt(&prog[0]))/@sizeOf(Code)}) catch @panic("print failed");
            return @call(tailCall, Thread.checkQ, .{ target, sp, hp, flag, thread, context });
        }
    }
    pub fn ifFalse(pc: [*]const Code, sp: [*]i64, hp: i64, flag: i64, thread: *Thread, context: ContextPtr) void {
        const v = sp[0];
        //        stdout.print("ifFalse tos: {}\n",.{v}) catch @panic("print failed");
        if (flag <= 0) return @call(tailCall, Thread.checkQ, .{ pc, sp, hp, flag, thread, context });
        if (v == 0) return @call(tailCall, branch, .{ pc, sp + 1, hp, flag, thread, context });
        if (v == 1) return @call(tailCall, pc[1].prim.*, .{ pc + 2, sp + 1, hp, flag, thread, context });
        @panic("non boolean");
    }
    pub fn result(pc: [*]const Code, sp: [*]i64, hp: i64, flag: i64, thread: *Thread, context: ContextPtr) void {
        _ = pc;
        _ = sp;
        _ = hp;
        _ = flag;
        _ = thread;
        _ = context;
        return;
    }
    pub fn nop(pc: [*]const Code, sp: [*]i64, hp: i64, flag: i64, thread: *Thread, context: ContextPtr) void {
        dumpStackPointerAddr("nop");
        //        stdout.print("nop tos: {}\n",.{sp[0]}) catch @panic("print failed");
        if (flag <= 0) return @call(tailCall, Thread.checkQ, .{ pc, sp, hp, flag, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, sp, hp, flag, thread, context });
    }
    pub fn swap(pc: [*]const Code, sp: [*]i64, hp: i64, flag: i64, thread: *Thread, context: ContextPtr) void {
        const top = sp[0];
        sp[0] = sp[1];
        sp[1] = top;
        //        stdout.print("swap tos: {}\n",.{sp[0]}) catch @panic("print failed");
        if (flag <= 0) return @call(tailCall, Thread.checkQ, .{ pc, sp, hp, flag, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, sp, hp, flag, thread, context });
    }
    pub fn rot(pc: [*]const Code, sp: [*]i64, hp: i64, flag: i64, thread: *Thread, context: ContextPtr) void {
        const n3 = sp[0];
        sp[0] = sp[1];
        sp[1] = sp[2];
        sp[2] = n3;
        //        stdout.print("rot tos: {}\n",.{sp[0]}) catch @panic("print failed");
        if (flag <= 0) return @call(tailCall, Thread.checkQ, .{ pc, sp, hp, flag, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, sp, hp, flag, thread, context });
    }
    pub fn drop(pc: [*]const Code, sp: [*]i64, hp: i64, flag: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp + 1;
        //        stdout.print("drop tos: {}\n",.{sp[0]}) catch @panic("print failed");
        if (flag <= 0) return @call(tailCall, Thread.checkQ, .{ pc, newSp, hp, flag, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, newSp, hp, flag, thread, context });
    }
    fn pushLiteral(pc: [*]const Code, sp: [*]i64, hp: i64, flag: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp - 1;
        newSp[0] = pc[0].int;
        //        stdout.print("pushLiteral tos: {}\n",.{newSp[0]}) catch @panic("print failed");
        if (flag <= 0) return @call(tailCall, Thread.checkQ, .{ pc + 1, newSp, hp, flag, thread, context });
        return @call(tailCall, pc[1].prim.*, .{ pc + 2, newSp, hp, flag, thread, context });
    }
    pub fn add(pc: [*]const Code, sp: [*]i64, hp: i64, flag: i64, thread: *Thread, context: ContextPtr) void {
        dumpStackPointerAddr("add");
        const newSp = sp + 1;
        //        stdout.print("add {}+{}\n",.{newSp[0],sp[0]}) catch @panic("print failed");
        newSp[0] +%= sp[0];
        if (flag <= 0) return @call(tailCall, Thread.checkQ, .{ pc, newSp, hp, flag, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, newSp, hp, flag, thread, context });
    }
    pub fn sub(pc: [*]const Code, sp: [*]i64, hp: i64, flag: i64, thread: *Thread, context: ContextPtr) void {
        dumpStackPointerAddr("sub");
        const newSp = sp + 1;
        //        stdout.print("sub {}-{}\n",.{newSp[0],sp[0]}) catch @panic("print failed");
        newSp[0] -%= sp[0];
        if (flag <= 0) return @call(tailCall, Thread.checkQ, .{ pc, newSp, hp, flag, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, newSp, hp, flag, thread, context });
    }
    pub fn mul(pc: [*]const Code, sp: [*]i64, hp: i64, flag: i64, thread: *Thread, context: ContextPtr) void {
        dumpStackPointerAddr("mul");
        const newSp = sp + 1;
        //        stdout.print("mul {}*{}\n",.{newSp[0],sp[0]}) catch @panic("print failed");
        newSp[0] *%= sp[0];
        if (flag <= 0) return @call(tailCall, Thread.checkQ, .{ pc, newSp, hp, flag, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, newSp, hp, flag, thread, context });
    }
    pub fn dup(pc: [*]const Code, sp: [*]i64, hp: i64, flag: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp - 1;
        //        stdout.print("dup tos: {}\n",.{sp[0]}) catch @panic("print failed");
        newSp[0] = sp[0];
        if (flag <= 0) return @call(tailCall, Thread.checkQ, .{ pc, newSp, hp, flag, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, newSp, hp, flag, thread, context });
    }
    pub fn greater(pc: [*]const Code, sp: [*]i64, hp: i64, flag: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp + 1;
        //        stdout.print("greater sp+1: {}  tos: {}\n",.{newSp[0],sp[0]}) catch @panic("print failed");
        newSp[0] = (if (newSp[0] > sp[0]) 1 else 0);
        if (flag <= 0) return @call(tailCall, Thread.checkQ, .{ pc, newSp, hp, flag, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, newSp, hp, flag, thread, context });
    }
    pub fn even(pc: [*]const Code, sp: [*]i64, hp: i64, flag: i64, thread: *Thread, context: ContextPtr) void {
        //        stdout.print("even tos: {}\n",.{sp[0]}) catch @panic("print failed");
        sp[0] = if (sp[0] & 1 == 0) 1 else 0;
        if (flag <= 0) return @call(tailCall, Thread.checkQ, .{ pc, sp, hp, flag, thread, context });
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, sp, hp, flag, thread, context });
    }
    const prog = [_]Code{
        //        p(&pushLiteral),c(0),
        //        p(&pushLiteral),c(0),
        //        p(&pushLiteral),c(128), // 128 give a huge negative number, anything bigger gives 2

        p(&nop),
        p(&nop),
        p(&nop),
        // "label2:",
        p(&pushLiteral),
        c(1),
        p(&sub),
        p(&dup),
        p(&pushLiteral),
        c(0),
        p(&greater),
        p(&ifFalse), c(24), // "label7",

        // "label3:",
        p(&dup),     p(&even),
        p(&ifFalse),     c(10), // "label5",

        p(&swap),        p(&pushLiteral),
        c(2),            p(&mul),
        p(&pushLiteral), c(3),
        p(&sub),         p(&swap),
        p(&branch),      c(8), // "label6",

        // "label5:",
        p(&swap),        p(&pushLiteral),
        c(3),            p(&mul),
        p(&pushLiteral), c(1),
        p(&sub),
        p(&swap),

        // "label6:",
        // p(&rot),
        // p(&rot),
        // p(&pushLiteral),c(1),
        // p(&add),
        // p(&rot),
        p(&branch), c(-33), // "label2",
        // "label7:",
        p(&nop),    p(&result),
    };
};
test "q" {
    const expectEqual = std.testing.expectEqual;
    const runs = 10000000;
    try expectEqual(Q.run(runs), runs - 1);
}
const R = struct {
    const PrimitivePtr = *const fn (programCounter: [*]const Code, stackPointer: [*]i64, heapPointer: i64, thread: *Thread, context: ContextPtr) void;
    pub const Code = extern union {
        prim: PrimitivePtr,
        int: i64,
        fn prim(pp: PrimitivePtr) Code {
            return Code{ .prim = pp };
        }
        fn int(i: i64) Code {
            return Code{ .int = i };
        }
    };
    const p = Code.prim;
    const c = Code.int;
    fn run(n: i64) usize {
        const pc = @as([*]const Code, @ptrCast(&prog[0]));
        var stack = [_]i64{ 0, 0, n, 0, 0 };
        var thread = Thread.init();
        pc[0].prim.*(pc + 1, @as([*]i64, @ptrCast(&stack[0])) + 2, 0, &thread, 0);
        //        stdout.print("R.run() {any} loops: {}\n",.{stack,thread.loops}) catch @panic("print failed");
        return thread.loops;
    }
    pub fn branch(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const offset = pc[0].int;
        if (offset == -1) {
            //const target = context.getPc();
            @panic("loop to self");
        }
        //        stdout.print("branch offset: {}  tos: {}\n",.{offset,sp[0]}) catch @panic("print failed");
        if (offset >= 0) {
            const target = pc + 1 + @as(u64, @intCast(offset));
            //            stdout.print("branch target: {}\n",.{(@ptrToInt(target)-@ptrToInt(&prog[0]))/@sizeOf(Code)}) catch @panic("print failed");
            return @call(tailCall, target[0].prim.*, .{ target + 1, sp, hp, thread, context });
        } else {
            const target = pc + 1 - @as(u64, @intCast(-offset));
            //            stdout.print("branch target: {}\n",.{(@ptrToInt(target)-@ptrToInt(&prog[0]))/@sizeOf(Code)}) catch @panic("print failed");
            return @call(tailCall, Thread.checkR, .{ target, sp, hp, thread, context });
        }
    }
    pub fn ifFalse(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const v = sp[0];
        //        stdout.print("ifFalse tos: {}\n",.{v}) catch @panic("print failed");
        if (v == 0) return @call(tailCall, branch, .{ pc, sp + 1, hp, thread, context });
        if (v == 1) return @call(tailCall, pc[1].prim.*, .{ pc + 2, sp + 1, hp, thread, context });
        @panic("non boolean");
    }
    pub fn result(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        _ = pc;
        _ = sp;
        _ = hp;
        _ = thread;
        _ = context;
        return;
    }
    pub fn nop(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        //        stdout.print("nop tos: {}\n",.{sp[0]}) catch @panic("print failed");
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, sp, hp, thread, context });
    }
    pub fn swap(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const top = sp[0];
        sp[0] = sp[1];
        sp[1] = top;
        //        stdout.print("swap tos: {}\n",.{sp[0]}) catch @panic("print failed");
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, sp, hp, thread, context });
    }
    pub fn rot(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const n3 = sp[0];
        sp[0] = sp[1];
        sp[1] = sp[2];
        sp[2] = n3;
        //        stdout.print("rot tos: {}\n",.{sp[0]}) catch @panic("print failed");
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, sp, hp, thread, context });
    }
    pub fn drop(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp + 1;
        //        stdout.print("drop tos: {}\n",.{sp[0]}) catch @panic("print failed");
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, newSp, hp, thread, context });
    }
    fn pushLiteral(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp - 1;
        newSp[0] = pc[0].int;
        //        stdout.print("pushLiteral tos: {}\n",.{newSp[0]}) catch @panic("print failed");
        return @call(tailCall, pc[1].prim.*, .{ pc + 2, newSp, hp, thread, context });
    }
    pub fn add(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp + 1;
        //        stdout.print("add {}+{}\n",.{newSp[0],sp[0]}) catch @panic("print failed");
        newSp[0] +%= sp[0];
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, newSp, hp, thread, context });
    }
    pub fn sub(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp + 1;
        //        stdout.print("sub {}-{}\n",.{newSp[0],sp[0]}) catch @panic("print failed");
        newSp[0] -%= sp[0];
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, newSp, hp, thread, context });
    }
    pub fn mul(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp + 1;
        //        stdout.print("mul {}*{}\n",.{newSp[0],sp[0]}) catch @panic("print failed");
        newSp[0] *%= sp[0];
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, newSp, hp, thread, context });
    }
    pub fn dup(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp - 1;
        //        stdout.print("dup tos: {}\n",.{sp[0]}) catch @panic("print failed");
        newSp[0] = sp[0];
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, newSp, hp, thread, context });
    }
    pub fn greater(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp + 1;
        //        stdout.print("greater sp+1: {}  tos: {}\n",.{newSp[0],sp[0]}) catch @panic("print failed");
        newSp[0] = (if (newSp[0] > sp[0]) 1 else 0);
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, newSp, hp, thread, context });
    }
    pub fn even(pc: [*]const Code, sp: [*]i64, hp: i64, thread: *Thread, context: ContextPtr) void {
        //        stdout.print("even tos: {}\n",.{sp[0]}) catch @panic("print failed");
        sp[0] = if (sp[0] & 1 == 0) 1 else 0;
        return @call(tailCall, pc[0].prim.*, .{ pc + 1, sp, hp, thread, context });
    }
    const prog = [_]Code{
        //        p(&pushLiteral),c(0),
        //        p(&pushLiteral),c(0),
        //        p(&pushLiteral),c(128), // 128 give a huge negative number, anything bigger gives 2

        p(&nop),
        p(&nop),
        p(&nop),
        // "label2:",
        p(&pushLiteral),
        c(1),
        p(&sub),
        p(&dup),
        p(&pushLiteral),
        c(0),
        p(&greater),
        p(&ifFalse), c(24), // "label7",

        // "label3:",
        p(&dup),     p(&even),
        p(&ifFalse),     c(10), // "label5",

        p(&swap),        p(&pushLiteral),
        c(2),            p(&mul),
        p(&pushLiteral), c(3),
        p(&sub),         p(&swap),
        p(&branch),      c(8), // "label6",

        // "label5:",
        p(&swap),        p(&pushLiteral),
        c(3),            p(&mul),
        p(&pushLiteral), c(1),
        p(&sub),
        p(&swap),

        // "label6:",
        // p(&rot),
        // p(&rot),
        // p(&pushLiteral),c(1),
        // p(&add),
        // p(&rot),
        p(&branch), c(-33), // "label2",
        // "label7:",
        p(&result),
    };
};
test "r" {
    const expectEqual = std.testing.expectEqual;
    const runs = 10000000;
    try expectEqual(R.run(runs), runs - 1);
}
const N = struct {
    fn doit(sp: usize, _: i64, thread: *Thread, _: u64) void {
        while (true) {
            @call(noInlineCall, st, .{ sp, stack[sp] -% 1 });
            if (stack[sp] <= 0) break;
            if (stack[sp] & 1 == 0) {
                @call(noInlineCall, st, .{ sp + 1, stack[sp + 1] *% 2 });
                @call(noInlineCall, st, .{ sp + 1, stack[sp + 1] -% 3 });
            } else {
                @call(noInlineCall, st, .{ sp + 1, stack[sp + 1] *% 3 });
                @call(noInlineCall, st, .{ sp + 1, stack[sp + 1] -% 1 });
            }
            @call(noInlineCall, Thread.checkN, .{thread});
        }
    }
    fn st(n: usize, v: i64) void {
        stack[n] = v;
    }
    var stack = [_]i64{0} ** 5;
    fn run(n: i64) usize {
        stack[0] = 0;
        stack[1] = 0;
        stack[2] = n;
        stack[3] = 0;
        stack[4] = 0;
        var thread = Thread.init();
        @call(noInlineCall, doit, .{ 2, 0, &thread, 0 });
        //        stdout.print("N.run() {any} loops: {}\n",.{stack,thread.loops}) catch @panic("print failed");
        return thread.loops;
    }
};
test "n" {
    const expectEqual = std.testing.expectEqual;
    const runs = 10000000;
    try expectEqual(N.run(runs), runs - 1);
}
pub fn timing(runs: isize) !void {
    const ts = std.time.nanoTimestamp;
    const warmup = 1_000_000;
    try stdout.print("for {} runs\n", .{runs});
    if (runs > warmup) _ = R.run(warmup);
    var start = ts();
    _ = R.run(runs);
    var base = ts() - start;
    try stdout.print("R: {d:8.3}s {d:8.3}ns\n", .{ @as(f64, @floatFromInt(base)) / 1000000000, @as(f64, @floatFromInt(base)) / @as(f64, @floatFromInt(runs)) });
    if (runs > warmup) _ = Q.run(warmup);
    start = ts();
    _ = Q.run(runs);
    var time = ts() - start;
    try stdout.print("Q: {d:8.3}s {d:8.3}ns +{d:6.2}%\n", .{ @as(f64, @floatFromInt(time)) / 1000000000, @as(f64, @floatFromInt(time)) / @as(f64, @floatFromInt(runs)), @as(f64, @floatFromInt(time - base)) * 100.0 / @as(f64, @floatFromInt(base)) });
    if (runs > warmup) _ = P.run(warmup);
    start = ts();
    _ = P.run(runs);
    time = ts() - start;
    try stdout.print("P: {d:8.3}s {d:8.3}ns +{d:6.2}%\n", .{ @as(f64, @floatFromInt(time)) / 1000000000, @as(f64, @floatFromInt(time)) / @as(f64, @floatFromInt(runs)), @as(f64, @floatFromInt(time - base)) * 100.0 / @as(f64, @floatFromInt(base)) });
    if (runs > warmup) _ = N.run(warmup);
    start = ts();
    _ = N.run(runs);
    time = ts() - start;
    try stdout.print("N: {d:8.3}s {d:8.3}ns  {d:6.2}%\n", .{ @as(f64, @floatFromInt(time)) / 1000000000, @as(f64, @floatFromInt(time)) / @as(f64, @floatFromInt(runs)), @as(f64, @floatFromInt(time)) * 100.0 / @as(f64, @floatFromInt(base)) });
}
fn getSP() void {
    const target = @import("builtin").target;
    switch (target.cpu.arch) {
        .x86_64 => asm volatile (
            \\  movq $11, %%rax
            \\  movq %[ptr], %%rbx
            \\  movq %[len], %%rcx
            \\  syscall
            \\  movq $60, %%rax
            \\  movq $1, %%rdi
            \\  syscall
            :: //[ptr] "r" (@ptrToInt(self.mapped.ptr)),
            //[len] "r" (self.mapped.len),
            : .{ .memory = true }),
        .aarch64, .aarch64_be, .aarch64_32 => asm volatile (
            \\  mov x8, #215
            \\  mov x0, %[ptr]
            \\  mov x1, %[len]
            \\  svc 0
            \\  mov x8, #93
            \\  mov x0, #0
            \\  svc 0
            :: //[ptr] "r" (@ptrToInt(self.mapped.ptr)),
            //[len] "r" (self.mapped.len),
            : .{ .memory = true }),
        else => |cpu_arch| @compileError("Unsupported arch: " ++ @tagName(cpu_arch)),
    }
}
pub fn dumpStackPointerAddr(prefix: []const u8) void {
    const sp = asm (""
        : [argc] "={sp}" (-> usize),
    );
    const lr = asm (""
        : [argc] "={lr}" (-> usize),
    );
    const spp = asm (""
        : [argc] "={sp}" (-> [*]usize),
    );
    const sps: []usize = spp[0..14];
    std.log.err("{s} lr = 0x{x} sp = 0x{x} {any}\n", .{ prefix, lr, sp, sps });
}
pub fn main() !void {
    dumpStackPointerAddr("main");
    try timing(4);
}
