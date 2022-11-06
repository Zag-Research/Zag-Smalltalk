const std = @import("std");
const debug = std.debug;
const math = std.math;
const tailCall: std.builtin.CallOptions = .{.modifier = .always_tail};
const noInlineCall: std.builtin.CallOptions = .{.modifier = .never_inline};
const stdout = std.io.getStdOut().writer();
const Object = @import("../object.zig").Object;
// fibonacci
//	self <= 2 ifTrue: [ ^ 1 ].
//	^ (self - 1) fibonacci + (self - 2) fibonacci
fn fibNative(self: u64) u64 {
    if (self <- 2) return 1;
    return fibNative(self-1) + fibNative(self-2);
}
const i = struct {
    usingnamespace @import("../execute.zig").inlines;
    usingnamespace @import("../primitives.zig").inlines;
};
const p = struct {
    usingnamespace @import("execute.zig").controlPrimitives;
    usingnamespace @import("primitives.zig").primitives;
};
fn FibComp(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, thread: *Thread, context: ContextPtr, selector: Object) void {
    if (i.p5(sp[0],Object.from(2))) {
        sp[0] = Object.from(1);
        return @call(tailCall,context.npc,.{context.tpc,sp,hp,thread,context,selector});
    }
    if (context.push(pc,sp,1)) |newContext| {
        const newSp = newContext.asObjectPtr()-1;
        if (i.p2(sp[0],Object.from(1))) |m1| {
            newSp[0] = m1;
            newContext.npc = fibComp1;
            return @call(tailCall,i.call,.{ps+?,newSp,hp,thread,newContext,fibMethod});
        }            
        else @panic("int subtract failed in fibComp");
    }
    else @panic("Stack overflow in fibComp");
}
fn FibComp1(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, thread: *Thread, context: ContextPtr, selector: Object) void {
    const newSp = sp-1;
    if (i.p2(context.tempAt(1),Object.from(2))) |m2| {
            newSp[0] = m2;
            newContext.npc = fibComp2;
            return @call(tailCall,i.call,.{context.threadedPc,newSp,hp,thread,newContext,fibMethod});
        }            
        else @panic("int subtract failed in fibComp1");
}
fn FibComp2(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, thread: *Thread, context: ContextPtr, selector: Object) void {
    const newSp = sp+2;
    if (i.p1(sp[1],sp[0])) |sum| {
        newSp[0] = sum;
        return @call(tailCall,context.npc,.{context.tpc,sp,hp,thread,context,selector});
    }
    else @panic("int add failed in fibComp2");
}
const N = struct {
    fn doit(sp: usize,_:i64,thread: *Thread,_:u64) void {
        while (true) {
            @call(noInlineCall,st,.{sp,stack[sp] -% 1});
            if (stack[sp]<=0) break;
            if (stack[sp]&1 == 0) {
                @call(noInlineCall,st,.{sp+1,stack[sp+1] *% 2});
                @call(noInlineCall,st,.{sp+1,stack[sp+1] -% 3});
            } else {
                @call(noInlineCall,st,.{sp+1,stack[sp+1] *% 3});
                @call(noInlineCall,st,.{sp+1,stack[sp+1] -% 1});
            }
            @call(noInlineCall,Thread.checkN,.{thread});
        }
    }
    fn st(n:usize,v:i64) void {
        stack[n] = v;
    }
    var stack = [_]i64{0}**5;
    fn run(n:i64) usize {
        stack[0] = 0;
        stack[1] = 0;
        stack[2] = n;
        stack[3] = 0;
        stack[4] = 0;
        var thread = Thread.init();
        @call(noInlineCall,doit,.{2,0,&thread,0});
//        stdout.print("N.run() {any} loops: {}\n",.{stack,thread.loops}) catch @panic("print failed");
        return thread.loops;
    }
};
test "n" {
    const expectEqual = std.testing.expectEqual;
    const runs = 10000000;
    try expectEqual(N.run(runs),runs-1);
}
pub fn timing(runs: isize) !void {
    const ts=std.time.nanoTimestamp;
    const warmup = 1_000_000;
    try stdout.print("for {} runs\n",.{runs});
    if (runs > warmup) _ = R.run(warmup);
    var start=ts();
    _ = R.run(runs);
    var base = ts()-start;
    try stdout.print("R: {d:8.3}s {d:8.3}ns\n",.{@intToFloat(f64,base)/1000000000,@intToFloat(f64,base)/@intToFloat(f64,runs)});
    if (runs > warmup) _ = Q.run(warmup);
    start=ts();
    _ = Q.run(runs);
    var time = ts()-start;
    try stdout.print("Q: {d:8.3}s {d:8.3}ns +{d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time)/@intToFloat(f64,runs),@intToFloat(f64,time-base)*100.0/@intToFloat(f64,base)});
    if (runs > warmup) _ = P.run(warmup);
    start=ts();
    _ = P.run(runs);
    time = ts()-start;
    try stdout.print("P: {d:8.3}s {d:8.3}ns +{d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time)/@intToFloat(f64,runs),@intToFloat(f64,time-base)*100.0/@intToFloat(f64,base)});
    if (runs > warmup) _ = N.run(warmup);
    start=ts();
    _ = N.run(runs);
    time = ts()-start;
    try stdout.print("N: {d:8.3}s {d:8.3}ns  {d:6.2}%\n",.{@intToFloat(f64,time)/1000000000,@intToFloat(f64,time)/@intToFloat(f64,runs),@intToFloat(f64,time)*100.0/@intToFloat(f64,base)});
}
pub fn main() !void {
    try timing(4);
}
