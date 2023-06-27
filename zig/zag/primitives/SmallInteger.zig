const std = @import("std");
const execute = @import("../execute.zig");
const trace = execute.trace;
const Context = execute.Context;
const ContextPtr = *Context;
const Code = execute.Code;
const tailCall = execute.tailCall;
const compileMethod = execute.compileMethod;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const Process = @import("../process.zig").Process;
const object = @import("../zobject.zig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const sym = @import("../symbol.zig").symbols;
const heap = @import("../heap.zig");
const MinSmallInteger: i64 = object.MinSmallInteger;
const MaxSmallInteger: i64 = object.MaxSmallInteger;
const empty = &[0]Object{};

pub fn init() void {
}
pub const inlines = struct {
    pub inline fn p1(self: Object, other: Object) !Object { // Add
        if (other.isInt()) {
            const result = @bitCast(Object,self.i()+%other.toUnchecked(i64));
            if (result.isInt()) return result;
        }
        return error.primitiveError;
    }
    pub inline fn p1L(self: Object, other: i32) !Object { // Add a positive literal
        const result = @bitCast(Object,self.i()+%other);
        if (result.atMostInt()) return result;
        return error.primitiveError;
    }
    pub inline fn p_negated(self: Object) !Object { // Negate
        const result = @bitCast(Object,object.u64_ZERO2-%self.u());
        if (result.isInt()) return result;
        return error.primitiveError;
    }
    pub inline fn p2(self: Object, other: Object) !Object { // Subtract
        if (other.isInt()) {
            const result = @bitCast(Object,self.i()-%other.toUnchecked(i64));
            if (result.isInt()) return result;
        }
        return error.primitiveError;
    }
    pub inline fn p2L(self: Object, other: i32) !Object { // Subtract a positive literal
        const result = @bitCast(Object,self.i()-%other);
        if (result.atLeastInt()) return result;
        return error.primitiveError;
    }
    pub inline fn p3(self: Object, other: Object) !bool { // LessThan
        if (!other.isInt()) return error.primitiveError;
        return self.u()<other.u();
    }
    pub inline fn p4(self: Object, other: Object) !bool { // GreaterThan
        if (!other.isInt()) return error.primitiveError;
        return self.u()>other.u();
    }
    pub inline fn p5(self: Object, other: Object) !bool { // LessOrEqual
        if (!other.isInt()) return error.primitiveError;
        return self.u()<=other.u();
    }
    pub inline fn p5N(self: Object, other: Object) bool { // LessOrEqual when both known SmallIntegers
        return self.u()<=other.u();
    }
    pub inline fn p6(self: Object, other: Object) !bool { // GreaterOrEqual
        if (!other.isInt()) return error.primitiveError;
        return self.u()>=other.u();
    }
    pub inline fn p7(self: Object, other: Object) !bool { // Equal
        if (!other.isInt()) return error.primitiveError;
        return self.u()==other.u();
    }
    pub inline fn p8(self: Object, other: Object) !bool { // NotEqual
        if (!other.isInt()) return error.primitiveError;
        return self.u()!=other.u();
    }
    inline fn unsafeAbs(x: i64) u64 {
        @setRuntimeSafety(false);
        return @intCast(u64,if (x < 0) -x else x);
    }
    pub inline fn p9(self: Object, other: Object) !Object { // Multiply
        if (other.isInt()) {
            const s = @truncate(i51,self.toUnchecked(i64));
            const o = @truncate(i51,other.toUnchecked(i64));
            const result = @mulWithOverflow(s,o);
            if (result.@"1"==0) return Object.from(result.@"0");
        }
        return error.primitiveError;
    }
    pub inline fn p9Orig(self: Object, other: Object) !Object { // Multiply
        if (other.isInt()) {
            const s = self.toUnchecked(i64);
            const sBits = @clz(unsafeAbs(s));
            const o = other.toUnchecked(i64);
            const oBits = @clz(unsafeAbs(o));
            if (sBits+oBits>13) return Object.from(s*o);
            if (sBits+oBits==13) {
                const result = s*o;
                if (@ctz(result)==50) return Object.from(result);
            }
        }
        return error.primitiveError;
    }
};

test "inline primitives" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual((try inlines.p9(Object.from(3),Object.from(4))).toInt(),12);
    try expectEqual(inlines.p9(Object.from(0x10000000),Object.from(0x1000000)),error.primitiveError);
    try expectEqual(inlines.p9(Object.from(0x10000000),Object.from(0x800000)),error.primitiveError);
    try expectEqual((try inlines.p9(Object.from(0x1000_0000),Object.from(0x20_0000))).toInt(),0x2_0000_0000_0000);
    try expectEqual((try inlines.p9(Object.from(0x1000_0000),Object.from(0x3f_ffff))).toInt(),0x3_ffff_f000_0000);
    try expectEqual((try inlines.p9(Object.from(0x1000_0010),Object.from(0x3f_ffff))).toInt(),0x3_ffff_f3ff_fff0);
    try expectEqual((try inlines.p9(Object.from(0x1000_0000),Object.from(-0x400_000))).toInt(),-0x4_0000_0000_0000);
    try expectEqual(inlines.p9(Object.from(0x1000_0000),Object.from(0x400_000)),error.primitiveError);
    try expectEqual((try inlines.p_negated(Object.from(42))).toInt(),-42);
    try expectEqual((try inlines.p_negated(Object.from(-0x3_ffff_ffff_ffff))).toInt(),0x3_ffff_ffff_ffff);
    try expectEqual((try inlines.p_negated(Object.from(0x3_ffff_ffff_ffff))).toInt(),-0x3_ffff_ffff_ffff);
    try expectEqual((try inlines.p_negated(Object.from(0))).toInt(),0);
    try expectEqual(inlines.p_negated(Object.from(-0x4_0000_0000_0000)),error.primitiveError);
    try expectEqual(try inlines.p5(Object.from(0),Object.from(0)),true);
    try expectEqual(try inlines.p5(Object.from(0),Object.from(1)),true);
    try expectEqual(try inlines.p5(Object.from(1),Object.from(0)),false);
    try expectEqual(inlines.p5N(Object.from(0),Object.from(0)),true);
    try expectEqual(inlines.p5N(Object.from(0),Object.from(1)),true);
    try expectEqual(inlines.p5N(Object.from(1),Object.from(0)),false);
    try expectEqual(try inlines.p6(Object.from(0),Object.from(0)),true);
    try expectEqual(try inlines.p6(Object.from(0),Object.from(1)),false);
    try expectEqual(try inlines.p6(Object.from(1),Object.from(0)),true);
}
pub const embedded = struct {
    const noFallback = execute.noFallback;
    pub var @"SmallInteger>>#+" = noFallback;
    pub fn p1(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {// SmallInteger>>#+
        trace("\nep1: {any}",.{context.stack(sp,process)});
        sp[1] = inlines.p1(sp[1],sp[0]) catch
            return @call(tailCall,Context.call,.{pc,sp,process,context,@"SmallInteger>>#+".asFakeObject()});
        trace(" -> {any}",.{context.stack(sp+1,process)});
        return @call(tailCall,pc[0].prim,.{pc+1,sp+1,process,context,selector});
    }
    pub fn p1L1(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        sp[0]=inlines.p1L(sp[0],1) catch {
            const newSp = sp - 1;
            newSp[0] = Object.from(1);
            return @call(tailCall,Context.call,.{pc,newSp,process,context,@"SmallInteger>>#+".asFakeObject()});
        };
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    pub var @"SmallInteger>>#-" = noFallback;
    pub fn p2(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {// SmallInteger>>#-
        sp[1] = inlines.p2(sp[1],sp[0]) catch
            return @call(tailCall,Context.call,.{pc,sp,process,context,@"SmallInteger>>#-".asFakeObject()});
        return @call(tailCall,pc[0].prim,.{pc+1,sp+1,process,context,selector});
    }
    pub fn p2L1(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        trace("\nep2L1: {any}",.{context.stack(sp,process)});
        sp[0]=inlines.p2L(sp[0],1) catch {
            const newSp = sp - 1;
            newSp[0] = Object.from(1);
            return @call(tailCall,Context.call,.{pc,newSp,process,context,@"SmallInteger>>#-".asFakeObject()});
        };
        trace(" -> {any}",.{context.stack(sp,process)});
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    pub fn p2L2(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        trace("\nep2L2: {any}",.{context.stack(sp,process)});
        sp[0]=inlines.p2L(sp[0],2) catch {
            const newSp = sp - 1;
            newSp[0] = Object.from(2);
            return @call(tailCall,Context.call,.{pc,newSp,process,context,@"SmallInteger>>#-".asFakeObject()});
        };
        trace(" -> {any}",.{context.stack(sp,process)});
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    pub var @"SmallInteger>>#<=" = noFallback;
    pub fn p5(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {// SmallInteger>>#<=
        sp[1] = Object.from(inlines.p5(sp[1],sp[0]) catch {
            return @call(tailCall,Context.call,.{pc,sp,process,context,@"SmallInteger>>#<=".asFakeObject()});});
        return @call(tailCall,pc[0].prim,.{pc+1,sp+1,process,context,selector});
    }
    pub fn p5N(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // SmallInteger>>#<=
        sp[1] = Object.from(inlines.p5N(sp[1],sp[0]));
        return @call(tailCall,pc[0].prim,.{pc+1,sp+1,process,context,selector});
    }
    pub var @"SmallInteger>>#*" = noFallback;
    pub fn p9(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {// SmallInteger>>#*
        sp[1] = inlines.p9Orig(sp[1],sp[0]) catch
            return @call(tailCall,Context.call,.{pc,sp,process,context,@"SmallInteger>>#*".asFakeObject()});
        return @call(tailCall,pc[0].prim,.{pc+1,sp+1,process,context,selector});
    }
};
pub const primitives = struct {
    const dnu = execute.controlPrimitives.dnu;
    pub fn p1(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {// SmallInteger>>#+
        if (!sym.@"+".hashEquals(selector)) return @call(tailCall,dnu,.{pc,sp,process,context,selector});
        sp[1] = inlines.p1(sp[1],sp[0]) catch
            return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
        return @call(tailCall,context.npc,.{context.tpc,sp+1,process,context,selector});
    }
    pub fn p2(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {// SmallInteger>>#-
        if (sym.@"-".hashEquals(selector)) return @call(tailCall,dnu,.{pc,sp,process,context,selector});
        sp[1] = inlines.p2(sp[1],sp[0]) catch
            return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
        return @call(tailCall,context.npc,.{context.tpc,sp+1,process,context,selector});
    }
    pub fn p7(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // at:
        if (sym.@"at:".hashEquals(selector)) return @call(tailCall,dnu,.{pc,sp,process,context,selector});
        unreachable;
    }
    pub fn p5(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object { // SmallInteger>>#<=
        if (sym.@"<=".hashEquals(selector)) return @call(tailCall,dnu,.{pc,sp,process,context,selector});
        trace("p5: {any}\n",.{context.stack(sp+1,process)});
        sp[1] = Object.from(inlines.p5(sp[1],sp[0])) catch
            return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
        return @call(tailCall,context.npc,.{context.tpc,sp+1,process,context,selector});
    }
    pub fn p9(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {// SmallInteger>>#*
        if (sym.@"*".hashEquals(selector)) return @call(tailCall,dnu,.{pc,sp,process,context,selector});
        sp[1] = inlines.p9(sp[1],sp[0]) catch
            return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
        return @call(tailCall,context.npc,.{context.tpc,sp+1,process,context,selector});
    }
};
const e = struct {
    usingnamespace execute.controlPrimitives;
    usingnamespace embedded;
};
fn testExecute(method: CompiledMethodPtr) []Object {
    var te = execute.TestExecution.new();
    te.init();
    var result = te.run(&[_]Object{Nil},method);
    std.debug.print("result = {any}\n",.{result});
    return result;
}
test "simple add" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(sym.value,0,0,.{
        &e.pushContext,"^",
        &e.pushLiteral,Object.from(3),
        &e.pushLiteral,Object.from(40),
        &e.call,"0Obj",
        &e.pushLiteral,Object.from(-1),
        &e.call,"0Obj",
        &e.returnTop,
    });
    var method2 = compileMethod(sym.@"+",0,0,.{
        &e.p1,
        &e.returnNoContext,
    });
    prog.setLiterals(empty,&[_]Object{Object.from(method2.asCompiledMethodPtr())});
    const result = testExecute(prog.asCompiledMethodPtr());
    try expectEqual(result[0].toInt(),42);
}
test "embedded add" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(sym.value,0,0,.{
        &e.pushContext,"^",
        &e.pushLiteral,Object.from(3),
        &e.pushLiteral,Object.from(40),
        &e.p1,
        &e.pushLiteral,Object.from(-1),
        &e.p1,
        &e.returnTop,
    });
    const result = testExecute(prog.asCompiledMethodPtr());
    try expectEqual(result[0].toInt(),42);
}
test "simple add with overflow" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(sym.value,0,0,.{
        &e.pushContext,"^",
        &e.pushLiteral,Object.from(4),
        &e.pushLiteral,Object.from(0x3_ffff_ffff_ffff),
        &e.p1,
        &e.returnTop,
    });
    var prog2 = compileMethod(sym.@"+",0,0,.{
        &e.pushLiteral,sym.noFallback,            
        &e.returnNoContext,
    });
    embedded.@"SmallInteger>>#+" = prog2.asCompiledMethodPtr();
    const result = testExecute(prog.asCompiledMethodPtr());
    try expectEqual(result[0],sym.noFallback);
}

test "dispatch3" {
}
pub fn main() void {
    var prog = compileMethod(sym.value,0,0,.{
        &e.pushLiteral,3,
        &e.pushLiteral,4,            
        &e.p1,
        &e.pushLiteral,Object.from(-999),
        &e.returnNoContext,
    });
    _ = testExecute(prog.asCompiledMethodPtr());
}
