const std = @import("std");
const execute = @import("execute.zig");
const trace = execute.trace;
const ContextPtr = execute.CodeContextPtr;
const Code = execute.Code;
const tailCall = execute.tailCall;
const compileMethod = execute.compileMethod;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const Thread = @import("thread.zig").Thread;
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const sym = @import("symbol.zig").symbols;
const heap = @import("heap.zig");
const Hp = heap.HeaderArray;
const MinSmallInteger: i64 = object.MinSmallInteger;
const MaxSmallInteger: i64 = object.MaxSmallInteger;

pub const inlines = struct {
    pub inline fn p1(self: Object, other: Object) !Object { // Add
        if (other.isInt()) {
            const result = @bitCast(Object,self.u()+%@bitCast(u64,other.toUnchecked(i64)));
            if (result.isInt()) {
                return result;
            }
        }
        return error.primitiveError;
    }
    pub inline fn p1L(self: Object, other: i64) !Object { // Add a literal
        const result = @bitCast(Object,self.i()+other);
        if (result.isInt()) return result;
        return error.primitiveError;
    }
    pub inline fn p2(self: Object, other: Object) !Object { // Subtract
        if (other.isInt()) {
            const result = @bitCast(Object,self.i()-%other.toUnchecked(i64));
            if (result.isInt()) {
                return result;
            }
        }
        return error.primitiveError;
    }
    pub inline fn p2L(self: Object, other: i64) !Object { // Subtract a literal
        const result = @bitCast(Object,self.i()-other);
        if (result.isInt()) return result;
        return error.primitiveError;
    }
    pub inline fn p3(self: Object, other: Object) !bool { // LessThan
        if (self.u()<other.u()) return true;
        if (other.isInt()) return false;
        return error.primitiveError;
    }
    pub inline fn p4(self: Object, other: Object) !bool { // GreaterThan
        if (self.u()<=other.u()) return false;
        if (other.isInt()) return true;
        return error.primitiveError;
    }
    pub inline fn p5(self: Object, other: Object) !bool { // LessOrEqual
        if (self.u()<=other.u()) return true;
        if (other.isInt()) return false;
        return error.primitiveError;
    }
    pub inline fn p5N(self: Object, other: Object) bool { // LessOrEqual when both known SmallIntegers
        return self.u()<=other.u();
    }
    pub inline fn p6(self: Object, other: Object) !bool { // GreaterOrEqual
        if (self<other) return false;
        if (other.isInt()) return true;
        return error.primitiveError;
    }
    pub inline fn p7(self: Object, other: Object) !bool { // Equal
        if (self==other) return true;
        if (other.isInt()) return false;
        return error.primitiveError;
    }
    pub inline fn p8(self: Object, other: Object) !bool { // NotEqual
        if (self==other) return false;
        if (other.isInt()) return true;
        return error.primitiveError;
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
            if (result.overflow==0)
                return Object.from(result.result);
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
    pub inline fn p60(self: Object, other: Object) !Object { // at:
        _ = self; _ = other;
        return error.primitiveError;
    }
    pub inline fn p61(self: Object, other: Object) !Object { // at:put:
        _ = self; _ = other;
        return error.primitiveError;
    }
    pub inline fn p71(self: Object, other: Object) !Object { // basicNew:
        _ = self; _ = other;
        return error.primitiveError;
    }
    pub inline fn p110(self: Object, other: Object) bool { // Identical - can't fail
        return self.equals(other);
    }
    pub inline fn p145(self: Object, other: Object) !Object { // atAllPut:
        _ = self; _ = other;
        return error.primitiveError;
    }
    pub inline fn p169(self: Object, other: Object) bool { // NotIdentical - can't fail
        return !self.equals(other);
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
    try expectEqual((try inlines.p9(Object.from(0x10000000),Object.from(-0x400000))).toInt(),-0x4_0000_0000_0000);
}

pub const primitives = struct {
    pub fn p1(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {// SmallInteger>>#+
        sp[1] = inlines.p1(sp[1],sp[0]) catch return @call(tailCall,pc[1].prim,.{pc+2,sp,hp,thread,context});
        return @call(tailCall,p.branch,.{pc,sp+1,hp,thread,context});
    }
    pub fn p1L1(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        sp[0]=inlines.p1L(sp[0],1) catch return @call(tailCall,pc[1].prim,.{pc+2,sp,hp,thread,context});
        return @call(tailCall,p.branch,.{pc,sp,hp,thread,context});
    }
    pub fn p2(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {// SmallInteger>>#+
        sp[1] = inlines.p2(sp[1],sp[0]) catch return @call(tailCall,pc[1].prim,.{pc+2,sp,hp,thread,context});
        return @call(tailCall,p.branch,.{pc,sp+1,hp,thread,context});
    }
    pub fn p2L1(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        sp[0]=inlines.p2L(sp[0],1) catch return @call(tailCall,pc[1].prim,.{pc+2,sp,hp,thread,context});
        return @call(tailCall,p.branch,.{pc,sp,hp,thread,context});
    }
    pub fn p2L2(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        sp[0]=inlines.p2L(sp[0],2) catch return @call(tailCall,pc[1].prim,.{pc+2,sp,hp,thread,context});
        return @call(tailCall,p.branch,.{pc,sp,hp,thread,context});
    }
    pub fn p7(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void { // at:
        _ = pc; _ = sp; _ = hp; _ = thread; _ = context; unreachable;
    }
    pub fn p5(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void { // SmallInteger>>#<=
        sp[1] = Object.from(inlines.p5(sp[1],sp[0]) catch @panic("<= error"));
        trace("p5: {any}\n",.{context.stack(sp+1)});
        return @call(tailCall,p.branch,.{pc,sp+1,hp,thread,context});
    }
    pub fn p5N(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void { // SmallInteger>>#<=
        sp[1] = Object.from(inlines.p5N(sp[1],sp[0]));
        return @call(tailCall,pc[0].prim,.{pc+1,sp+1,hp,thread,context});
    }
    pub fn p9(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {// SmallInteger>>#*
        sp[1] = inlines.p9(sp[1],sp[0]) catch return @call(tailCall,pc[1].prim,.{pc+2,sp,hp,thread,context});
        return @call(tailCall,p.branch,.{pc,sp+1,hp,thread,context});
    }
    pub fn p9o(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {// SmallInteger>>#*
        sp[1] = inlines.p9Orig(sp[1],sp[0]) catch return @call(tailCall,pc[1].prim,.{pc+2,sp,hp,thread,context});
        return @call(tailCall,p.branch,.{pc,sp+1,hp,thread,context});
    }
    pub fn p60(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void { // at:
        _ = pc; _ = sp; _ = hp; _ = thread; _ = context; unreachable;
    }
    pub fn p61(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void { // at:
        _ = pc; _ = sp; _ = hp; _ = thread; _ = context; unreachable;
    }
    pub fn p71(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void { // at:
        _ = pc; _ = sp; _ = hp; _ = thread; _ = context; unreachable;
    }
    pub fn p110(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void { // ProtoObject>>#==
        sp[1] = Object.from(inlines.p110(sp[1],sp[0]));
        return @call(tailCall,p.branch,.{pc,sp+1,hp,thread,context});
    }
    pub fn p145(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void { // atAllPut:
        inlines.p145(sp[1],sp[0]) catch return @call(tailCall,pc[1].prim,.{pc+2,sp,hp,thread,context});
        return @call(tailCall,p.branch,.{pc,sp+1,hp,thread,context});
    }
    pub fn p169(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void { // ProtoObject>>#~~
        sp[1] = Object.from(inlines.p169(sp[1],sp[0]));
        return @call(tailCall,p.branch,.{pc,sp+1,hp,thread,context});
    }
    // pub inline fn p111(pc: [*]const Code, sp: [*]Object, heap: Hp, rpc: [*]const Code, thread: *Thread, caller: Context) Object { // ProtoObject>>class
};
const p = struct {
    usingnamespace @import("execute.zig").controlPrimitives;
    usingnamespace primitives;
};
test "simple ==" {
    const expect = std.testing.expect;
    var prog = compileMethod(sym.value,0,0,.{
        &p.pushLiteral,Object.from(4),
        &p.pushLiteral,Object.from(4),
        &p.p110,"next","next:",
        &p.returnNoContext,
    });
    const result = testExecute(prog.asCompiledMethodPtr());
    try expect(result[0].to(bool));
}
fn testExecute(method: CompiledMethodPtr) []Object {
    var te = @import("execute.zig").TestCodeExecution.new();
    te.init();
    var objs = [_]Object{};
    var result = te.run(objs[0..],method);
    std.debug.print("result = {any}\n",.{result});
    return result;
}
test "simple add" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(sym.value,0,0,.{
        &p.pushLiteral,Object.from(3),
        &p.pushLiteral,Object.from(4),
        &p.p1,"success",
        &p.pushLiteral,Object.from(-999),
        "success:", &p.returnNoContext,
    });
    const result = testExecute(prog.asCompiledMethodPtr());
    try expectEqual(result[0].toInt(),7);
}
test "simple add with overflow" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(sym.value,0,0,.{
        &p.pushLiteral,Object.from(4),            
        &p.pushLiteral,Object.from(0x3_ffffffffffff),
        &p.p1,"succeeded",
        &p.pushLiteral,Object.from(-999),
        "succeeded:",
        &p.returnNoContext,
    });
    try expectEqual(testExecute(prog.asCompiledMethodPtr())[0].toInt(),-999);
}
test "simple compare" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(sym.value,0,0,.{
        &p.pushLiteral,Object.from(3),
        &p.pushLiteral,Object.from(4),
        &p.p110,"success","success:",
        &p.returnNoContext,
    });
    try expectEqual(testExecute(prog.asCompiledMethodPtr())[0],False);
}
test "simple compare and don't branch" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(sym.value,0,0,.{
        &p.pushLiteral,Object.from(3),
        &p.pushLiteral,Object.from(4),
        &p.p110,"success","success:",
        &p.ifTrue,"true",
        &p.pushLiteral,Object.from(17),
        &p.branch,"common",
        "true:",
        &p.pushLiteral,Object.from(42),
        "common:", &p.returnNoContext,
    });
    try expectEqual(testExecute(prog.asCompiledMethodPtr())[0].toInt(),17);
}
test "simple compare and branch" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(sym.value,0,0,.{
        &p.pushLiteral,Object.from(3),
        &p.pushLiteral,Object.from(4),
        &p.p169,"success","success:",
        &p.ifTrue,"true",
        &p.pushLiteral,Object.from(17),
        &p.branch,"common",
        "true:",
        &p.pushLiteral,Object.from(42),
        "common:", &p.returnNoContext,
    });
    try expectEqual(testExecute(prog.asCompiledMethodPtr())[0].toInt(),42);
}

test "dispatch3" {
}
pub fn main() void {
    var prog = compileMethod(sym.value,0,0,.{
        &p.pushLiteral,3,
        &p.pushLiteral,4,            
        &p.p1,"success",
        &p.pushLiteral,Object.from(-999),
        "success:", &p.returnNoContext,
    });
    _ = testExecute(prog.asCompiledMethodPtr());
}
