const std = @import("std");
const execute = @import("execute.zig");
const ContextPtr = execute.ContextPtr;
const Code = execute.Code;
const tailCall = execute.tailCall;
const compileMethod = execute.compileMethod;
const testExecute = execute.testing.testExecute;
const failed_test = execute.testing.failed_test;
const return_tos = execute.testing.return_tos;
const Thread = @import("thread.zig").Thread;
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const sym = @import("symbol.zig");
const heap = @import("heap.zig");
const HeapPtr = heap.HeapPtr;
const MinSmallInteger: i64 = object.MinSmallInteger;
const MaxSmallInteger: i64 = object.MaxSmallInteger;

pub const inlines = struct {
    pub inline fn p110(self: Object, other: Object) bool { // Identical - can't fail
        return self.equals(other);
    }
    pub inline fn p169(self: Object, other: Object) bool { // NotIdentical - can't fail
        return !self.equals(other);
    }
    pub inline fn p145(self: Object, _: Object) !void { // atAllPut:
        return self;
    }
    pub inline fn p1(self: Object, other: Object) !Object { // Add
        if (other.isInt()) {
            const result = @bitCast(Object,self.u()+%@bitCast(u64,other.toUnchecked(i64)));
            if (result.isInt()) {
                return result;
            }
        }
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
    pub inline fn p3(self: Object, other: Object) !bool { // LessThan
        if (self.u()<other.u()) return true;
        if (other.isInt()) return false;
        return error.primitiveError;
    }
    pub inline fn p4(self: Object, other: Object) !bool { // GreaterThan
        if (self<=other) return false;
        if (other.isInt()) return true;
        return error.primitiveError;
    }
    pub inline fn p5(self: Object, other: Object) !bool { // LessOrEqual
        if (self.u()<=other.u()) return true;
        if (other.isInt()) return false;
        return error.primitiveError;
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
            var result: i51 = undefined;
            if (!@mulWithOverflow(i51,s,o,&result))
                return Object.from(result);
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
    try expectEqual((try inlines.p9(Object.from(3),Object.from(4))).toInt(u64_MINVAL),12);
    try expectEqual(inlines.p9(Object.from(0x10000000),Object.from(0x1000000)),error.primitiveError);
    try expectEqual(inlines.p9(Object.from(0x10000000),Object.from(0x800000),u64_MINVAL),error.primitiveError);
    try expectEqual((try inlines.p9(Object.from(0x1000_0000),Object.from(0x20_0000),u64_MINVAL)).toInt(u64_MINVAL),0x2_0000_0000_0000);
    try expectEqual((try inlines.p9(Object.from(0x1000_0000),Object.from(0x3f_ffff),u64_MINVAL)).toInt(u64_MINVAL),0x3_ffff_f000_0000);
    try expectEqual((try inlines.p9(Object.from(0x1000_0010),Object.from(0x3f_ffff),u64_MINVAL)).toInt(u64_MINVAL),0x3_ffff_f3ff_fff0);
    try expectEqual((try inlines.p9(Object.from(0x10000000),Object.from(-0x400000),u64_MINVAL)).toInt(u64_MINVAL),-0x4_0000_0000_0000);
}

pub const primitives = struct {
    pub fn p1(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, thread: *Thread, context: ContextPtr, selector: Object) void {// SmallInteger>>#+
        sp[1] = inlines.p1(sp[1],sp[0]) catch return @call(tailCall,pc[1].prim.*,.{pc+2,sp,hp,thread,context,selector});
        return @call(tailCall,p.branch,.{pc,sp+1,hp,thread,context,selector});
    }
    pub fn p2(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, thread: *Thread, context: ContextPtr, selector: Object) void {// SmallInteger>>#+
        sp[1] = inlines.p2(sp[1],sp[0]) catch return @call(tailCall,pc[1].prim.*,.{pc+2,sp,hp,thread,context,selector});
        return @call(tailCall,p.branch,.{pc,sp+1,hp,thread,context,selector});
    }
    pub fn p9(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, thread: *Thread, context: ContextPtr, selector: Object) void {// SmallInteger>>#*
        sp[1] = inlines.p9(sp[1],sp[0]) catch return @call(tailCall,pc[1].prim.*,.{pc+2,sp,hp,thread,context,selector});
        return @call(tailCall,p.branch,.{pc,sp+1,hp,thread,context,selector});
    }
    pub fn p9o(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, thread: *Thread, context: ContextPtr, selector: Object) void {// SmallInteger>>#*
        sp[1] = inlines.p9Orig(sp[1],sp[0]) catch return @call(tailCall,pc[1].prim.*,.{pc+2,sp,hp,thread,context,selector});
        return @call(tailCall,p.branch,.{pc,sp+1,hp,thread,context,selector});
    }
    pub fn p5(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, thread: *Thread, context: ContextPtr, selector: Object) void { // ProtoObject>>#==
        sp[1] = Object.from(inlines.p5(sp[1],sp[0]) catch @panic("<= error"));
        return @call(tailCall,p.branch,.{pc,sp+1,hp,thread,context,selector});
    }
    pub fn p110(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, thread: *Thread, context: ContextPtr, selector: Object) void { // ProtoObject>>#==
        sp[1] = Object.from(inlines.p110(sp[1],sp[0]));
        return @call(tailCall,p.branch,.{pc,sp+1,hp,thread,context,selector});
    }
    pub fn p145(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, thread: *Thread, context: ContextPtr, selector: Object) void { // atAllPut:
        inlines.p145(sp[1],sp[0]) catch return @call(tailCall,pc[1].prim.*,.{pc+2,sp,hp,thread,context,selector});
        return @call(tailCall,p.branch,.{pc,sp+1,hp,thread,context,selector});
    }
    pub fn p169(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, thread: *Thread, context: ContextPtr, selector: Object) void { // ProtoObject>>#~~
        sp[1] = Object.from(inlines.p169(sp[1],sp[0]));
        return @call(tailCall,p.branch,.{pc,sp+1,hp,thread,context,selector});
    }
    // pub inline fn p111(pc: [*]const Code, sp: [*]Object, heap: HeapPtr, rpc: [*]const Code, thread: *Thread, caller: Context) Object { // ProtoObject>>class
};
const p = struct {
    usingnamespace @import("execute.zig").controlPrimitives;
    usingnamespace primitives;
};
test "simple ==" {
    const expect = std.testing.expect;
    const prog = compileMethod(Nil,0,0,.{
        &p.pushLiteral,4,
        &p.pushLiteral,4,            
        &p.p110,"next","next:",
        &return_tos,
    });
    const pr = std.io.getStdOut().writer().print;
    const result = testExecute(prog.asCompiledMethodPtr());
    try pr("result = {}\n",.{result});
    try expect(result.to(bool));
}
test "simple add" {
    const expectEqual = std.testing.expectEqual;
    const prog = compileMethod(Nil,0,0,.{
        &p.pushLiteral,3,
        &p.pushLiteral,4,            
        &p.p1,"success",
        &failed_test,
        "success:", &return_tos,
    });
    const pr = std.io.getStdOut().writer().print;
    const result = testExecute(prog.asCompiledMethodPtr());
    try pr("result = {}\n",.{result});
    try expectEqual(result.toInt(u64_MINVAL),7);
}
test "simple add with overflow" {
    const expectEqual = std.testing.expectEqual;
    const prog = compileMethod(Nil,0,0,.{
        &p.pushLiteral,4,            
        &p.pushLiteral, 0x3_ffffffffffff,
        &p.p1,"succeeded",
        &return_tos,
        "succeeded:",&failed_test,
    });
    try expectEqual(testExecute(prog.asCompiledMethodPtr()).toInt(u64_MINVAL),4);
}
test "simple compare" {
    const expectEqual = std.testing.expectEqual;
    const prog = compileMethod(Nil,0,0,.{
        &p.pushLiteral,3,
        &p.pushLiteral,4,            
        &p.p110,"success","success:",
        &return_tos,
    });
    try expectEqual(testExecute(prog.asCompiledMethodPtr()),False);
}
test "simple compare and don't branch" {
    const expectEqual = std.testing.expectEqual;
    const prog = compileMethod(Nil,0,0,.{
        &p.pushLiteral,3,
        &p.pushLiteral,4,            
        &p.p110,"success","success:",
        &p.ifTrue,"true",
        &p.pushLiteral,17,
        &p.branch,"common",
        "true:",
        &p.pushLiteral,42,
        "common:", &return_tos,
    });
    try expectEqual(testExecute(prog.asCompiledMethodPtr()).toInt(u64_MINVAL),17);
}
test "simple compare and branch" {
    const expectEqual = std.testing.expectEqual;
    const prog = compileMethod(Nil,0,0,.{
        &p.pushLiteral,3,
        &p.pushLiteral,4,            
        &p.p169,"success","success:",
        &p.ifTrue,"true",
        &p.pushLiteral,17,
        &p.branch,"common",
        "true:",
        &p.pushLiteral,42,
        "common:", &return_tos,
    });
    try expectEqual(testExecute(prog.asCompiledMethodPtr()).toInt(u64_MINVAL),42);
}

test "dispatch3" {
}
pub fn main() void {
    const prog = compileMethod(Nil,0,0,.{
        &p.pushLiteral,3,
        &p.pushLiteral,4,            
        &p.p1,"success",
        &failed_test,
        "success:", &return_tos,
    });
    _ = testExecute(prog.asCompiledMethodPtr());
}
