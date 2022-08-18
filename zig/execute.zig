const std = @import("std");
const Thread = @import("thread.zig").Thread;
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const NilFlag = object.NilFlag;
const True = object.True;
const False = object.False;
const heap = @import("heap.zig");
const HeapPtr = heap.HeapPtr;
const class = @import("class.zig");
pub const tailCall: std.builtin.CallOptions = .{.modifier = .always_tail};
const PrimitivePtr = fn(programCounter: [*]const Code, stackPointer: [*]Object, heapPointer: HeapPtr, returnPc: [*]const Code, thread: *Thread, thisContext: ContextPtr) Object;

pub const ContextPtr = *Context;
const Context = struct {
    header: heap.Header,
    ctxt: ContextPtr,
    pc: [*]const Code,
    inline fn isInStack(self: *Context, sp: [*]Object, thread: *Thread) bool {
        return @ptrToInt(sp)<@ptrToInt(self) and @ptrToInt(self)<@ptrToInt(thread.endOfStack());
    }
    fn pop(self: ContextPtr, sp: [*]Object, thread: *Thread) struct {
        sp: [*]Object,
        ctxt: ContextPtr,
        pc: [*]const Code,
    } {
        if (self.isInStack(sp,thread))
            return .{.sp=self.asObjectPtr()-1,.ctxt=self.ctxt,.pc=self.pc};
        @panic("restore remote Context");
    }
    fn asObjectPtr(self : *Context) [*]Object {
        return @ptrCast([*]Object,self);
    }
    fn fromObjectPtr(op: [*]Object) *Context {
        return @ptrCast(*Context,op);
    }
};
fn collectNursery(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, rpc: [*]const Code, thread: *Thread, thisContext: ContextPtr)  struct {
    sp: [*]Object,
    ctxt: ContextPtr,
    hp: HeapPtr,
} {
    if (true) @panic("need to collect nursery");
    return @call(tailCall,makeContext,.{pc,sp,hp,rpc,thread,thisContext});
}
fn makeContext(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, rpc: [*]const Code, thread: *Thread, thisContext: ContextPtr)  struct {
    sp: [*]Object,
    ctxt: ContextPtr,
    hp: HeapPtr,
} {
    const newSp = sp - 3;
    if (heap.arenaFree(newSp,hp)<16) return @call(tailCall,collectNursery,.{pc,sp,hp,rpc,thread,thisContext});
    const ctxt = HeapPtr.fromObjectPtr(newSp);
    const size = ((if (thisContext.isInStack(sp,thread))
                       @ptrToInt(thisContext)
                       else
                       @ptrToInt(thread.endOfStack()))-@ptrToInt(newSp))/@sizeOf(Object);
    ctxt.ctxt = thisContext;
    ctxt.rpc = rpc;
    ctxt.header = heap.header(size-1, heap.format, class.Context_I, 0);
    return .{.sp=newSp,.ctxt=ctxt,.hp=hp};
}
pub const Code = packed union {
    prim: PrimitivePtr,
    int: i64,
    uint: u64,
    object: Object,
    fn object(o: Object) Code {
        return Code{.object=o};
    }
    fn end() Code {
        return Code{.object=NilFlag};
    }
    fn int(i: i64) Code {
        return Code{.int=i};
    }
    fn prim(pp: PrimitivePtr) Code {
        return Code{.prim=pp};
    }
};
fn countNonLabels(comptime tup: anytype) usize {
    var n = 0;
    inline for (tup) |field| {
        switch (@typeInfo(@TypeOf(field))) {
            .Pointer => {if (field[field.len-1]!=':') n = n + 1;},
            else => {n = n+1;},
        }
    }
    return n;
}
pub fn compileTuple(name: Object, comptime tup: anytype) [countNonLabels(tup)+2]Code {
    var result: [countNonLabels(tup)+2]Code = undefined;
    result[0] = Code.object(name);
    comptime var n = 1;
    inline for (tup) |field| {
        switch (@TypeOf(field)) {
            Object => {result[n]=Code.object(field);n=n+1;},
            @TypeOf(null) => {result[n]=Code.object(Nil);n=n+1;},
            comptime_int,comptime_float => {result[n]=Code.object(Object.from(field));n = n+1;},
            PrimitivePtr => {result[n]=Code.prim(field);n=n+1;},
            else => {
                comptime var found = false;
                switch (@typeInfo(@TypeOf(field))) {
                    .Pointer => {
                        if (field[field.len-1]==':') {
                            found = true;
                        } else {
                            comptime var lp = 0;
                            inline for (tup) |t| {
                                switch (@typeInfo(@TypeOf(t))) {
                                    .Pointer => {
                                        if (t[t.len-1]==':') {
                                            if (comptime std.mem.startsWith(u8,t,field)) {
                                                result[n]=Code.int(lp+1-n);
                                                n=n+1;
                                                found = true;
                                            }
                                        } else lp=lp+1;
                                    },
                                    else => {lp=lp+1;},
                                }
                            }
                            if (!found) @compileError("missing label: "++field);
                        }
                    },
                    else => {},
                }
                if (!found) @compileError("don't know how to handle \""++@typeName(@TypeOf(field))++"\"");
            },
        }
    }
    result[n]=Code.end();
    return result;
}
const stdout = std.io.getStdOut().writer();
test "compiling tuple" {
    const expectEqual = std.testing.expectEqual;
    var t = compileTuple(Nil,.{"abc:", testing.return_tos, "def", True, 42, "def:", "abc", null});
    try expectEqual(t.len,8);
    try expectEqual(t[0].object,Nil);
    try expectEqual(t[1].prim,testing.return_tos);
    try expectEqual(t[2].int,3);
    try expectEqual(t[3].object,True);
    try expectEqual(t[4].object,Object.from(42));
    try expectEqual(t[5].int,-4);
    try expectEqual(t[6].object,Nil);
    try expectEqual(t[t.len-1].object,NilFlag);
}
fn execute(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, rpc: [*]const Code, thread: *Thread, thisContext: ContextPtr) Object {
    return @call(tailCall,pc[1].prim,.{pc+2,sp,hp,rpc,thread,thisContext});
}
pub const controlPrimitives = struct {
    pub inline fn checkSpace(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, rpc: [*]const Code, thread: *Thread, thisContext: Context, needed: usize) void {
        _ = thread;
        _ = pc;
        _ = hp;
        _ = rpc;
        _ = thisContext;
        _ = sp;
        _ = needed;
    }
    pub fn branch(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, rpc: [*]const Code, thread: *Thread, aContext: ContextPtr) Object {
        const offset = pc[0].int;
        if (offset == -1) {
            const target = aContext.pc;
            return @call(tailCall,target[0].prim,.{target+1,sp,hp,thread,aContext});
        }
        const target = pc+@intCast(u64,if (offset>=0) offset else -offset);
        return @call(tailCall,target[0].prim,.{target+1,sp,hp,rpc,thread,aContext});
    }
    pub fn if_true(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, rpc: [*]const Code, thread: *Thread, thisContext: ContextPtr) Object {
        const v = sp[0];
        if (True.equals(v)) return @call(tailCall,branch,.{pc,sp,hp,rpc,thread,thisContext});
        if (False.equals(v)) return @call(tailCall,pc[1].prim,.{pc+2,sp+1,hp,rpc,thread,thisContext});
        @panic("non boolean");
    }
    pub fn if_false(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, rpc: [*]const Code, thread: *Thread, thisContext: ContextPtr) Object {
        const v = sp[0];
        if (False.equals(v)) return @call(tailCall,branch,.{pc,sp,hp,rpc,thread,thisContext});
        if (True.equals(v)) return @call(tailCall,pc[1].prim,.{pc+2,sp+1,hp,rpc,thread,thisContext});
        @panic("non boolean");
    }
    pub fn pushConst(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, rpc: [*]const Code, thread: *Thread, thisContext: ContextPtr) Object {
        checkSpace(pc,sp,hp,rpc,thread,thisContext,1);
        const newSp = sp-1;
        newSp[0]=pc[0].object;
        return @call(tailCall,pc[1].prim,.{pc+2,newSp,hp,rpc,thread,thisContext});
    }
    pub fn pushConst0(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, rpc: [*]const Code, thread: *Thread, thisContext: ContextPtr) Object {
        checkSpace(pc,sp,hp,rpc,thread,thisContext,1);
        const newSp = sp-1;
        newSp[0]=Object.from(0);
        return @call(tailCall,pc[1].prim,.{pc+1,newSp,hp,rpc,thread,thisContext});
    }
    pub fn pushConst1(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, rpc: [*]const Code, thread: *Thread, thisContext: ContextPtr) Object {
        checkSpace(pc,sp,hp,rpc,thread,thisContext,1);
        const newSp = sp-1;
        newSp[0]=Object.from(1);
        return @call(tailCall,pc[1].prim,.{pc+1,newSp,hp,rpc,thread,thisContext});
    }
    pub fn push1Nil(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, rpc: [*]const Code, thread: *Thread, thisContext: ContextPtr) Object {
        checkSpace(pc,sp,hp,rpc,thread,thisContext,1);
        const newSp = sp-1;
        newSp[0]=Nil;
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,hp,rpc,thread,thisContext});
    }
};
pub const testing = struct {
    pub fn return_tos(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, rpc: [*]const Code, thread: *Thread, thisContext: ContextPtr) Object {
        _ = thread;
        _ = pc;
        _ = hp;
        _ = rpc;
        _ = thisContext;
        return sp[0];
    }
    pub fn failed_test(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, rpc: [*]const Code, thread: *Thread, thisContext: ContextPtr) Object {
        _ = thread;
        _ = pc;
        _ = hp;
        _ = rpc;
        _ = thisContext;
        _ = sp;
        @panic("failed_test");
    }
    pub fn unexpected_return(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, rpc: [*]const Code, thread: *Thread, thisContext: ContextPtr) Object {
        _ = thread;
        _ = pc;
        _ = hp;
        _ = rpc;
        _ = thisContext;
        _ = sp;
        @panic("unexpected_return");
    }
    pub fn testExecute(code: [] const Code) Object {
        var rpc = [_]Code{Code.prim(unexpected_return)};
        const rpcSlice: []Code = rpc[0..];
        var context: Context = undefined;
        var thread = Thread.initForTest() catch unreachable;
        var sp = thread.endOfStack()-1;
        sp[0]=Nil;
            return execute(code.ptr,sp,thread.getArena().heap,rpcSlice.ptr,&thread,&context);
    }
};

test "simple return via execute" {
    const expectEqual = std.testing.expectEqual;
    const prog = compileTuple(Nil,.{
        testing.return_tos,
    });
    try expectEqual(testing.testExecute(prog[0..]),Nil);
}
