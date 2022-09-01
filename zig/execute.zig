const std = @import("std");
const Thread = @import("thread.zig").Thread;
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const NotAnObject = object.NotAnObject;
const True = object.True;
const False = object.False;
const heap = @import("heap.zig");
const HeapPtr = heap.HeapPtr;
const class = @import("class.zig");
const sym = @import("symbol.zig").symbols;
pub const tailCall: std.builtin.CallOptions = .{.modifier = .always_tail};
pub const PrimitivePtr = * const fn(programCounter: [*]const Code, stackPointer: [*]Object, heapPointer: HeapPtr, pauseFlag: i64, thread: *Thread, context: ContextPtr) void;

pub const ContextPtr = *Context;
const Context = struct {
    header: heap.Header,
    pc: Object, // this is a [*]const Code so points into the middle of the Method object, but has to be an object, so could be stored as an integer offset, or as a cast int
    prevCtxt: ?Object,
    method: Object,
    size: u64,
    temps: [1]Object,
    fn print(self: ContextPtr,sp: [*]Object, thread: *Thread) void {
        const pr = std.io.getStdOut().writer().print;
        pr("Context: pc: 0x{x:0>16} {any} stack: {any}\n",.{self.getPc(),self.temps(),self.stack()});
        if (self.prevCtxt) |ctxt| {ctxt.print(sp,thread);}
    }
    fn temps(self: ContextPtr) []Object {
        @setRuntimeSafety(false);
        return self.temps[0..self.nTemps()];
    }
    fn stack(self: ContextPtr) []Object {
        @setRuntimeSafety(false);
        return self.temps[self.nTemps()..self.size];
    }
    inline fn nTemps(_: ContextPtr) usize {
        return 0;
    }
    inline fn getPc(self: ContextPtr) [*]const Code {
        _ = self;
        @panic("unimplemented");
    }
    inline fn setPc(self: ContextPtr, pc: [*]const Code) void {
        _ = self;
        _ = pc;
        @panic("unimplemented");
    }
    inline fn isInStack(self: *Context, sp: [*]Object, thread: *Thread) bool {
        return @ptrToInt(sp)<@ptrToInt(self) and @ptrToInt(self)<@ptrToInt(thread.endOfStack());
    }
    inline fn getTemp(self: ContextPtr, n: usize) Object {
        @setRuntimeSafety(false);
        return self.temps[n];
    }
    inline fn setTemp(self: ContextPtr, n: usize, v: Object) void {
        @setRuntimeSafety(false);
        self.temps[n] = v;
    }
    fn pop(self: ContextPtr, sp: [*]Object, thread: *Thread) struct {
        sp: [*]Object,
        ctxt: ContextPtr,
        pc: [*]const Code,
    } {
        if (self.isInStack(sp,thread))
            return .{.sp=self.asObjectPtr()-1,.ctxt=self.ctxt,.pc=self.getPc()};
        @panic("restore remote Context");
    }
    fn asObjectPtr(self : *Context) [*]Object {
        return @ptrCast([*]Object,self);
    }
    fn fromObjectPtr(op: [*]Object) *Context {
        return @ptrCast(*Context,op);
    }
    fn collectNursery(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) Object {
        if (true) @panic("need to collect nursery");
        return @call(tailCall,push,.{pc,sp,hp,doCheck,thread,context});
    }
    fn push(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        const method = pc[0].method;
        const stackInfo = method.stackInfo.toUnchecked(u64);
        const locals = stackInfo & 255;
        const newSp = sp - 5 - locals;
        const maxStackNeeded = @truncate(u32,stackInfo>>8);
        if (doCheck<=0 or heap.arenaFree(newSp,hp)<5+maxStackNeeded)
            return @call(tailCall,Thread.check,.{pc-1,sp,hp,@as(i64,5+maxStackNeeded),thread,context}); // redo this instruction after collect
        const ctxt = @ptrCast(ContextPtr,@alignCast(@alignOf(Context),newSp));
        const size = ((if (context.isInStack(sp,thread))
                           @ptrToInt(context)
                           else
                           @ptrToInt(thread.endOfStack()))-@ptrToInt(newSp))/@sizeOf(Object);
        ctxt.header = heap.header(3, heap.Format.both, class.Context_I, 0);
        ctxt.pc = Nil;
        ctxt.prevCtxt = Object.from(context);
        ctxt.method = Object.from(method);
        ctxt.size = size;
        for (ctxt.temps[0..locals]) |*local| {local.*=Nil;}
        return @call(tailCall,pc[1].prim.*,.{pc+2,newSp,hp,doCheck-1,thread,context});
    }
};

pub const MethodPtr = *Method;
pub const Method = struct {
    header: heap.Header,
    name: Object,
    class: Object,
    stackInfo: Object, // number of local values beyond the parameters
    size: u64, // number of code values (i.e. size of the array portion of the method)
    code: [1] Code,
    fn codeSlice(self: * const Method) [] const Code{
        @setRuntimeSafety(false);
        return self.code[0..self.size];
    }
    fn codePtr(self: * const Method) [*] const Code {
        return @ptrCast([*]const Code,&self.code[0]);
    }
};
pub const Code = packed union {
    prim: PrimitivePtr,
    int: i64,
    uint: u64,
    object: Object,
    header: heap.Header,
    method: MethodPtr,
    fn prim(pp: PrimitivePtr) Code {
        return Code{.prim=pp};
    }
    fn int(i: i64) Code {
        return Code{.int=i};
    }
    fn uint(u: u64) Code {
        return Code{.uint=u};
    }
    fn object(o: Object) Code {
        return Code{.object=o};
    }
    fn header(h: heap.Header) Code {
        return Code{.header=h};
    }
    fn method(m: MethodPtr) Code {
        return Code{.method=m};
    }
    fn end() Code {
        return Code{.object=NotAnObject};
    }
};
fn countNonLabels(comptime tup: anytype) usize {
    var n = 0;
    inline for (tup) |field| {
        switch (@TypeOf(field)) {
            Object => {n+=1;},
            @TypeOf(null) => {n+=1;},
            comptime_int,comptime_float => {n+=1;},
            PrimitivePtr => {n+=1;},
            else => 
                switch (@typeInfo(@TypeOf(field))) {
                    .Pointer => {if (field[field.len-1]!=':') n = n + 1;},
                    else => {n = n+1;},
            }
        }
    }
    return n;
}
fn CompileTimeMethod(comptime tup: anytype) type {
    const codeSize = countNonLabels(tup);
    return struct { // structure must exactly match Method
        header: heap.Header,
        name: Object,
        class: Object,
        stackInfo: Object,
        size: u64,
        code: [codeSize] Code,
        const pr = std.io.getStdOut().writer().print;
        const codeOffset = @sizeOf(Method)/@sizeOf(Code)-1;
        const methodIVars = codeOffset-2;
        const Self = @This();
        fn init(name: Object, comptime stackInfo: comptime_int) Self {
            return .{
                .header = heap.header(methodIVars,heap.Format.both,class.Method_I,name.hash_u24()),
                .name = name,
                .class = Nil,
                .stackInfo = Object.from(stackInfo),
                .size = codeSize,
                .code = undefined,
            };
        }
        fn asMethodPtr(self: *Self) * const Method {
            return @ptrCast(* const Method,self);
        }
        fn headerOffset(_: *Self, codeIndex: usize) Code {
            return Code.uint(codeIndex+codeOffset);
        }
        fn getCodeSize(_: *Self) usize {
            return codeSize;
        }
        fn print(self: *Self) void {
            pr("Method: {} {} {} {} (",.{self.header,self.name,self.class,self.stackInfo}) catch @panic("io");
            for (self.code[0..]) |c| {
                pr(" 0x{x:0>16}",.{@bitCast(u64,c)}) catch @panic("io");
            }
            pr(")\n",.{}) catch @panic("io");
        }
    };
}
pub fn compileMethod(name: Object, comptime parameters: comptime_int, comptime locals: comptime_int, comptime tup: anytype) CompileTimeMethod(tup) {
//    var result: [countNonLabels(tup)+Method.codeOffset]Code = undefined;
    const methodType = CompileTimeMethod(tup);
    var method = methodType.init(name,locals);
    comptime var n = 0;
    _ = parameters;
    inline for (tup) |field| {
        switch (@TypeOf(field)) {
            Object => {method.code[n]=Code.object(field);n=n+1;},
            @TypeOf(null) => {method.code[n]=Code.object(Nil);n=n+1;},
            comptime_int,comptime_float => {method.code[n]=Code.object(Object.from(field));n = n+1;},
            PrimitivePtr => {method.code[n]=Code.prim(field);n=n+1;},
            else => {
                comptime var found = false;
                switch (@typeInfo(@TypeOf(field))) {
                    .Pointer => {
                        if (field[field.len-1]==':') {
                            found = true;
                        } else if (field.len==1 and field[0]=='^') {
                            method.code[n]=Code.int(n);
                            n=n+1;
                            found = true;
                        } else if (field.len==1 and field[0]=='*') {
                            method.code[n]=Code.int(-1);
                            n=n+1;
                            found = true;
                        } else {
                            comptime var lp = 0;
                            inline for (tup) |t| {
                                if (@TypeOf(t) == PrimitivePtr) lp=lp+1
                                    else
                                    switch (@typeInfo(@TypeOf(t))) {
                                        .Pointer => {
                                            if (t[t.len-1]==':') {
                                                if (comptime std.mem.startsWith(u8,t,field)) {
                                                    method.code[n]=Code.int(lp-n-1);
                                                    n=n+1;
                                                    found = true;
                                                }
                                            } else lp=lp+1;
                                        },
                                        else => {lp=lp+1;},
                                }
                            }
                            if (!found) @compileError("missing label: \""++field++"\"");
                        }
                    },
                    else => {},
                }
                if (!found) @compileError("don't know how to handle \""++@typeName(@TypeOf(field))++"\"");
            },
        }
    }
//    method.code[n]=Code.end();
    method.print();
    return method;
}
const stdout = std.io.getStdOut().writer();
const print = std.io.getStdOut().writer().print;
test "compiling method" {
    const expectEqual = std.testing.expectEqual;
    var m = compileMethod(Nil,0,0,.{"abc:", &testing.return_tos, "def", True, 42, "def:", "abc", "*", "^", null});
    var t = m.code[0..];
//    for (t[0..]) |code| { try print(" {}",.{code.int}); }
    try expectEqual(t.len,8);
//    try expectEqual(t[0].prim,&testing.return_tos);
    try expectEqual(t[1].int,2);
    try expectEqual(t[2].object,True);
    try expectEqual(t[3].object,Object.from(42));
    try expectEqual(t[4].int,-5);
    try expectEqual(t[5].int,-1);
    try expectEqual(t[6].int,6);
    try expectEqual(t[7].object,Nil);
}
fn execute(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
    return @call(tailCall,pc[0].prim.*,.{pc+1,sp,hp,doCheck,thread,context});
}
pub const controlPrimitives = struct {
    pub inline fn checkSpace(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: Context, needed: usize) void {
        _ = thread;
        _ = pc;
        _ = hp;
        _ = doCheck;
        _ = context;
        _ = sp;
        _ = needed;
    }
    pub fn branch(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        const offset = pc[0].int;
        if (offset>=0) {
            const target = pc+1+@intCast(u64, offset);
            if (doCheck<=0) return @call(tailCall,Thread.check,.{target,sp,hp,0,thread,context});
            return @call(tailCall,target[0].prim.*,.{target+1,sp,hp,doCheck,thread,context});
        }
        if (offset == -1) {
            const target = context.getPc();
            if (doCheck<=0) return @call(tailCall,Thread.check,.{target,sp,hp,0,thread,context});
            return @call(tailCall,target[0].prim.*,.{target+1,sp,hp,doCheck,thread,context});
        }
        const target = pc+1-@intCast(u64, -offset);
        if (doCheck<=0) return @call(tailCall,Thread.check,.{target,sp,hp,0,thread,context});
        return @call(tailCall,target[0].prim.*,.{target+1,sp,hp,doCheck-1,thread,context});
    }
    pub fn ifTrue(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        const v = sp[0];
        if (True.equals(v)) return @call(tailCall,branch,.{pc,sp+1,hp,doCheck,thread,context});
        if (doCheck<=0) return @call(tailCall,Thread.check1,.{pc+1,sp,hp,0,thread,context});
        if (False.equals(v)) return @call(tailCall,pc[1].prim.*,.{pc+2,sp+1,hp,doCheck,thread,context});
        @panic("non boolean");
    }
    pub fn ifFalse(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        const v = sp[0];
        if (False.equals(v)) return @call(tailCall,branch,.{pc,sp+1,hp,doCheck,thread,context});
        if (True.equals(v)) return @call(tailCall,pc[1].prim.*,.{pc+2,sp+1,hp,doCheck,thread,context});
        @panic("non boolean");
    }
    pub fn pushLiteral(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        newSp[0]=pc[0].object;
        return @call(tailCall,pc[1].prim.*,.{pc+2,newSp,hp,doCheck,thread,context});
    }
    pub fn pushLiteral0(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        newSp[0]=Object.from(0);
        return @call(tailCall,pc[1].prim.*,.{pc+1,newSp,hp,doCheck,thread,context});
    }
    pub fn pushLiteral1(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        newSp[0]=Object.from(1);
        return @call(tailCall,pc[1].prim.*,.{pc+1,newSp,hp,doCheck,thread,context});
    }
    pub fn push1Nil(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        newSp[0]=Nil;
        return @call(tailCall,pc[0].prim.*,.{pc+1,newSp,hp,doCheck,thread,context});
    }
    pub fn push2Nil(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-2;
        newSp[0]=Nil;
        newSp[1]=Nil;
        return @call(tailCall,pc[0].prim.*,.{pc+1,newSp,hp,doCheck,thread,context});
    }
    pub fn popIntoTemp(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        context.setTemp(pc[0].uint,sp[0]);
        return @call(tailCall,pc[1].prim.*,.{pc+2,sp+1,hp,doCheck,thread,context});
    }
    pub fn pushTemp(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        newSp[0]=context.getTemp(pc[0].uint);
        return @call(tailCall,pc[1].prim.*,.{pc+2,newSp,hp,doCheck,thread,context});
    }
    fn lookupMethod(cls: class.ClassIndex,selector: Object) MethodPtr {
        _ = cls;
        _ = selector;
        @panic("unimplemented");
    }
    pub fn send(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        const selector = pc[0].object;
        const numArgs = selector.numArgs();
        const newPc = lookupMethod(sp[numArgs].get_class(),selector).codePtr();
        context.setPc(pc+1);
        return @call(tailCall,newPc[0].prim.*,.{newPc+1,sp,hp,doCheck,thread,context});
    }
    pub fn call(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        context.setReturn(pc+1);
        const newPc = pc[0].method.codePtr();
        return @call(tailCall,newPc[0].prim.*,.{newPc+1,sp,hp,doCheck,thread,context});
    }
    pub fn returnWithContext(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        _ = pc;
        _ = sp;
        _ = hp;
        _ = doCheck;
        _ = thread;
        _ = context;
//        const result = sp[0];
//        context.setReturn(pc+1);
//        const newPc = pc[0].method.codePtr();
//        return @call(tailCall,newPc[0].prim.*,.{newPc+1,sp,hp,doCheck,thread,context});
        @panic("unimplemented");
    }
};
const p = struct {
    const pushContext = Context.push;
    usingnamespace controlPrimitives;
};
pub const testing = struct {
    pub fn return_tos(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        _ = doCheck;
        _ = thread;
        _ = pc;
        _ = sp;
        _ = hp;
        _ = context;
        return;
    }
    pub fn failed_test(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        _ = doCheck;
        _ = thread;
        _ = pc;
        _ = hp;
        _ = context;
        _ = sp;
        @panic("failed_test");
    }
    pub fn unexpected_return(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        _ = doCheck;
        _ = thread;
        _ = pc;
        _ = hp;
        _ = context;
        _ = sp;
        @panic("unexpected_return");
    }
    pub fn dumpContext(pc: [*]const Code, sp: [*]Object, hp: HeapPtr, doCheck: i64, thread: *Thread, context: ContextPtr) void {
        print("pc: 0x{x:0>16} sp: 0x{x:0>16} hp: 0x{x:0>16}",.{pc,sp,hp});
        context.print(sp,thread);
        return @call(tailCall,pc[0].prim.*,.{pc+1,sp,hp,doCheck,thread,context});
    }
    pub fn testExecute(prog: * const Method) Object {
        const code = prog.codeSlice();
        var context: Context = undefined;
        var thread = Thread.initForTest() catch unreachable;
        var sp = thread.endOfStack()-1;
        sp[0]=Nil;
        execute(code.ptr,sp,thread.getArena().heap,1000,&thread,&context);
        return sp[0];
    }
};

test "simple return via execute" {
    const expectEqual = std.testing.expectEqual;
    var prog = compileMethod(Nil,0,0,.{
        &testing.return_tos,
    });
    try expectEqual(testing.testExecute(prog.asMethodPtr()),Nil);
}
test "simple executable" {
    const prog = compileMethod(Nil,0,1,.{
        &p.pushContext,"^",
        "label1:",
        &p.pushLiteral,42,
        &p.popIntoTemp,1,
        &p.pushTemp,1,
        &p.pushLiteral0,
        &p.send,sym.@"<",
        &p.ifFalse,"label3",
        "label2",
        "label3:",
        &p.pushTemp,1,
        "label4:",
        &p.returnWithContext,
        "label2:",
        &p.pushLiteral0,
        "label4",
    });
    _ = prog;
}
