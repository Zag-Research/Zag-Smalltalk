const std = @import("std");
const checkEqual = @import("utilities.zig").checkEqual;
const Thread = @import("thread.zig").Thread;
const ThreadPtr = @import("thread.zig").ThreadPtr;
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const NotAnObject = object.NotAnObject;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const Context = @import("context.zig").Context;
const TestExecution = @import("context.zig").TestExecution;
const arenas = @import("arenas.zig");
const heap = @import("heap.zig");
const HeapPtr = heap.HeapPtr;
const HeapConstPtr = heap.HeapConstPtr;
pub const Hp = heap.HeaderArray;
const Format = heap.Format;
const Age = heap.Age;
const class = @import("class.zig");
const sym = @import("symbol.zig").symbols;
const indexSymbol = @import("symbol.zig").indexSymbol;
pub const tailCall: std.builtin.CallModifier = .always_tail;
const noInlineCall: std.builtin.CallModifier = .never_inline;
pub const MethodReturns = void;
// union(enum) {
//     Normal: Object,
//     NonLocal,
//     ExceptionSignaled,
//     pub fn nonLocal(self : MethodReturns) bool {
//         return self>=MethodReturns.NonLocal;
//     }
// };

pub const ThreadedFn = * const fn(programCounter: [*]const Code, stackPointer: [*]Object, heapPointer: Hp, thread: *Thread, context: CodeContextPtr) MethodReturns;
pub const CodeContextPtr = *Context;
pub const CompiledMethodPtr = *CompiledMethod;
pub const CompiledMethod = extern struct {
    header: heap.Header,
    selector: Object,
    stackStructure: Object, // number of local values beyond the parameters
    code: [1] Code,
    //size: u64,
    //address: [*]Object,
    //references: [n]Object,
    const Self = @This();
    const codeOffset = @offsetOf(CompiledMethod,"code");
    pub fn init(name: Object,methodFn: ThreadedFn) Self {
        return Self {
            .header = heap.header(3,Format.objectNP,class.Method_I,name.hash24(),Age.static),
            .selector = name,
            .stackStructure = Object.from(0),
            .code = [1]Code{Code.prim(methodFn)},
        };
    }
    pub inline fn execute(self: * const Self, sp: [*]Object, hp: Hp, thread: *Thread, context: CodeContextPtr) void {
        const pc = @ptrCast([*]const Code,&self.code);
//        return @call(tailCall,pc[0].prim,.{pc+1,sp,hp,thread,context});
        return pc[0].prim(pc+1,sp,hp,thread,context);
    }
    inline fn asHeapPtr(self: * const Self) HeapConstPtr {
        return @ptrCast(HeapConstPtr,self);
    }
    pub inline fn matchedSelector(self: *Self, selector: Object) bool {
        return selector.equals(self.selector);
    }
    pub inline fn methodFromCodeOffset(pc: [*]const Code,offset: u64) CompiledMethodPtr {
        return @intToPtr(CompiledMethodPtr,@ptrToInt(pc-offset));
    }
    fn print(self: *Self) void {
        std.debug.print("CMethod: {} {} {} (",.{self.header,self.name,self.stackStructure});
//            for (self.code[0..]) |c| {
//                std.debug.print(" 0x{x:0>16}",.{@bitCast(u64,c)});
//            }
        std.debug.print(")\n",.{});
    }
};
pub const Code = extern union {
    prim: ThreadedFn,
    int: i64,
    uint: u64,
    object: Object,
    header: heap.Header,
    pub inline fn prim(pp: ThreadedFn) Code {
        return Code{.prim=pp};
    }
    inline fn int(i: i64) Code {
        return Code{.int=i};
    }
    inline fn uint(u: u64) Code {
        return Code{.uint=u};
    }
    inline fn object(o: Object) Code {
        return Code{.object=o};
    }
    inline fn ref(u: u64) Code {
        return Code{.object=indexSymbol(@truncate(u24,u))};
    }
    inline fn header(h: heap.Header) Code {
        return Code{.header=h};
    }
    inline fn end() Code {
        return Code{.object=NotAnObject};
    }
    inline fn codePtr(self: * const Code) [*]const Code {
        return @ptrCast([*]const Code,self);
    }
};
fn intOf(str: []const u8) usize {
    var n: u16 = 0;
    for (str) |c| {
        if (c>'9') return n;
        n = n*10 + (c-'0');
    }
    return n;
}
test "intOf" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(intOf("012Abc"),12);
    try expectEqual(intOf("1230Abc"),1230);
}
pub const CountSizes = struct {codes: usize, refs: usize, objects: usize};
pub fn countNonLabels(comptime tup: anytype) CountSizes {
    comptime var n = 1;
    comptime var r = 0;
    comptime var o = 0;
    inline for (tup) |field| {
        switch (@TypeOf(field)) {
            Object => {n+=1;if (!comptime field.isLiteral()) @compileError("use reference for non-literal object");o+=1;},
            @TypeOf(null) => {n+=1;o+=1;},
            comptime_int,comptime_float => {n+=1;},
            else => 
                switch (@typeInfo(@TypeOf(field))) {
                    .Pointer => |pointer| {
                        switch (@typeInfo(pointer.child)) {
                            .Fn => n+=1,
                            else => {
                                if (@hasField(pointer.child,"len"))
                                    switch (field[0]) {
                                        ':' => {},
                                        '0'...'9' => {r = comptime @max(r,intOf(field[0..])+1); n+=1;},
                                        else => n+=1,
                                } else n+=1;
                        }}
                    },
                    else => {n+=1;},
            }
        }
    }
    return .{.codes=n, .refs=r, .objects = o};
}
test "countNonLabels" {
    const expectEqual = std.testing.expectEqual;
    const r1 = countNonLabels(.{
        ":abc",
        &p.dnu,
        "def",
        True,
        comptime Object.from(42),
        ":def",
        "abc",
        "*",
        "^",
        3,
        "1mref",
        null,
    });
    try expectEqual(r1.codes,11);
    try expectEqual(r1.refs,2);
    try expectEqual(r1.objects,3);
}

fn CompileTimeMethod(comptime counts: CountSizes) type {
    const codeSize = counts.codes;
    const refsSize = counts.refs;
    return extern struct { // structure must exactly match CompiledMethod
        header: heap.Header,
        selector: Object,
        stackStructure: Object,
        code: [codeSize] Code,
        size: u64,
        address: [*]Object,
        references: [refsSize]Object,
        const codeOffsetInUnits = CompiledMethod.codeOffset/@sizeOf(Code);
        const Self = @This();
        comptime {
            if (checkEqual(CompiledMethod.codeOffset,@offsetOf(Self,"code"))) |s|
                @compileError("CompiledMethod prefix not the same as CompileTimeMethod == " ++ s);
        }
        fn init(name: Object, comptime locals: comptime_int, comptime maxStack: comptime_int) Self {
            return .{
                .header = heap.header(codeSize+2,Format.bothAP,class.CompiledMethod_I,name.hash24(),Age.static),
                .selector = name,
                .stackStructure = Object.packedInt(locals,maxStack,locals+name.numArgs()),
                .size = refsSize,
                .address = undefined,
                .code = undefined,
                .references = undefined,
            };
        }
        pub fn asCompiledMethodPtr(self: *Self) * CompiledMethod {
            return @ptrCast(* CompiledMethod,self);
        }
        pub fn setReferences(self: *Self, refs: []Object) void {
            if (refs.len!=refsSize) @panic("refs count wrong");
            self.address = @ptrCast([*]Object,&self.references);
            for (refs) |obj,idx|
                self.references[idx] = obj;
            for (self.code) |*c,idx| {
                if (c.object.isIndexSymbol())
                    c.* = Code.uint((@ptrToInt(&self.references[c.object.hash24()])-@ptrToInt(&self.code[idx]))/@sizeOf(Object));
            }
        }
        fn getCodeSize(_: *Self) usize {
            return codeSize;
        }
        fn print(self: *Self) void {
            std.debug.print("CTMethod: {} {} {} (",.{self.header,self.selector,self.stackStructure});
            for (self.code[0..]) |c| {
                std.debug.print(" 0x{x:0>16}",.{@bitCast(u64,c)});
            }
            std.debug.print(")\n",.{});
        }
    };
}
test "CompileTimeMethod" {
    const expectEqual = std.testing.expectEqual;
    const c1 = CompileTimeMethod(countNonLabels(.{
        ":abc",
        &p.dnu,
        "def",
        True,
        comptime Object.from(42),
        ":def",
        "abc",
        "*",
        "^",
        3,
        "1mref",
        null,
    }));
    var r1 = c1.init(Nil,2,3);
    var r1r = [_]Object{Nil,True};
    r1.setReferences(r1r[0..]);
    try expectEqual(r1.getCodeSize(),11);
}
pub fn compileMethod(name: Object, comptime locals: comptime_int, comptime maxStack: comptime_int, comptime tup: anytype) CompileTimeMethod(countNonLabels(tup)) {
    @setEvalBranchQuota(20000);
    const methodType = CompileTimeMethod(countNonLabels(tup));
    var method = methodType.init(name,locals,maxStack);
    method.code[0] = Code.prim(controlPrimitives.noop);
    const code = method.code[0..];
    comptime var n = 1;
    inline for (tup) |field| {
        switch (@TypeOf(field)) {
            Object => {code[n]=Code.object(field);n=n+1;},
            @TypeOf(null) => {code[n]=Code.object(Nil);n=n+1;},
            comptime_int => {code[n]=Code.int(field);n = n+1;},
            ThreadedFn => {code[n]=Code.prim(field);n=n+1;},
            else => {
                comptime var found = false;
                switch (@typeInfo(@TypeOf(field))) {
                    .Pointer => {
                        if (field[0]==':') {
                            found = true;
                        } else if (field.len==1 and field[0]=='^') {
                            code[n]=Code.int(n+3);
                            n=n+1;
                            found = true;
                        } else if (field.len==1 and field[0]=='*') {
                            code[n]=Code.int(-1);
                            n=n+1;
                            found = true;
                        } else if (field.len>=1 and field[0]>='0' and field[0]<='9') {
                            code[n]=Code.ref(intOf(field[0..]));
                            n+=1;
                            found = true;
                         } else {
                            comptime var lp = 0;
                            inline for (tup) |t| {
                                if (@TypeOf(t) == ThreadedFn) lp=lp+1
                                    else
                                    switch (@typeInfo(@TypeOf(t))) {
                                        .Pointer => |tPointer| {
                                            if (@hasField(tPointer.child,"len") and t[0]==':') {
                                                if (comptime std.mem.endsWith(u8,t,field)) {
                                                    code[n]=Code.int(lp-n);
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
    return method;
}
const stdout = std.io.getStdOut().writer();
const print = std.io.getStdOut().writer().print;
test "compiling method" {
    const expectEqual = std.testing.expectEqual;
    var m = compileMethod(sym.yourself,0,0,.{":abc", &p.dnu, "def", True, comptime Object.from(42), ":def", "abc", "*", "^", 3, "0mref", null});
    const mcmp = m.asCompiledMethodPtr();
    m.setReferences(&[_]Object{Object.from(mcmp)});
    var t = m.code[0..];
    for (t) |tv,idx|
        std.debug.print("\nt[{}]: 0x{x:0>16}",.{idx,tv.uint});
    try expectEqual(t[0].prim,controlPrimitives.noop);
    try expectEqual(t[1].prim,p.dnu);
    try expectEqual(t[2].int,2);
    try expectEqual(t[3].object,True);
    try expectEqual(t[4].object,Object.from(42));
    try expectEqual(t[5].int,-5);
    try expectEqual(t[6].int,-1);
    try expectEqual(t[7].int,10);
    try expectEqual(t[8].int,3);
    try expectEqual(t[9].int,4);
    try expectEqual(t[10].object,Nil);
    try expectEqual(t.len,11);
}
//pub const trace = std.debug.print;
pub inline fn trace(_:anytype,_:anytype) void {}
pub const controlPrimitives = struct {
    const ContextPtr = CodeContextPtr;
    pub inline fn checkSpace(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr, needed: usize) void {
        _ = thread;
        _ = pc;
        _ = hp;
        _ = context;
        _ = sp;
        _ = needed;
    }
    pub fn noop(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        return @call(tailCall,pc[0].prim,.{pc+1,sp,hp,thread,context});
    }
    pub fn branch(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const offset = pc[0].int;
        trace("\nbranch offset: {}\n",.{offset});
        if (offset == -1) {
            const target = context.getTPc();
            if (thread.needsCheck()) return @call(tailCall,Thread.check,.{target,sp,hp,thread,context});
            return @call(tailCall,target[0].prim,.{target+1,sp,hp,thread,context});
        }
        if (offset>=0) {
            const target = pc+1+@intCast(u64, offset);
            if (thread.needsCheck()) return @call(tailCall,Thread.check,.{target,sp,hp,thread,context});
            trace("\nbranch target: {}",.{target[0].uint});
            return @call(tailCall,target[0].prim,.{target+1,sp,hp,thread,context});
        }
        const target = pc+1-@intCast(u64, -offset);
        if (thread.needsCheck()) return @call(tailCall,Thread.check,.{target,sp,hp,thread,context});
        return @call(tailCall,target[0].prim,.{target+1,sp,hp,thread.decCheck(),context});
    }
    pub fn ifTrue(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const v = sp[0];
        if (True.equals(v)) return @call(tailCall,branch,.{pc,sp+1,hp,thread,context});
        if (False.equals(v)) return @call(tailCall,pc[1].prim,.{pc+2,sp+1,hp,thread,context});
        @panic("non boolean");
    }
    pub fn ifFalse(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        trace("\nifFalse: {any}",.{context.stack(sp,thread)});
        const v = sp[0];
        if (False.equals(v)) return @call(tailCall,branch,.{pc,sp+1,hp,thread,context});
        if (True.equals(v)) return @call(tailCall,pc[1].prim,.{pc+2,sp+1,hp,thread,context});
        @panic("non boolean");
    }
    pub fn ifNil(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const v = sp[0];
        if (Nil.equals(v)) return @call(tailCall,branch,.{pc,sp+1,hp,thread,context});
        return @call(tailCall,pc[1].prim,.{pc+2,sp+1,hp,thread,context});
    }
    pub fn primFailure(_: [*]const Code, _: [*]Object, _: Hp, _: *Thread, _: ContextPtr) void {
        @panic("primFailure");
    }
    pub fn dup(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        newSp[0]=newSp[1];
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,hp,thread,context});
    }
    pub fn over(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        newSp[0]=newSp[2];
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,hp,thread,context});
    }
    pub fn drop(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        return @call(tailCall,pc[0].prim,.{pc+1,sp+1,hp,thread,context});
    }
    pub fn pushLiteral(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        newSp[0]=pc[0].object;
        trace("\npushLiteral: {}",.{newSp[0]});
        return @call(tailCall,pc[1].prim,.{pc+2,newSp,hp,thread,context});
    }
    pub fn pushLiteral0(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        newSp[0]=Object.from(0);
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,hp,thread,context});
    }
    pub fn pushLiteral1(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        newSp[0]=Object.from(1);
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,hp,thread,context});
    }
    pub fn pushLiteral2(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        newSp[0]=Object.from(2);
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,hp,thread,context});
    }
    pub fn pushLiteralM1(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        newSp[0]=Object.from(-1);
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,hp,thread,context});
    }
    pub fn pushLiteralIndirect(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        const offset = pc[0].int;
        newSp[0]=@intToPtr(*Object,@bitCast(i64,pc)+offset).*;
        trace("\npushLiteralIndirect: {}",.{newSp[0]});
        return @call(tailCall,pc[1].prim,.{pc+2,newSp,hp,thread,context});
    }
    pub fn pushNil(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        newSp[0]=Nil;
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,hp,thread,context});
    }
    pub fn pushTrue(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        newSp[0]=True;
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,hp,thread,context});
    }
    pub fn pushFalse(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        newSp[0]=False;
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,hp,thread,context});
    }
    pub fn pushThisContext(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        newSp[0]=Object.from(context);
        context.convertToProperHeapObject(sp,thread);
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,hp,thread,context});
    }
    pub fn popIntoTemp(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        trace("\npopIntoTemp: {} {}",.{pc[0].uint,sp[0]});
        context.setTemp(pc[0].uint-1,sp[0]);
        return @call(tailCall,pc[1].prim,.{pc+2,sp+1,hp,thread,context});
    }
    pub fn popIntoTemp1(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        context.setTemp(0,sp[0]);
        return @call(tailCall,pc[0].prim,.{pc+1,sp+1,hp,thread,context});
    }
    pub fn pushSelf(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        newSp[0]=context.getSelf();
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,hp,thread,context});
    }
    pub fn pushTemp(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        newSp[0]=context.getTemp(pc[0].uint-1);
        trace("\npushTemp: {} {any} {any}",.{pc[0].uint,context.stack(newSp,thread),context.allTemps()});
        return @call(tailCall,pc[1].prim,.{pc+2,newSp,hp,thread,context});
    }
    pub fn pushTemp1(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const newSp = sp-1;
        newSp[0]=context.getTemp(0);
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,hp,thread,context});
    }
    fn lookupMethod(cls: class.ClassIndex,selector: u64) CompiledMethodPtr {
        _ = cls;
        _ = selector;
        @panic("unimplemented");
    }
    pub fn send(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        _=pc; _=sp; _=hp; _=thread; _=context;
        @panic("not implemented");
        // const selector = pc[0].object;
        // const numArgs = selector.numArgs();
        // const newPc = lookupMethod(sp[numArgs].get_class(),selector.hash32()).codePtr();
        // context.setTPc(pc+1);
        // return @call(tailCall,newPc[0].prim,.{newPc+1,sp,hp,thread,context});
    }
    pub fn call(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        context.setTPc(pc+1);
        const newPc = pc[0].method.codePtr();
        return @call(tailCall,newPc[0].prim,.{newPc+1,sp,hp,thread,context});
    }
    pub fn callLocal(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        context.setTPc(pc+1);
        const offset = pc[0].int;
        const newPc = if (offset>=0) pc+1+@intCast(u64, offset) else pc+1-@intCast(u64, -offset);
        return @call(tailCall,newPc[0].prim,.{newPc+1,sp,hp,thread,context});
    }
    pub fn pushContext(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const method = CompiledMethod.methodFromCodeOffset(pc,pc[0].uint);
        const stackStructure = method.stackStructure;
        const locals = stackStructure.h0;
        const maxStackNeeded = stackStructure.h1;
        const selfOffset = @enumToInt(stackStructure.l2);
        const result = context.push(sp,hp,thread,method,locals,maxStackNeeded,selfOffset);
        const ctxt = result.ctxt;
        ctxt.setNPc(returnTrampoline);
        trace("\npushContext: {any} {} {any}",.{thread.stack(sp),locals,thread.stack(result.ctxt.asObjectPtr())});
        return @call(tailCall,pc[1].prim,.{pc+2,result.ctxt.asObjectPtr(),result.hp,thread,ctxt});
    }
    pub fn returnTrampoline(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        return @call(tailCall,pc[0].prim,.{pc+1,sp,hp,thread,context});
    }
    pub fn returnWithContext(_: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const result = context.pop(thread);
        const newSp = result.sp;
        const callerContext = result.ctxt;
        trace("\nreturnWithContext: {any} -> {any}",.{context.stack(sp,thread),callerContext.stack(newSp,thread)});
        return @call(tailCall,callerContext.getNPc(),.{callerContext.getTPc(),newSp,hp,thread,callerContext});
    }
    pub fn returnTop(_: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        const top = sp[0];
        trace("\nreturnTop: {any}",.{context.stack(sp,thread)});
        trace(" 0x{x}:{}",.{@ptrToInt(context),context});
        const result = context.pop(thread);
        const newSp = result.sp;
        newSp[0] = top;
        const callerContext = result.ctxt;
        trace(" ->",.{});
        trace(" {any}",.{callerContext.stack(newSp,thread)});
        return @call(tailCall,callerContext.getNPc(),.{callerContext.getTPc(),newSp,hp,thread,callerContext});
    }
    pub fn returnNoContext(_: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        return @call(tailCall,context.getNPc(),.{context.getTPc(),sp,hp,thread,context});
    }
    pub fn dnu(pc: [*]const Code, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) void {
        _ = pc;
        _ = sp;
        _ = hp;
        _ = thread;
        _ = context;
        @panic("unimplemented");
    }
};
const p = struct {
    usingnamespace controlPrimitives;
};
// test "simple return via TestExecution" {
//     const expectEqual = std.testing.expectEqual;
//     var method = compileMethod(sym.yourself,0,0,.{
//         &p.noop,
//         &p.pushLiteral,comptime Object.from(42),
//         &p.returnNoContext,
//     });
//     var te = TestCodeExecution.new();
//     te.init();
//     var objs = [_]Object{Nil,True};
//     var result = te.run(objs[0..],method.asCompiledMethodPtr());
//     try expectEqual(result.len,3);
//     try expectEqual(result[0],Object.from(42));
//     try expectEqual(result[1],Nil);
//     try expectEqual(result[2],True);
// }
// test "context return via TestExecution" {
//     const expectEqual = std.testing.expectEqual;
//     var method = compileMethod(sym.@"at:",0,0,.{
//         &p.noop,
//         &p.pushContext,"^",
//         &p.pushLiteral,comptime Object.from(42),
//         &p.returnWithContext,
//     });
//     var te = TestCodeExecution.new();
//     te.init();
//     var objs = [_]Object{Nil,True};
//     var result = te.run(objs[0..],method.asCompiledMethodPtr());
//     try expectEqual(result.len,1);
//     try expectEqual(result[0],True);
// }
// test "context returnTop via TestExecution" {
//     const expectEqual = std.testing.expectEqual;
//     var method = compileMethod(sym.yourself,3,0,.{
//         &p.noop,
//         &p.pushContext,"^",
//         &p.pushLiteral,comptime Object.from(42),
//         &p.returnTop,
//     });
//     var te = TestCodeExecution.new();
//     te.init();
//     var objs = [_]Object{Nil,True};
//     const result = te.run(objs[0..],method.asCompiledMethodPtr());
//     try expectEqual(result.len,2);
//     try expectEqual(result[0],Object.from(42));
// }
// test "context returnTop with indirect via TestExecution" {
//     const expectEqual = std.testing.expectEqual;
//     var method = compileMethod(sym.yourself,3,0,.{
//         &p.noop,
//         &p.pushContext,"^",
//         &p.pushLiteralIndirect,"0Obj",
//         &p.returnTop,
//     });
//     method.references([_]Object{Object.from(42)});
//     var te = TestCodeExecution.new();
//     te.init();
//     var objs = [_]Object{Nil,True};
//     const result = te.run(objs[0..],method.asCompiledMethodPtr());
//     try expectEqual(result.len,2);
//     try expectEqual(result[0],Object.from(42));
// }
// test "simple executable" {
//     const expectEqual = std.testing.expectEqual;
//     var method = compileMethod(sym.yourself,1,0,.{
//         &p.pushContext,"^",
//         ":label1",
//         &p.pushLiteral,comptime Object.from(42),
//         &p.popIntoTemp,1,
//         &p.pushTemp1,
//         &p.pushLiteral0,
//         &p.pushTrue,
//         &p.ifFalse,"label3",
//         &p.branch,"label2",
//         ":label3",
//         &p.pushTemp,1,
//         ":label4",
//         &p.returnTop,
//         ":label2",
//         &p.pushLiteral0,
//         &p.branch,"label4",
//     });
//     var objs = [_]Object{Nil};
//     var te = TestCodeExecution.new();
//     te.init();
//     const result = te.run(objs[0..],method.asCompiledMethodPtr());
//     std.debug.print("result = {any}\n",.{result});
//     try expectEqual(result.len,1);
//     try expectEqual(result[0],Object.from(0));
// }
