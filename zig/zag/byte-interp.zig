const std = @import("std");
const checkEqual = @import("utilities.zig").checkEqual;
const Thread = @import("thread.zig").Thread;
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const NotAnObject = object.NotAnObject;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const heap = @import("heap.zig");
const HeapPtr = heap.HeapPtr;
pub const Hp = heap.HeaderArray;
const Format = heap.Format;
const Age = heap.Age;
const class = @import("class.zig");
const sym = @import("symbol.zig").symbols;
const uniqueSymbol = @import("symbol.zig").uniqueSymbol;
const Context = @import("execute.zig").Context;
const ContextPtr = @import("execute.zig").ContextPtr;
pub const tailCall: std.builtin.CallOptions = .{.modifier = .always_tail};
const noInlineCall: std.builtin.CallOptions = .{.modifier = .never_inline};
const print = std.debug.print;
pub const TestByteExecution = struct {
    thread: Thread,
    ctxt: Context,
    sp: [*]Object,
    hp: Hp,
    pc: [*] const ByteCode,
    const Self = @This();
    var endSp: [*]Object = undefined;
    var endHp: Hp = undefined;
    var endPc: [*] const ByteCode = undefined;
    var baseByteCodeMethod = CompiledByteCodeMethod.init(Nil,0);
    pub fn new() Self {
        return Self {
            .thread = Thread.new(),
            .ctxt = Context.init(&baseByteCodeMethod),
            .sp = undefined,
            .hp = undefined,
            .pc = undefined,
        };
    }
    pub fn init(self: *Self) void {
        self.thread.init();
    }
    fn end(pc: [*]const ByteCode, sp: [*]Object, hp: Hp, _: *Thread, _: ContextPtr) void {
        endPc = pc;
        endHp = hp;
        endSp = sp;
    }
    pub fn run(self: *Self, source: [] const Object, method: CompiledByteCodeMethodPtr) []Object {
        const sp = self.thread.endOfStack() - source.len;
        for (source) |src,idx|
            sp[idx] = src;
        const pc = method.codePtr();
        const hp = self.thread.getHeap();
        self.ctxt.setNPc(Self.end);
        ByteCode.interpret(pc,sp,hp,&self.thread,&self.ctxt);
        self.sp = endSp;
        self.hp = endHp;
        self.pc = endPc;
        return self.thread.stack(self.sp);
    }
};

pub const CompiledByteCodeMethodPtr = *CompiledByteCodeMethod;
pub const CompiledByteCodeMethod = extern struct {
    header: heap.Header,
    name: Object,
    class: Object,
    stackStructure: Object, // number of local values beyond the parameters
    size: u64,
    code: [1] ByteCode,
    const Self = @This();
    const codeOffset = @offsetOf(CompiledByteCodeMethod,"code");
    const nIVars = codeOffset/@sizeOf(Object)-2;
    comptime {
        if (checkEqual(codeOffset,@offsetOf(CompileTimeByteCodeMethod(.{0}),"code"))) |s|
            @compileError("CompileByteCodeMethod prefix not the same as CompileTimeByteCodeMethod == " ++ s);
    }
    const pr = std.io.getStdOut().writer().print;
    fn init(name: Object, size: u64) Self {
        return Self {
            .header = undefined,
            .name = name,
            .class = Nil,
            .stackStructure = Object.from(0),
            .size = size,
            .code = [1]ByteCode{ByteCode.int(0)},
        };
    }
    fn codeSlice(self: * const CompiledByteCodeMethod) [] const ByteCode{
        @setRuntimeSafety(false);
        return self.code[0..self.codeSize()];
    }
    fn codePtr(self: * const CompiledByteCodeMethod) [*] const ByteCode {
        return @ptrCast([*]const ByteCode,&self.code[0]);
    }
    inline fn codeSize(self: * const CompiledByteCodeMethod) usize {
        return @alignCast(8,&self.header).inHeapSize()-@sizeOf(Self)/@sizeOf(Object)+1;
    }
    fn matchedSelector(pc: [*] const ByteCode) bool {
        _ = pc;
        return true;
    }
    fn methodFromCodeOffset(pc: [*] const ByteCode) CompiledByteCodeMethodPtr {
        const method = @intToPtr(CompiledByteCodeMethodPtr,@ptrToInt(pc)-codeOffset-(pc[0].uint)*@sizeOf(ByteCode));
        return method;
    }
    fn print(self: *Self) void {
        pr("CByteCodeMethod: {} {} {} {} (",.{self.header,self.name,self.class,self.stackStructure}) catch @panic("io");
//            for (self.code[0..]) |c| {
//                pr(" 0x{x:0>16}",.{@bitCast(u64,c)}) catch @panic("io");
//            }
        pr(")\n",.{}) catch @panic("io");
    }
};
pub const ByteCode = enum(u8) {
    noop,
    branch,
    ifTrue,
    ifFalse,
    primFailure,
    dup,
    over,
    drop,
    pushLiteral,
    pushLiteral0,
    pushLiteral1,
    pushNil,
    pushTrue,
    pushFalse,
    popIntoTemp,
    popIntoTemp1,
    pushTemp,
    pushTemp1,
    lookupByteCodeMethod,
    send,
    call,
    pushContext,
    returnTrampoline,
    returnWithContext,
    returnTop,
    returnNoContext,
    dnu,
    return_tos,
    failed_test,
    unexpected_return,
    dumpContext,
    exit,
    fn interpret(pc: [*]const ByteCode, sp: [*]Object, hp: Hp, _: *Thread, _: ContextPtr) void {
        while (true) {
            switch (pc[0]) {

            }
        }
    }
    inline fn int(i: i8) ByteCode {
        @setRuntimeSafety(false);
        return @intToEnum(ByteCode,i);
    }
};
fn countNonLabels(comptime tup: anytype) usize {
    var n = 1;
    inline for (tup) |field| {
        switch (@TypeOf(field)) {
            Object => {n+=1;},
            @TypeOf(null) => {n+=1;},
            comptime_int,comptime_float => {n+=1;},
            ByteCode => {n+=1;},
            else => 
                switch (@typeInfo(@TypeOf(field))) {
                    .Pointer => {if (field[field.len-1]!=':') n = n + 1;},
                    else => {n = n+1;},
            }
        }
    }
    return n;
}
fn CompileTimeByteCodeMethod(comptime tup: anytype) type {
    const codeSize = countNonLabels(tup);
    return extern struct { // structure must exactly match CompiledByteCodeMethod
        header: heap.Header,
        name: Object,
        class: Object,
        stackStructure: Object,
        size: u64,
        code: [codeSize] ByteCode,
        const pr = std.io.getStdOut().writer().print;
        const codeOffsetInUnits = CompiledByteCodeMethod.codeOffset/@sizeOf(ByteCode);
        const methodIVars = CompiledByteCodeMethod.nIVars;
        const Self = @This();
        fn init(name: Object, comptime locals: comptime_int) Self {
            return .{
                .header = heap.header(methodIVars,Format.both,class.CompiledByteCodeMethod_I,name.hash24(),Age.static),
                .name = name,
                .class = Nil,
                .stackStructure = Object.packedInt(locals,locals+name.numArgs(),0),
                .size = codeSize,
                .code = undefined,
            };
        }
        pub fn asCompiledByteCodeMethodPtr(self: *Self) * CompiledByteCodeMethod {
            return @ptrCast(* CompiledByteCodeMethod,self);
        }
        pub fn update(self: *Self, tag: Object, method: CompiledByteCodeMethodPtr) void {
            for (self.code) |*c| {
                if (c.object.equals(tag)) c.* = ByteCode.method(method);
            }
        }
        fn headerOffset(_: *Self, codeIndex: usize) ByteCode {
            return ByteCode.uint(codeIndex+codeOffsetInUnits);
        }
        fn getCodeSize(_: *Self) usize {
            return codeSize;
        }
        fn print(self: *Self) void {
            pr("CTByteCodeMethod: {} {} {} {} (",.{self.header,self.name,self.class,self.stackStructure}) catch @panic("io");
            for (self.code[0..]) |c| {
                pr(" 0x{x:0>16}",.{@bitCast(u64,c)}) catch @panic("io");
            }
            pr(")\n",.{}) catch @panic("io");
        }
    };
}
pub fn compileByteCodeMethod(name: Object, comptime parameters: comptime_int, comptime locals: comptime_int, comptime tup: anytype) CompileTimeByteCodeMethod(tup) {
    @setEvalBranchQuota(2000);
    const methodType = CompileTimeByteCodeMethod(tup);
    var method = methodType.init(name,locals);
    comptime var n = 0;
    _ = parameters;
    inline for (tup) |field| {
        switch (@TypeOf(field)) {
            Object => {method.code[n]=ByteCode.object(field);n=n+1;},
            @TypeOf(null) => {method.code[n]=ByteCode.object(Nil);n=n+1;},
            comptime_int => {method.code[n]=ByteCode.int(field);n = n+1;},
            ByteCode => {method.code[n]=ByteCode.prim(field);n=n+1;},
            else => {
                comptime var found = false;
                switch (@typeInfo(@TypeOf(field))) {
                    .Pointer => {
                        if (field[field.len-1]==':') {
                            found = true;
                        } else if (field.len==1 and field[0]=='^') {
                            method.code[n]=ByteCode.int(n);
                            n=n+1;
                            found = true;
                        } else if (field.len==1 and field[0]=='*') {
                            method.code[n]=ByteCode.int(-1);
                            n=n+1;
                            found = true;
                        } else {
                            comptime var lp = 0;
                            inline for (tup) |t| {
                                if (@TypeOf(t) == ByteCode) lp=lp+1
                                    else
                                    switch (@typeInfo(@TypeOf(t))) {
                                        .Pointer => {
                                            if (t[t.len-1]==':') {
                                                if (comptime std.mem.startsWith(u8,t,field)) {
                                                    method.code[n]=ByteCode.int(lp-n-1);
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
//    method.print();
    return method;
}
const b = ByteCode;
test "compiling method" {
    const expectEqual = std.testing.expectEqual;
    const mref = comptime uniqueSymbol(42);
    var m = compileByteCodeMethod(Nil,0,0,.{"abc:", testing.return_tos, "def", True, comptime Object.from(42), "def:", "abc", "*", "^", 3, mref, null});
    const mcmp = m.asCompiledByteCodeMethodPtr();
    m.update(mref,mcmp);
    var t = m.code[0..];
    try expectEqual(t.len,11);
    try expectEqual(t[0].prim,controlPrimitives.noop);
    try expectEqual(t[1].prim,testing.return_tos);
    try expectEqual(t[2].int,2);
    try expectEqual(t[3].object,True);
    try expectEqual(t[4].object,Object.from(42));
    try expectEqual(t[5].int,-5);
    try expectEqual(t[6].int,-1);
    try expectEqual(t[7].int,7);
    try expectEqual(CompiledByteCodeMethod.methodFromCodeOffset((&t[7]).codePtr()),m.asCompiledByteCodeMethodPtr());
    try expectEqual((&t[7]).methodPtr(),mcmp);
    try expectEqual(t[8].int,3);
    try expectEqual(t[9].method,mcmp);
    try expectEqual(t[10].object,Nil);
}
const p = struct {
    usingnamespace controlPrimitives;
};
test "simple return via execute" {
    const expectEqual = std.testing.expectEqual;
    var method = compileByteCodeMethod(Nil,0,0,.{
        p.noop,
        testing.return_tos,
    });
    try expectEqual(testing.testExecute(method.asCompiledByteCodeMethodPtr()),Nil);
}
test "simple return via TestExecution" {
    const expectEqual = std.testing.expectEqual;
    var method = compileByteCodeMethod(Nil,0,0,.{
        p.noop,
        p.pushLiteral,comptime Object.from(42),
        p.returnNoContext,
    });
    var te = TestByteExecution.new();
    te.init();
    var objs = [_]Object{Nil,True};
    var result = te.run(objs[0..],method.asCompiledByteCodeMethodPtr());
    try expectEqual(result.len,3);
    try expectEqual(result[0],Object.from(42));
    try expectEqual(result[1],Nil);
    try expectEqual(result[2],True);
}
test "context return via TestExecution" {
    const expectEqual = std.testing.expectEqual;
    var method = compileByteCodeMethod(Nil,0,0,.{
        p.noop,
        p.pushContext,"^",
        p.pushLiteral,comptime Object.from(42),
        p.returnWithContext,1,
    });
    var te = TestByteExecution.new();
    te.init();
    var objs = [_]Object{Nil,True};
    var result = te.run(objs[0..],method.asCompiledByteCodeMethodPtr());
    try expectEqual(result.len,1);
    try expectEqual(result[0],True);
}
test "context returnTop via TestExecution" {
    const expectEqual = std.testing.expectEqual;
    var method = compileByteCodeMethod(Nil,0,0,.{
        p.noop,
        p.pushContext,"^",
        p.pushLiteral,comptime Object.from(42),
        p.returnTop,1,
    });
    var te = TestByteExecution.new();
    te.init();
    var objs = [_]Object{Nil,True};
    var result = te.run(objs[0..],method.asCompiledByteCodeMethodPtr());
    try expectEqual(result.len,1);
    try expectEqual(result[0],Object.from(42));
}
test "simple executable" {
    var method = compileByteCodeMethod(Nil,0,1,.{
        p.pushContext,"^",
        "label1:",
        p.pushLiteral,comptime Object.from(42),
        p.popIntoTemp,1,
        p.pushTemp1,
        p.pushLiteral0,
        p.pushTrue,
        p.ifFalse,"label3",
        p.branch,"label2",
        "label3:",
        p.pushTemp,1,
        "label4:",
        p.returnWithContext,1,
        "label2:",
        p.pushLiteral0,
        p.branch,"label4",
    });
    var objs = [_]Object{Nil};
    var te = TestByteExecution.new();
    te.init();
    _ = te.run(objs[0..],method.asCompiledByteCodeMethodPtr());
}
