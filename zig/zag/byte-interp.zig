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
const Context = @import("context.zig").Context;
pub const TestExecution = @import("context.zig").TestExecution;
const arenas = @import("arenas.zig");
const heap = @import("heap.zig");
const HeapPtr = heap.HeapPtr;
pub const Hp = heap.HeaderArray;
const Format = heap.Format;
const Age = heap.Age;
const class = @import("class.zig");
const sym = @import("symbol.zig").symbols;
const uniqueSymbol = @import("symbol.zig").uniqueSymbol;
pub const tailCall: std.builtin.CallOptions = .{.modifier = .always_tail};
const noInlineCall: std.builtin.CallOptions = .{.modifier = .never_inline};
const print = std.debug.print;
pub const MethodReturns = void;

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
    pub fn init(name: Object, locals: u16, maxStack: u16) Self {
        return Self {
            .header = undefined,
            .name = name,
            .class = Nil,
            .stackStructure = Object.packedInt(locals,maxStack,locals+name.numArgs()),
            .size = 0,
            .code = [1]ByteCode{ByteCode.int(0)},
        };
    }
    fn codeSlice(self: * const CompiledByteCodeMethod) [] const ByteCode{
        @setRuntimeSafety(false);
        return self.code[0..self.codeSize()];
    }
    pub fn codePtr(self: * const CompiledByteCodeMethod) [*] const ByteCode {
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
        const startOfCode = @ptrToInt(pc-@bitCast(u8,@enumToInt(pc[0])));
        const method = @intToPtr(CompiledByteCodeMethodPtr,startOfCode-codeOffset);
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
pub const ByteCode = enum(i8) {
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
    callLocal,
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
    p1,
    p2,
    p5,
    exit,
    _,
    const Self = @This();
    fn interpret(_pc: [*]const ByteCode, _sp: [*]Object, _hp: Hp, thread: *Thread, _context: *Context(ByteCode,*CompiledByteCodeMethod)) MethodReturns {
        var pc = _pc;
        var sp = _sp;
        var hp = _hp;
        var context = _context;
        const inlines = @import("primitives.zig").inlines;
        interp: while (true) {
            const code = pc[0];
            pc += 1;
            while (true) {
                switch (code) {
                    .noop => {continue :interp;},
                    .branch => {break; // to common code
                    },
                    .ifTrue => {
                        const v = sp[0];
                        sp+=1;
                        if (False.equals(v)) {pc+=1;continue :interp;}
                        if (!True.equals(v)) @panic("non boolean");
                        break; // to branch
                    },
                    .ifFalse => {
                        const v = sp[0];
                        sp+=1;
                        if (True.equals(v)) {pc+=1;continue :interp;}
                        if (!False.equals(v)) @panic("non boolean");
                        break; // to branch
                    },
                    .dup => {
                        sp-=1;
                        sp[0]=sp[1];
                        continue :interp;
                    },
                    .over => {
                        sp-=1;
                        sp[0]=sp[2];
                        continue :interp;
                    },
                    .drop => {
                        sp+=1;
                        continue :interp;
                    },
                    .pushLiteral => {
                        sp-=1;
                        sp[0]=pc[0].o();
                        pc+=1;
                        continue :interp;
                    },
                    .pushLiteral0 => {
                        sp-=1;
                        sp[0]=Object.from(0);
                        continue :interp;
                    },
                    .pushTrue => {
                        sp-=1;
                        sp[0]=True;
                        continue :interp;
                    },
                    .pushContext => {
                        const method = CompiledByteCodeMethod.methodFromCodeOffset(pc);
                        const stackStructure = method.stackStructure;
                        const locals = stackStructure.h0;
                        const maxStackNeeded = stackStructure.h1;
                        const selfOffset = @enumToInt(stackStructure.l2);
                        const result = context.push(sp,hp,thread,method,locals,maxStackNeeded,selfOffset);
                        const ctxt = result.ctxt;
                        ctxt.setNPc(interpret);
                        sp = result.ctxt.asObjectPtr();
                        hp = result.hp;
                        context = ctxt;
                        pc += 1;
                        continue :interp;
                    },
                    .pushTemp => {
                        sp-=1;
                        sp[0]=context.getTemp(pc[0].u()-1);
                        pc += 1;
                        continue :interp;
                    },
                    .pushTemp1 => {
                        sp-=1;
                        sp[0]=context.getTemp(0);
                        continue :interp;
                    },
                    .popIntoTemp => {
                        context.setTemp(pc[0].u(),sp[0]);
                        sp+=1;
                        pc += 1;
                        continue :interp;
                    },
                    .popIntoTemp1 => {
                        context.setTemp(0,sp[0]);
                        sp+=1;
                        continue :interp;
                    },
                    .returnWithContext => {
                        const result = context.pop(thread);
                        const newSp = result.sp;
                        const callerContext = result.ctxt;
                        return @call(tailCall,callerContext.getNPc(),.{callerContext.getTPc(),newSp,hp,thread,callerContext});
                    },
                    .returnNoContext => return @call(tailCall,context.getNPc(),.{context.getTPc(),sp,hp,thread,context}),
                    .returnTop => {
                        const top = sp[0];
                        const result = context.pop(thread);
                        const newSp = result.sp;
                        newSp[0] = top;
                        const callerContext = result.ctxt;
                        return @call(tailCall,callerContext.getNPc(),.{callerContext.getTPc(),newSp,hp,thread,callerContext});
                    },
                    .p1 => {// SmallInteger>>#+
                        sp[1] = inlines.p1(sp[1],sp[0]) catch {pc+=1;continue :interp;};
                        sp+=1;
                        break;
                    },
                    .p2 => {// SmallInteger>>#+
                        sp[1] = inlines.p2(sp[1],sp[0]) catch {pc+=1;continue :interp;};
                        sp+=1;
                        break;
                    },
                    .p5 => { // SmallInteger>>#<=
                        sp[1] = Object.from(inlines.p5(sp[1],sp[0]) catch @panic("<= error"));
                        sp+=1;
                        break; // to branch
                    },
                    .callLocal => {
                        context.setTPc(pc+1);
                        const offset = pc[0].i();
                        if (offset>=0) pc += 1+@intCast(u64, offset) else pc -= @intCast(u64, @as(i32,-offset)-1);
                        continue :interp;
                    },
                    .exit => @panic("fell off the end"),
                    else => { var buf: [100]u8 = undefined;
                             @panic(std.fmt.bufPrint(buf[0..], "unexpected bytecode {}", .{code}) catch unreachable);
                             }
                }
            }
            const offset = pc[0].i();
            if (offset>=0) { pc = pc+1+@intCast(u64, offset); continue;}
            if (offset == -1) {
                @panic("return branch");
            }
            pc = pc+1-@intCast(u64, -offset);
        }
    }
    inline fn i(self: Self) i8 {
        return @enumToInt(self);
    }
    inline fn u(self: Self) u8 {
        return @bitCast(u8,self.i());
    }
    inline fn o(self: Self) Object {
        return objects[self.u()];
    }
    inline fn int(v: i8) ByteCode {
        return @intToEnum(ByteCode,v);
    }
    var objects: [128]Object = undefined;
    var nObjects:usize = 0;
    fn findObject(search: Object) usize {
        for (objects[0..nObjects]) |v,index| {
            if (@bitCast(u64,search)==@bitCast(u64,v))
                return index;
        }
        objects[nObjects] = search;
        nObjects += 1;
        return nObjects-1;
    }
    var methods: [128] CompiledByteCodeMethodPtr = undefined;
    var nMethods = 0;
    fn findMethod(search: CompiledByteCodeMethodPtr) i8 {
        for (methods[0..nMethods]) |v,index| {
            if (search==v)
                return @intCast(i8,index);
        }
        methods[nMethods] = search;
        nMethods += 1;
        return nMethods-1;
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
                    .Pointer => |pointer| {if (@hasField(pointer.child,"len") and field[field.len-1]!=':') n = n + 1;},
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
            return Self {
                .header = heap.header(methodIVars,Format.both,class.CompiledMethod_I,name.hash24(),Age.static),
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
        pub fn update(_: *Self, _: Object, _: CompiledByteCodeMethodPtr) void {
//            for (self.code) |*c| {
//                if (c.asObject().equals(tag)) c.* = ByteCode.method(method);
            //            }
            unreachable;
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
    _ = ByteCode.findObject(Nil);
    inline for (tup) |field| {
        switch (@TypeOf(field)) {
            Object => {method.code[n]=ByteCode.int(@intCast(i8,ByteCode.findObject(field)));n+=1;},
            @TypeOf(null) => {method.code[n]=ByteCode.int(0);n+=1;},
            comptime_int => {method.code[n]=ByteCode.int(field);n+=1;},
            ByteCode => {method.code[n]=field;n+=1;},
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
                                if (@TypeOf(t) == ByteCode) lp+=1
                                    else
                                    switch (@typeInfo(@TypeOf(t))) {
                                       .Pointer => |tPointer| {
                                            if (@hasField(tPointer.child,"len") and t[t.len-1]==':') {
                                                 if (comptime std.mem.startsWith(u8,t,field)) {
                                                    method.code[n]=ByteCode.int(lp-n-1);
                                                    n+=1;
                                                    found = true;
                                                }
                                            } else lp+=1;
                                        },
                                        else => {lp+=1;},
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
    method.code[n]=ByteCode.exit;
//    method.print();
    return method;
}
const b = ByteCode;
test "compiling method" {
    const expectEqual = std.testing.expectEqual;
    const mref = comptime uniqueSymbol(42);
    var m = compileByteCodeMethod(Nil,0,0,.{"abc:", b.return_tos, "def", True, comptime Object.from(42), "def:", "abc", "*", "^", 3, mref, Nil});
//    const mcmp = m.asCompiledByteCodeMethodPtr();
//    m.update(mref,mcmp);
    var t = m.code[0..];
    for (t) | v,idx | {
        std.debug.print("t[{}] = {}\n",.{idx,v});
    }
    try expectEqual(t.len,11);
    try expectEqual(t[0],b.return_tos);
    try expectEqual(t[1].i(),2);
    //try expectEqual(t[2].o(),True);
    //try expectEqual(t[3].o(),Object.from(42));
    try expectEqual(t[4].i(),-5);
    try expectEqual(t[5].i(),-1);
    try expectEqual(t[6].i(),6);
//    try expectEqual(t[?].asMethodPtr(),mcmp);
    try expectEqual(t[7].i(),3);
//    try expectEqual(t[8].method,mcmp);
    try expectEqual(t[9].o(),Nil);
}
pub const TestByteCodeExecution = TestExecution(ByteCode,CompiledByteCodeMethod,&b.interpret);
test "simple return via TestByteCodeExecution" {
    const expectEqual = std.testing.expectEqual;
    var method = compileByteCodeMethod(Nil,0,0,.{
        b.noop,
        b.pushLiteral,comptime Object.from(42),
        b.returnNoContext,
    });
    var te = TestByteCodeExecution.new();
    te.init();
    var objs = [_]Object{Nil,True};
    var result = te.run(objs[0..],method.asCompiledByteCodeMethodPtr());
    try expectEqual(result.len,3);
    try expectEqual(result[0],Object.from(42));
    try expectEqual(result[1],Nil);
    try expectEqual(result[2],True);
}
test "context return via TestByteCodeExecution" {
    const expectEqual = std.testing.expectEqual;
    var method = compileByteCodeMethod(Nil,0,0,.{
        b.noop,
        b.pushContext,"^",
        b.pushLiteral,comptime Object.from(42),
        b.returnWithContext,1,
    });
    var te = TestByteCodeExecution.new();
    te.init();
    var objs = [_]Object{Nil,True};
    var result = te.run(objs[0..],method.asCompiledByteCodeMethodPtr());
    try expectEqual(result.len,1);
    try expectEqual(result[0],True);
}
test "context returnTop via TestByteCodeExecution" {
    const expectEqual = std.testing.expectEqual;
    var method = compileByteCodeMethod(Nil,0,0,.{
        b.noop,
        b.pushContext,"^",
        b.pushLiteral,comptime Object.from(42),
        b.returnTop,1,
    });
    var te = TestByteCodeExecution.new();
    te.init();
    var objs = [_]Object{Nil,True};
    var result = te.run(objs[0..],method.asCompiledByteCodeMethodPtr());
    try expectEqual(result.len,1);
    try expectEqual(result[0],Object.from(42));
}
test "simple executable" {
    var method = compileByteCodeMethod(Nil,0,1,.{
        b.pushContext,"^",
        "label1:",
        b.pushLiteral,comptime Object.from(42),
        b.popIntoTemp,1,
        b.pushTemp1,
        b.pushLiteral0,
        b.pushTrue,
        b.ifFalse,"label3",
        b.branch,"label2",
        "label3:",
        b.pushTemp,1,
        "label4:",
        b.returnWithContext,1,
        "label2:",
        b.pushLiteral0,
        b.branch,"label4",
    });
    var objs = [_]Object{Nil};
    var te = TestByteCodeExecution.new();
    te.init();
    _ = te.run(objs[0..],method.asCompiledByteCodeMethodPtr());
}
