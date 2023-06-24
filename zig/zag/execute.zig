const std = @import("std");
const checkEqual = @import("utilities.zig").checkEqual;
const Process = @import("process.zig").Process;
const ProcessPtr = @import("process.zig").ProcessPtr;
const object = @import("zobject.zig");
const Object = object.Object;
const Nil = object.Nil;
const NotAnObject = object.NotAnObject;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const indexSymbol = object.indexSymbol;
pub const Context = @import("context.zig").Context;
//const TestExecution = @import("context.zig").TestExecution;
const heap = @import("heap.zig");
const HeapObject = heap.HeapObject;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
const Format = heap.Format;
const Age = heap.Age;
const class = @import("class.zig");
const sym = @import("symbol.zig").symbols;
pub const tailCall: std.builtin.CallModifier = .always_tail;
const noInlineCall: std.builtin.CallModifier = .never_inline;
pub const MethodReturns = [*]Object;

pub fn check(pc: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) [*]Object {
    if (process.debugger()) |debugger|
        return  @call(tailCall,debugger,.{pc,sp,process,context,selector});
    return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
}

pub const ThreadedFn = * const fn(programCounter: [*]const Code, stackPointer: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns;
fn noFallbackFn(_: [*]const Code, _: [*]Object, _: *Process, _: CodeContextPtr, _: Object) MethodReturns {
    @panic("no fall back");
}
pub const preferred_noFallback = (&CompiledMethod.init(sym.noFallback,&noFallbackFn)).asFakeObject();
pub const noFallback = &CompiledMethod.init(sym.noFallback,&noFallbackFn);
pub const CodeContextPtr = *Context;
pub const CompiledMethodPtr = *CompiledMethod;
pub const CompiledMethod = extern struct {
    header: HeapObject,
    stackStructure: Object, // number of local values beyond the parameters
    selector: Object, // must be the word before code
    code: [2] Code, // will typically be a lot more then 2, as it will be the processed version of the method
    //references: [n]Object,
    footer: HeapObject,
    const Self = @This();
    pub const codeOffset = @offsetOf(CompiledMethod,"code");
    pub fn init(name: Object, methodFn: ThreadedFn) Self {
        return init2(name,methodFn,Code.end);
    }
    pub fn init2(name: Object, methodFn: ThreadedFn, methodFn2: ThreadedFn) Self {
        return Self {
            .header = HeapObject.staticHeaderWithClassLengthHash(4,class.CompiledMethod_I,name.hash24()),
            .selector = name,
            .stackStructure = Object.from(0),
            .code = [2]Code{Code.prim(methodFn),Code.prim(methodFn2)},
            .footer = HeapObject.calcHeapObject(5,class.CompiledMethod_I,name.hash24(),Age.static,null,0,false) catch unreachable,
        };
    }
    pub fn execute(self: *Self, sp: [*]Object, process: *Process, context: CodeContextPtr) [*]Object {
        const pc = self.codePtr();
//        std.debug.print("execute [{*}]: {*}\n",.{pc,pc[0].prim});
//        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,self.selector});
        return pc[0].prim(pc+1,sp,process,context,self.selector);
    }
    inline fn asHeapObjectPtr(self: * const Self) HeapObjectConstPtr {
        return @ptrCast(HeapObjectConstPtr,self);
    }
    pub inline fn matchedSelector(self: *Self, selector: Object) bool {
        return selector.equals(self.selector);
    }
    pub inline fn codePtr(self: *Self) [*] Code {
        return @ptrCast([*] Code,&self.code);
    }
    pub fn format(
        self: *const Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _=fmt;_=options;
        return self.write(writer);
    }
    pub fn write(
        self: *const Self,
        writer: anytype,
    ) !void {
        const realHO = self.header.realHeapObject();
        const all = @ptrCast([]Code,realHO.asSlice() catch unreachable);
        const refs = realHO.arrayAsSlice(Object) catch unreachable;
        const locals = self.stackStructure.h0;
        const maxStackNeeded = self.stackStructure.h1;
        const selfOffset = self.stackStructure.l2;
        try writer.print("\nCMethod: {} locals:{} maxStack:{} selfOffset:{} realHO:{} {any} ({any})",.{
            self.selector,locals,maxStackNeeded,selfOffset,realHO,
            all[codeOffset/8..all.len-refs.len],refs});
    }
    pub fn asFakeObject(self: *const Self) Object {
        return @bitCast(Object,@ptrToInt(self));
    }
};
pub const Code = extern union {
    prim: ThreadedFn,
    int: i64,
    uint: u64,
    object: Object,
    header: heap.HeapObject,
    codeRef: [*]Code,
    const refFlag = 1024;
    pub inline fn prim(pp: ThreadedFn) Code {
        return Code{.prim=pp};
    }
    inline fn int(i: i64) Code {
        return Code{.int=i};
    }
    pub inline fn uint(u: u64) Code {
        return Code{.uint=u};
    }
    pub inline fn object(o: Object) Code {
        return Code{.object=o};
    }
    inline fn ref(comptime u: u24) Code {
        return Code{.object=indexSymbol(u)};
    }
    inline fn header(h: heap.HeapObject) Code {
        return Code{.header=h};
    }
    pub inline fn codeRef(c: [*]const Code) Code {
        return Code{.codeRef=@constCast(c)};
    }
    pub fn end(_: [*]const Code, sp: [*]Object, _: *Process, _: * Context, _: Object) [*]Object {
        return sp;
    }
    pub const endThread = &[_]Code{.{.prim=&end}};
    inline fn compiledMethodX(self: *const Code) *const CompiledMethod {
       return @ptrCast(*const CompiledMethod,self);
    }
    pub inline fn compiledMethodPtr(self: *const Code, comptime index: comptime_int) *const CompiledMethod {
        return @fieldParentPtr(CompiledMethod,"code",@ptrCast(*const [2]Code,@ptrCast([*]const Code,self)-index));
    }
    pub inline fn literalIndirect(self: *const Code) Object {
        const offset = self.uint;
        return @ptrCast(*const Object,@ptrCast([*]const Code,self)+1+offset).*;
    }
    pub fn format(
        self: *const Code,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        if (!self.object.isDouble()) {
            try writer.print("{}",.{self.object});
        } else if (self.int>=-100 and self.int<100) {
            try writer.print("({})",.{self.int});
        } else
            try writer.print("0x{x}",.{self.uint});
    }
};
pub fn intOf(comptime str: []const u8) u16 {
    var n: u16 = 0;
    for (str) |c| {
        if (c>'9') return n;
        n = n*10 + (c-'0');
    }
    return n;
}
test "intOf" {
    const expectEqual = std.testing.expectEqual;
    try expectEqual(comptime intOf("012Abc"),12);
    try expectEqual(comptime intOf("1230Abc"),1230);
}
pub const CountSizes = struct {codes: usize, refs: usize = 0, objects: usize = 0};
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

pub fn CompileTimeMethod(comptime counts: CountSizes) type {
    const codeSize = counts.codes;
    const refsSize = counts.refs;
    return extern struct { // structure must exactly match CompiledMethod
        header: HeapObject,
        stackStructure: Object,
        selector: Object,
        code: [codeSize] Code,
        references: [refsSize]Object,
        footer: HeapObject,
        const codeOffsetInUnits = CompiledMethod.codeOffset/@sizeOf(Code);
        const Self = @This();
        // comptime {
        //     if (checkEqual(CompiledMethod.codeOffset,@offsetOf(Self,"code"))) |s|
        //         @compileError("CompiledMethod prefix not the same as CompileTimeMethod == " ++ s);
        // }
        pub fn init(name: Object, locals: u16, maxStack: u16) Self {
            const footer = HeapObject.calcHeapObject(codeOffsetInUnits+codeSize, class.CompiledMethod_I, name.hash24(), Age.static, refsSize, @sizeOf(Object), false) catch @compileError("too many refs");
//            @compileLog(codeSize,refsSize);
//            trace("\nfooter={}",.{footer});
            return .{
                .header = HeapObject.staticHeaderWithLength(codeOffsetInUnits-1+codeSize+refsSize),
                .selector = name,
                .stackStructure = Object.packedInt(locals,maxStack,locals+name.numArgs()),
                .code = undefined,
                .references = [_]Object{object.NotAnObject}**refsSize,
                .footer = footer,
            };
        }
        pub fn withCode(name: Object, locals: u16, maxStack: u16, code: [codeSize]Code) Self {
            return .{
                .header = HeapObject.staticHeaderWithLength(codeOffsetInUnits-1+codeSize+refsSize),
                .selector = name,
                .stackStructure = Object.packedInt(locals,maxStack,locals+name.numArgs()),
                .code = code,
                .references = [_]Object{object.NotAnObject}**refsSize,
                .footer = heap.footer(codeOffsetInUnits+codeSize,Format.indexedWithPointers,class.CompiledMethod_I,name.hash24(),Age.static),
            };
        }
        pub fn asCompiledMethodPtr(self: *const Self) * CompiledMethod {
            return @ptrCast(* CompiledMethod,@constCast(self));
        }
        pub fn asFakeObject(self: *const Self) Object {
            return @bitCast(Object,self);
        }
        pub fn setLiterals(self: *Self, replacements: []const Object, refs: []const Object) void {
            for (replacements,1..) |replacement,index| {
                const match =  indexSymbol(@truncate(u24,index));
                if (self.selector.equals(match))
                    self.selector = replacement;
                for (&self.code) |*c| {
                    if (c.object.equals(match))
                        c.* = Code.object(replacement);
                }
            }
            for (refs,self.references[0..refs.len]) |obj,*srefs|
                srefs.* = obj;
            if (self.references.len>0) {
                for (&self.code) |*c| {
                    if (c.object.isIndexSymbol())
                        c.* = Code.uint((@ptrToInt(&self.references[c.object.hash24()&(Code.refFlag-1)])-@ptrToInt(c))/@sizeOf(Object)-1);
                }
            }
        }
        pub fn getCodeSize(_: *Self) usize {
            return codeSize;
        }
        pub fn findObject(self: *Self, search: Object) usize {
            var index = self.references.len-1;
            while (index>=0) : (index -= 1) {
                const v = self.references[index];
                if (search.equals(v))
                    return index;
                if (object.NotAnObject.equals(v)) {
                    self.references[index] = search;
                    return index;
                }
            }
            unreachable;
        }
    };
}
const empty = &[0]Object{};
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
    r1.setLiterals(empty,&r1r);
    try expectEqual(r1.getCodeSize(),11);
}
pub fn compiledMethodType(comptime codeSize: comptime_int) type {
    return CompileTimeMethod(.{.codes=codeSize});
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
                    .Pointer => |fPointer| {
                        switch (@typeInfo(fPointer.child)) {
                            .Array => {
                                if (field[0]==':') {
                                    found = true;
                                } else if (field.len==1 and field[0]=='^') {
                                    code[n]=Code.uint(n);
                                    n=n+1;
                                    found = true;
                                } else if (field.len==1 and field[0]=='*') {
                                    code[n]=Code.int(-1);
                                    n=n+1;
                                    found = true;
                                } else if (field.len>=1 and field[0]>='0' and field[0]<='9') {
                                    code[n]=Code.ref(intOf(field[0..])+Code.refFlag);
                                    n+=1;
                                    found = true;
                                } else {
                                    comptime var lp = 0;
                                    inline for (tup) |t| {
                                        if (@TypeOf(t) == ThreadedFn) lp=lp+1
                                            else
                                            switch (@typeInfo(@TypeOf(t))) {
                                                .Pointer => |tPointer| {
                                                    switch (@typeInfo(tPointer.child)) {
                                                        .Array => {
                                                            if (t[0]==':') {
                                                                if (comptime std.mem.endsWith(u8,t,field)) {
                                                                    code[n]=Code.int(lp-n);
                                                                    n=n+1;
                                                                    found = true;
                                                                }
                                                            } else lp=lp+1;
                                                        },
                                                        //                                                .Fn => |fun| @compileLog(fun),
                                                        else => lp += 1,
                                                    }
                                                },
                                                else => {lp=lp+1;},
                                        }
                                    }
                                    if (!found) @compileError("missing label: \""++field++"\"");
                                }
                            },
                            .Fn => {@compileLog(field);code[n]=Code.prim(field);n=n+1;found = true;},
                            else => {}
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
    m.setLiterals(empty,&[_]Object{Object.from(mcmp)});
    var t = m.code[0..];
//    for (t,0..) |tv,idx|
//        trace("\nt[{}]: 0x{x:0>16}",.{idx,tv.uint});
    try expectEqual(t[0].prim,controlPrimitives.noop);
    try expectEqual(t[1].prim,p.dnu);
    try expectEqual(t[2].int,2);
    try expectEqual(t[3].object,True);
    try expectEqual(t[4].object,Object.from(42));
    try expectEqual(t[5].int,-5);
    try expectEqual(t[6].int,-1);
    try expectEqual(t[7].int,7);
    try expectEqual(t[8].int,3);
    try expectEqual(t[9].int,1);
    try expectEqual(t[10].object,Nil);
    try expectEqual(t.len,11);
}
//pub const trace = std.debug.print;
pub inline fn trace(_:anytype,_:anytype) void {}
pub const controlPrimitives = struct {
    const ContextPtr = CodeContextPtr;
    pub inline fn checkSpace(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, needed: usize) void {
        _ = process;
        _ = pc;
        _ = context;
        _ = sp;
        _ = needed;
    }
    pub fn noop(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    pub fn verifySelector(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const method = (&pc[0]).compiledMethodPtr(1); // must be first word in method, pc already bumped
        if (!method.selector.equals(selector)) return @call(tailCall,dnu,.{pc,sp,process,context,selector});
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    pub fn branch(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const offset = pc[0].int;
        trace("\nbranch offset: {}\n",.{offset});
        if (offset>=0) {
            const target = pc+1+@intCast(u64, offset);
            if (process.needsCheck()) return @call(tailCall,check,.{target,sp,process,context,selector});
            trace("\nbranch target: {}",.{target[0].uint});
            return @call(tailCall,target[0].prim,.{target+1,sp,process,context,selector});
        }
        const target = pc+1-@intCast(u64, -offset);
        if (process.needsCheck()) return @call(tailCall,check,.{target,sp,process,context,selector});
        return @call(tailCall,target[0].prim,.{target+1,sp,process.decCheck(),context,selector});
    }
    pub fn ifTrue(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const v = sp[0];
        if (True.equals(v)) return @call(tailCall,branch,.{pc,sp+1,process,context,selector});
        if (False.equals(v)) return @call(tailCall,pc[1].prim,.{pc+2,sp+1,process,context,selector});
        @panic("non boolean");
    }
    pub fn ifFalse(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        trace("\nifFalse: {any}",.{context.stack(sp,process)});
        const v = sp[0];
        if (False.equals(v)) return @call(tailCall,branch,.{pc,sp+1,process,context,selector});
        if (True.equals(v)) return @call(tailCall,pc[1].prim,.{pc+2,sp+1,process,context,selector});
        @panic("non boolean");
    }
    pub fn ifNil(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const v = sp[0];
        if (Nil.equals(v)) return @call(tailCall,branch,.{pc,sp+1,process,context,selector});
        return @call(tailCall,pc[1].prim,.{pc+2,sp+1,process,context,selector});
    }
    pub fn ifNotNil(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const v = sp[0];
        if (Nil.equals(v)) return @call(tailCall,pc[1].prim,.{pc+2,sp+1,process,context,selector});
        return @call(tailCall,branch,.{pc,sp+1,process,context,selector});
        
    }
    pub fn primFailure(_: [*]const Code, _: [*]Object, _: *Process, _: ContextPtr, _: Object) [*]Object {
        @panic("primFailure");
    }
    pub fn dup(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp-1;
        newSp[0]=newSp[1];
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,process,context,selector});
    }
    pub fn over(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp-1;
        newSp[0]=newSp[2];
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,process,context,selector});
    }
    pub fn drop(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        return @call(tailCall,pc[0].prim,.{pc+1,sp+1,process,context,selector});
    }
    pub fn pushLiteral(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp-1;
        newSp[0]=pc[0].object;
        trace("\npushLiteral: {}",.{newSp[0]});
        return @call(tailCall,pc[1].prim,.{pc+2,newSp,process,context,selector});
    }
    pub fn pushLiteral0(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp-1;
        newSp[0]=Object.from(0);
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,process,context,selector});
    }
    pub fn pushLiteral1(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp-1;
        newSp[0]=Object.from(1);
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,process,context,selector});
    }
    pub fn pushLiteral2(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp-1;
        newSp[0]=Object.from(2);
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,process,context,selector});
    }
    pub fn pushLiteralM1(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp-1;
        newSp[0]=Object.from(-1);
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,process,context,selector});
    }
    pub fn pushLiteralIndirect(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp-1;
        newSp[0]=@ptrCast(*const Code,pc).literalIndirect();
        return @call(tailCall,pc[1].prim,.{pc+2,newSp,process,context,selector});
    }
    pub fn pushLiteralNil(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp-1;
        newSp[0]=Nil;
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,process,context,selector});
    }
    pub fn pushLiteralTrue(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp-1;
        newSp[0]=True;
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,process,context,selector});
    }
    pub fn pushLiteralFalse(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp-1;
        newSp[0]=False;
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,process,context,selector});
    }
    pub fn pushThisContext(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp-1;
        newSp[0]=Object.from(context);
        context.convertToProperHeapObject(sp,process);
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,process,context,selector});
    }
    pub fn pushLocal(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp-1;
        newSp[0]=context.getLocal(pc[0].uint);
        trace("\npushLocal: {} {any} {any}",.{pc[0].uint,context.stack(newSp,process),context.allLocals(process)});
        return @call(tailCall,pc[1].prim,.{pc+2,newSp,process,context,selector});
    }
    pub fn pushLocal0(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp-1;
        newSp[0]=context.getLocal(0);
        trace("\npushLocal1: {any} {*}",.{context.stack(newSp,process),pc});
        return @call(tailCall,pc[0].prim,.{pc+1,newSp,process,context,selector});
    }
    pub fn pushLocalField(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const ref = pc[0].uint;
        const local = context.getLocal(ref&0xfff);
        const newSp = sp-1;
        newSp[0] = local.getField(ref>>12);
        trace("\npushLocalField: {} {} {any} {any}",.{ref,local,context.stack(newSp,process),context.allLocals(process)});
        return @call(tailCall,pc[1].prim,.{pc+2,newSp,process,context,selector});
    }
    pub fn popLocalField(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const ref = pc[0].uint;
        const local = context.getLocal(ref&0xfff);
        trace("\npopLocalField: {} {}",.{ref,sp[0]});
        local.setField(ref>>12,sp[0]);
        return @call(tailCall,pc[1].prim,.{pc+2,sp+1,process,context,selector});
    }
    pub fn popLocal(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        trace("\npopIntoLocal: {} {}",.{pc[0].uint,sp[0]});
        context.setLocal(pc[0].uint,sp[0]);
        return @call(tailCall,pc[1].prim,.{pc+2,sp+1,process,context,selector});
    }
    pub fn popLocal0(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        context.setLocal(0,sp[0]);
        return @call(tailCall,pc[0].prim,.{pc+1,sp+1,process,context,selector});
    }
    fn lookupMethod(cls: object.ClassIndex,selector: u64) CompiledMethodPtr {
        _ = cls;
        _ = selector;
        @panic("unimplemented");
    }
    pub fn send0(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, _: Object) [*]Object {
        const selector = pc[0].object;
        const newPc = lookupMethod(sp[0].get_class(),selector.hash32()).codePtr();
        context.setReturn(pc+1);
        return @call(tailCall,newPc[0].prim,.{newPc+1,sp,process,context,selector});
    }
    pub fn send1(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, _: Object) [*]Object {
        const selector = pc[0].object;
        const newPc = lookupMethod(sp[1].get_class(),selector.hash32()).codePtr();
        context.setReturn(pc+1);
        return @call(tailCall,newPc[0].prim,.{newPc+1,sp,process,context,selector});
    }
    // pub fn send(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, _: Object) [*]Object {
    //     const selector = pc[0].object;
    //     const numArgs = selector.numArgs();
    //     const newPc = lookupMethod(sp[numArgs].get_class(),selector.hash32()).codePtr();
    //     context.setTPc(pc+1);
    //     return @call(tailCall,newPc[0].prim,.{newPc+1,sp,process,context,selector});
    // }
    pub fn call(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        context.setReturn(pc+1);
        const offset = pc[0].uint;
        const method = pc[1+offset].object.to(CompiledMethodPtr);
        const newPc = method.codePtr();
        return @call(tailCall,newPc[0].prim,.{newPc+1,sp,process,context,selector});
    }
    pub fn callRecursive(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        context.setReturn(pc+1);
        const offset = pc[0].int;
        const newPc = pc+1-@intCast(u64, -offset);
        trace("\ncallRecursive: {any} {}",.{context.stack(sp,process)});
        return @call(tailCall,newPc[0].prim,.{newPc+1,sp,process,context,selector});
    }
    pub fn pushContext(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const method =  @intToPtr(CompiledMethodPtr,@ptrToInt(pc-pc[0].uint)-CompiledMethod.codeOffset);
        const stackStructure = method.stackStructure;
        const locals = stackStructure.h0 & 255;
        const maxStackNeeded = stackStructure.h1;
        const selfOffset = stackStructure.l2;
        const ctxt = context.push(sp,process,method,locals,maxStackNeeded,selfOffset);
        const newSp = ctxt.asObjectPtr();
        trace("\npushContext: {any} {} {}",.{process.getStack(sp),locals,method.selector});
        return @call(tailCall,pc[1].prim,.{pc+2,newSp,process,ctxt,selector});
    }
    pub fn returnWithContext(_: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const result = context.pop(process);
        const newSp = result.sp;
        var callerContext = result.ctxt;
        trace("\nreturnWithContext: {any} -> {any}",.{context.stack(sp,process),callerContext.stack(newSp,process)});
        return @call(tailCall,callerContext.getNPc(),.{callerContext.getTPc(),newSp,process,@constCast(callerContext),selector});
    }
    pub fn returnTop(_: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const top = sp[0];
        var result = context.pop(process);
        const newSp = result.sp;
        newSp[0] = top;
        var callerContext = result.ctxt;
        trace("\nreturnTop: {any} -> {any}",.{context.stack(sp,process),callerContext.stack(newSp,process)});
        return @call(tailCall,callerContext.getNPc(),.{callerContext.getTPc(),newSp,process,@constCast(callerContext),selector});
    }
    pub fn returnNoContext(_: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        trace("\nreturnNoContext: N={*} T={*} {any}",.{context.getNPc(),context.getTPc(),context.stack(sp,process)});
        return @call(tailCall,context.getNPc(),.{context.getTPc(),sp,process,context,selector});
    }
    pub fn dnu(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        _ = pc;
        _ = sp;
        _ = process;
        _ = context;
        _ = selector;
        @panic("unimplemented");
    }
};
pub const TestExecution = struct {
    process: Process,
    ctxt: Context,
    sp: [*]Object,
    const Self = @This();
    pub fn new() Self {
        return Self {
            .process = Process.new(),
            .ctxt = Context.init(),
            .sp = undefined,
        };
    }
    pub fn init(self: *Self) void {
        self.process.init();
        self.sp = self.process.endOfStack();
    }
    pub fn run(self: *Self, source: [] const Object, method: CompiledMethodPtr) []Object {
        const sp = self.process.endOfStack() - source.len;
        for (source,sp[0..source.len]) |src,*dst|
            dst.* = src;
        self.ctxt.setReturn(Code.endThread);
        if (@TypeOf(trace)==@TypeOf(std.debug.print) and trace==std.debug.print) method.write(stdout) catch unreachable;
        self.sp = method.execute(sp,&self.process,&self.ctxt);
        return self.ctxt.stack(self.sp,&self.process);
    }
};
const p = struct {
    usingnamespace controlPrimitives;
};
test "simple return via TestExecution" {
    const expectEqual = std.testing.expectEqual;
    var method = compileMethod(sym.yourself,0,0,.{
        &p.pushLiteral,comptime Object.from(42),
        &p.returnNoContext,
    });
    var te = TestExecution.new();
    te.init();
    var objs = [_]Object{Nil,True};
    const compiledMethod = method.asCompiledMethodPtr();
    var result = te.run(objs[0..],compiledMethod);
    try expectEqual(result.len,3);
    try expectEqual(result[0],Object.from(42));
    try expectEqual(result[1],Nil);
    try expectEqual(result[2],True);
}
test "context return via TestExecution" {
    const expectEqual = std.testing.expectEqual;
    var method = compileMethod(sym.@"at:",0,0,.{
        &p.pushContext,"^",
        &p.pushLiteral,comptime Object.from(42),
        &p.returnWithContext,
    });
    var te = TestExecution.new();
    te.init();
    var objs = [_]Object{Nil,True};
    const compiledMethod = method.asCompiledMethodPtr();
    var result = te.run(objs[0..],compiledMethod);
    try expectEqual(result.len,1);
    try expectEqual(result[0],True);
}
test "context returnTop via TestExecution" {
    const expectEqual = std.testing.expectEqual;
    var method = compileMethod(sym.yourself,3,0,.{
        &p.pushContext,"^",
        &p.pushLiteral,comptime Object.from(42),
        &p.returnTop,
    });
    var te = TestExecution.new();
    te.init();
    var objs = [_]Object{Nil,True};
    const compiledMethod = method.asCompiledMethodPtr();
    var result = te.run(objs[0..],compiledMethod);
    try expectEqual(result.len,2);
    try expectEqual(result[0],Object.from(42));
}
test "context returnTop twice via TestExecution" {
    const expectEqual = std.testing.expectEqual;
    var method1 = compileMethod(sym.yourself,3,0,.{
        &p.pushContext,"^",
        &p.pushLiteral,comptime Object.from(1),
        &p.call,"0Obj",
        &p.returnTop,
    });
    var method2 = compileMethod(sym.name,3,0,.{
        &p.pushContext,"^",
        &p.pushLiteral,comptime Object.from(42),
        &p.returnTop,
    });
    method1.setLiterals(empty,&[_]Object{Object.from(method2.asCompiledMethodPtr())});
    var te = TestExecution.new();
    te.init();
    var objs = [_]Object{Nil,True};
    const compiledMethod = method1.asCompiledMethodPtr();
    var result = te.run(objs[0..],compiledMethod);
    try expectEqual(result.len,2);
    try expectEqual(result[0],Object.from(42));
}
test "context returnTop with indirect via TestExecution" {
    const expectEqual = std.testing.expectEqual;
    var method = compileMethod(sym.yourself,3,0,.{
        &p.noop,
        &p.pushContext,"^",
        &p.pushLiteralIndirect,"0Obj",
        &p.returnTop,
    });
    method.setLiterals(empty,&[_]Object{Object.from(42)});
    var te = TestExecution.new();
    te.init();
    var objs = [_]Object{Nil,True};
    const compiledMethod = method.asCompiledMethodPtr();
    var result = te.run(objs[0..],compiledMethod);
    try expectEqual(result.len,2);
    try expectEqual(result[0],Object.from(42));
}
test "simple executable" {
    const expectEqual = std.testing.expectEqual;
    var method = compileMethod(sym.yourself,1,0,.{
        &p.pushContext,"^",
        ":label1",
        &p.pushLiteral,comptime Object.from(42),
        &p.popLocal,0,
        &p.pushLocal0,
        &p.pushLiteral0,
        &p.pushLiteralTrue,
        &p.ifFalse,"label3",
        &p.branch,"label2",
        ":label3",
        &p.pushLocal,0,
        ":label4",
        &p.returnTop,
        ":label2",
        &p.pushLiteral0,
        &p.branch,"label4",
    });
    var objs = [_]Object{Nil};
    var te = TestExecution.new();
    te.init();
    const result = te.run(objs[0..],method.asCompiledMethodPtr());
    trace("result = {any}\n",.{result});
    try expectEqual(result.len,1);
    try expectEqual(result[0],Object.from(0));
}
