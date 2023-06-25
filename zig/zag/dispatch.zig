const std = @import("std");
const tailCall: std.builtin.CallModifier = .always_tail;
const object = @import("zobject.zig");
const Object = object.Object;
const Nil = object.Nil;
const class = @import("class.zig");
const ClassIndex = class.ClassIndex;
const max_classes = class.ReservedNumberOfClasses;
const Process = @import("process.zig").Process;
const heap = @import("heap.zig");
const HeapPtr = heap.HeapPtr;
const HeapObject = heap.HeapObject;
const builtin = @import("builtin");
const symbol = @import("symbol.zig");
const symbols = symbol.symbols;
const execute = @import("execute.zig");
const Context = execute.Context;
const TestExecution = execute.TestExecution;
const MethodReturns = execute.MethodReturns;
const ThreadedFn = execute.ThreadedFn;
const CompiledMethod = execute.CompiledMethod;
const compileMethod = execute.compileMethod;
const compiledMethodType = execute.compiledMethodType;
const Code = execute.Code;
const CodeContextPtr = execute.CodeContextPtr;
const u32_phi_inverse=@import("utilities.zig").inversePhi(u32);
// note that self and other could become invalid after any method call if they are heap objects, so will need to be re-loaded from context.fields if needed thereafter

pub const forTest = Dispatch.forTest;
const noArgs = ([0]Object{})[0..];
const Dispatch = extern struct {
    header: HeapObject,
    hash: u64,
    free: u16,
    length: u16,
    state: DispatchState,
    superOrDNU: [2]Code, // could handle DNU separately, but no current reason
    methods: [minHash+extra][*]const Code, // this is just the default... normally a larger array
    const Self = @This();
    const classIndex = class.Dispatch_I;
    const DispatchState = enum(u8){clean,beingUpdated,dead};
    const minHash = 13; // must be prime
    const extra = 8; // must be multiple of 8 to allow cast below
    var internal = [_]ThreadedFn{&super}**(bitTests.len+5);
    var empty: Self = .{
        .header = HeapObject.staticHeaderWithClassLengthHash(classIndex,@offsetOf(Self,"methods")/8-1+1,0), // don't count header, but do count one element of methods
        .hash = 1,
        .free = 0,
        .length = 1,
        .state = .clean,
        .superOrDNU = .{Code.prim(&dnu),Code.uint(0)},
        .methods = undefined, // should make this a footer
    };
    var dispatches = [_]Self{&empty}**max_classes;
    pub inline fn lookup(selector: Object,classIndex: ClassIndex) [*]const Code {
        return dispatches[classIndex].lookupAddress(preHash(selector.hash32())).*;
    }
    var internalNeedsInitialization = true;
    fn new() Self {
        if (internalNeedsInitialization) {
            empty.methods[0] = @ptrCast([*]const Code,&empty.superOrDNU);
            empty.header.addFooter();
            internal[0] = super;
            internal[1] = dnu;
            internal[2] = fail;
            internal[3] = prime3;
            internal[4] = prime5;
            for (bitTests[0..],internal[5..]) |s,*i| {
                i.* = s;
            }
            std.sort.insertion(ThreadedFn,&internal,{},lessThan);
            internalNeedsInitialization = false;
        }
        return undefined;
    }
    fn lessThan(_:void, lhs: ThreadedFn, rhs: ThreadedFn) bool {
        return @ptrToInt(lhs)<@ptrToInt(rhs);
    }
    inline fn initPrivate(self: *Self, code: [2]Code) void { // should only be used by next three functions or tests
        self.header = HeapObject.staticHeaderWithClassLengthHash(classIndex,4,0);
        self.hash = minHash;
        self.superOrDNU = code;
        for (self.methods[0..minHash]) |*ptr|
            ptr.* = @ptrCast([*]const Code,&self.superOrDNU);
        self.free = minHash;
        self.length = @sizeOf(Self);
        self.state = .clean;
        if (extra>0)
            self.methods[minHash] = @intToPtr([*]const Code,extra);
    }
    fn initSuper(self: *Self, superClass: ClassIndex) void {
        self.initPrivate(.{Code.prim(&super),Code.uint(superClass)});
    }
    fn initDNU(self: *Self) void {
        self.initPrivate(.{Code.prim(&dnu),Code.uint(0)});
    }
    fn initTest(self: *Self, target: *usize) void {
        self.initPrivate(.{Code.prim(&testIncrement),Code.uint(@ptrToInt(target))});
    }
    pub fn forTest() void {
        var foo = Self.new();
        foo.initDNU();
    }
    fn isExternalCompiledMethod(self: *Self, cmp: ThreadedFn) bool {
        const ptr = @ptrToInt(cmp);
        const cmpVsDispatchDifferential = CompiledMethod.codeOffset-@offsetOf(Self,"superOrDNU"); // fudge because Dispatch and CM are different
        if (ptr>=@ptrToInt(self)-cmpVsDispatchDifferential and ptr<=(@ptrToInt(self)+self.length)*@sizeOf(Object)) return false;
        var low: usize = 0;
        var high: usize = internal.len;
        while (low<high) {
            const mid = (low+high)/2;
            const v = internal[mid];
            if (v==cmp) return false;
            if (@ptrToInt(v)<ptr) {
                low = mid+1;
            } else {
                high = mid;
            }
        }
        return true;
    }
    fn isAvailable(self: *Self, cmp: *const CompiledMethod) bool {
        const ptr = @ptrToInt(cmp);
        if (ptr==@ptrToInt((&self.superOrDNU[0]).compiledMethodPtr(0))) return true;
        return false;
    }
    fn lookupAddress(self: *Self, selector: u64) *[*]const Code {
        return &self.methods[selector*self.hash>>32];
    }
    inline fn preHash(selector: u32) u64 {
        return @as(u64,selector*%u32_phi_inverse);
    }
    pub fn dispatch(self: *Self, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const hash = selector.hash32();
        const hashed = preHash(hash);
        const code = self.lookupAddress(hashed).*;
        // all the ugly casting is to make signature match
        return @call(tailCall,@ptrCast(*const fn(*Self,[*]Object,*Process,CodeContextPtr,Object) MethodReturns,code[0].prim),.{@ptrCast(*Dispatch,@constCast(code+1)),sp,process,context,selector});
    }
    fn disambiguate(location: [] Code,one: *const CompiledMethod, another: *const CompiledMethod) [*]Code {
        const oneHash = one.selector.hash32();
        const anotherHash = another.selector.hash32();
        const shift = @ctz(oneHash^anotherHash);
        if (shift==32) unreachable;
        const bit = @as(u64,1)<<shift;
        if (oneHash&bit==0) {
            location[1] = Code.codeRef(&one.code);
            location[2] = Code.codeRef(&another.code);
        } else {
            location[1] = Code.codeRef(&another.code);
            location[2] = Code.codeRef(&one.code);
        }
        location[0]= Code.prim(bitTests[shift]);
        return location.ptr;
    }
    fn add(self: *Self, cmp: *CompiledMethod) !void {
        while (true) {
            if (@cmpxchgWeak(DispatchState,&self.state,.clean,.beingUpdated,.SeqCst,.SeqCst)) |notClean| {
                if (notClean==.dead) return error.Conflict;
            } else break;
        }
        defer {self.state = .clean;}
        const hashed = preHash(cmp.selector.hash32());
        const address = self.lookupAddress(hashed);
        if (@cmpxchgWeak([*]const Code,address,&self.superOrDNU,cmp.codePtr(),.SeqCst,.SeqCst)==null)
            return; // we replaced DNU with method
        const existing = @ptrCast(*const Code,address.*).compiledMethodPtr(0);
        if (existing.selector.equals(cmp.selector)) {
            address.* = cmp.codePtr();
            return;
        }
        if (self.isExternalCompiledMethod(@constCast(existing).codePtr()[0].prim)) { // an actual cmp - not internal
            const end = self.length-@offsetOf(Self,"methods")/@sizeOf(Object)-2;
            if (self.free < end) {
                self.free += 3;
                const disambiguator = disambiguate(@ptrCast([*]Code,&self.methods)[self.free-3..self.free],existing,cmp);
                address.* = disambiguator;
                return;
            }
        }
        return error.Conflict;
    }
    const bitTests = [_]ThreadedFn{
        &bitTest0,&bitTest1,&bitTest2,&bitTest3,&bitTest4,&bitTest5,
        &bitTest6,&bitTest7,&bitTest8,&bitTest9,&bitTest10,&bitTest11,
        &bitTest12,&bitTest13,&bitTest14,&bitTest15,&bitTest16,&bitTest17,
        &bitTest18,&bitTest19,&bitTest20,&bitTest21,&bitTest22,&bitTest23,
        &bitTest24,&bitTest25,&bitTest26,&bitTest27,&bitTest28,&bitTest29,
        &bitTest30,&bitTest31,
    };
    fn bitTest0(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<0==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest1(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<1==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest2(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<2==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest3(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<3==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest4(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<4==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest5(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<5==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest6(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<6==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest7(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<7==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest8(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<8==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest9(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<9==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest10(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<10==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest11(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<11==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest12(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<12==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest13(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<13==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest14(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<14==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest15(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<15==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest16(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<16==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest17(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<17==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest18(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<18==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest19(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<19==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest20(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<20==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest21(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<21==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest22(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<22==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest23(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<23==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest24(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<24==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest25(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<25==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest26(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<26==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest27(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<27==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest28(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<28==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest29(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<29==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest30(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<30==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn bitTest31(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = (if (selector.hash32()&1<<31==0) programCounter[0] else programCounter[1]).codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    const primes  = [_]?ThreadedFn{null,null,null,&prime3,null,&prime5};
    fn prime3(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = programCounter[(preHash(selector.hash32())*3)>>32].codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn prime5(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        const pc = programCounter[(preHash(selector.hash32())*5)>>32].codeRef;
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,selector});
    }
    fn super(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        _ = .{programCounter, sp, process, context, selector};
        @panic("called super function");
    }
    fn fail(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        _ = .{programCounter, sp, process, context, selector};
        if (programCounter[0].uint==0)
            @panic("called fail function");
        @panic("fail with non-zero next");
    }
    fn dnu(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        _ = .{programCounter, sp, process, context, selector};
        @panic("called dnu function");
    }
    fn testIncrement(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
        _ = .{process, context, selector};
        @intToPtr(*usize,programCounter[0].uint).* += 1;
        return sp;
    }
};
test "disambiguate" {
    const ee = std.testing.expectEqual;
    const fns = struct {
        fn push1(_: [*]const Code, sp: [*]Object, _: *Process, _: CodeContextPtr, _: Object) MethodReturns {
            const newSp = sp - 1;
            newSp[0] = Object.from(1);
            return newSp;
        }
        fn push2(_: [*]const Code, sp: [*]Object, _: *Process, _: CodeContextPtr, _: Object) MethodReturns {
            const newSp = sp - 1;
            newSp[0] = Object.from(2);
            return newSp;
        }
        fn push3(_: [*]const Code, sp: [*]Object, _: *Process, _: CodeContextPtr, _: Object) MethodReturns {
            const newSp = sp - 1;
            newSp[0] = Object.from(3);
            return newSp;
        }
    };
    // value=01101 yourself=00001 @"<="=11101
    const method1 = compileMethod(symbols.value,0,0,.{&fns.push1,&Code.end});
    const method2 = compileMethod(symbols.yourself,0,0,.{&fns.push2,&Code.end});
    const method3 = compileMethod(symbols.@"<=",0,0,.{&fns.push3,&Code.end});
    var space = [_]Code{Code.object(Nil),Code.object(Nil),Code.object(Nil)};
    var dispatcher = Dispatch.disambiguate(&space,method1.asCompiledMethodPtr(),method2.asCompiledMethodPtr());
    const push1Code = @ptrCast([*]Code,&method1.asCompiledMethodPtr().code);
    const push2Code = @ptrCast([*]Code,&method2.asCompiledMethodPtr().code);
    try ee(space[2].codeRef,push1Code);
    try ee(space[1].codeRef,push2Code);
    dispatcher = Dispatch.disambiguate(&space,method2.asCompiledMethodPtr(),method1.asCompiledMethodPtr());
    try ee(space[2].codeRef,push1Code);
    try ee(space[1].codeRef,push2Code);
    var process = Process.new();
    process.init();
    defer process.deinit();
    var context = Context.init();
    const sp = process.endOfStack();
    try ee(dispatcher[0].prim(dispatcher+1,sp,&process,&context,symbols.value)[0].to(i64),1);
    try ee(dispatcher[0].prim(dispatcher+1,sp,&process,&context,symbols.yourself)[0].to(i64),2);
    try ee(dispatcher[0].prim,&Dispatch.bitTest2);
    dispatcher = Dispatch.disambiguate(&space,method3.asCompiledMethodPtr(),method1.asCompiledMethodPtr());
    try ee(dispatcher[0].prim(dispatcher+1,sp,&process,&context,symbols.@"<=")[0].to(i64),3);
    try ee(dispatcher[0].prim(dispatcher+1,sp,&process,&context,symbols.value)[0].to(i64),1);
    try ee(dispatcher[0].prim,&Dispatch.bitTest4);
}
test "isExternalCompiledMethod" {
    const e = std.testing.expect;
    var d: Dispatch = undefined;
    try e(d.isExternalCompiledMethod(&Dispatch.bitTest0));
    d = Dispatch.new();
    try e(!d.isExternalCompiledMethod(&Dispatch.bitTest0));
    try e(!d.isExternalCompiledMethod(&Dispatch.bitTest31));
    try e(!d.isExternalCompiledMethod(&Dispatch.super));
    try e(d.isExternalCompiledMethod(&Dispatch.testIncrement));
}
test "empty dispatch" {
    const ee = std.testing.expectEqual;
    _ = Dispatch.new();
    const empty = Dispatch.empty;
    try ee(empty.lookupAddress(symbols.value),&empty.superOrDNUU);
}
test "add methods" {
    var temp0: usize = 0;
    var temp: usize = 0;
    const methodType = compiledMethodType(2);
    const fns = struct {
        fn testYourself(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
            _ = .{process, context};
            if (!selector.equals(symbols.yourself)) @panic("hash doesn't match");
            @intToPtr(*usize,programCounter[0].uint).* += 2;
            return sp;
        }
        fn testAt(programCounter: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns {
            _ = .{process, context};
            if (!selector.equals(symbols.@"at:")) @panic("hash doesn't match");
            @intToPtr(*usize,programCounter[0].uint).* += 4;
            return sp;
        }
    };
    var code0 = methodType.withCode(symbols.yourself,0,0,.{Code.prim(&fns.testYourself),Code.uint(@ptrToInt(&temp0))});
    var code1 = methodType.withCode(symbols.yourself,0,0,.{Code.prim(&fns.testYourself),Code.uint(@ptrToInt(&temp))});
    var code2 = methodType.withCode(symbols.@"at:",0,0,.{Code.prim(&fns.testAt),Code.uint(@ptrToInt(&temp))});
    var tE = TestExecution.new();
    tE.init();
    var dispatch = Dispatch.new();
    dispatch.initTest(&temp);
    try dispatch.add(code0.asCompiledMethodPtr());
    try dispatch.add(code1.asCompiledMethodPtr());
    _ = dispatch.dispatch(tE.sp,&tE.process,&tE.ctxt,symbols.yourself);
    try std.testing.expectEqual(temp,2);
    _ = dispatch.dispatch(tE.sp,&tE.process,&tE.ctxt,symbols.self); // invoke DNU
    try std.testing.expectEqual(temp,3);
    try dispatch.add(code2.asCompiledMethodPtr());
    _ = dispatch.dispatch(tE.sp,&tE.process,&tE.ctxt,symbols.yourself);
    try std.testing.expectEqual(temp,5);
    _ = dispatch.dispatch(tE.sp,&tE.process,&tE.ctxt,symbols.@"at:");
    try std.testing.expectEqual(temp,9);
    try std.testing.expectEqual(dispatch.add(code2.asCompiledMethodPtr()),error.Conflict);
    // for (1..40) |n| {
    //     std.debug.print("{} = {} {}\n",.{symbol.symbol1(n),(Dispatch.preHash(symbol.symbol0(n).hash32())*%@as(u64,13))>>32,(Dispatch.preHash(symbol.symbol1(n).hash32())*%@as(u64,13))>>32});
    // }
}
pub const DispatchPtr = *Dispatch;
const ClassDispatch = extern struct {
    dispatch: ?*Dispatch,
    hash: u32,
    const Self = @This();
    const init:Self =  .{.dispatch = null,.hash=0};
    fn store(self:*Self,value:Self) void {
        @atomicStore(u128, @ptrCast(*u128,self), @bitCast(u128,value), std.builtin.AtomicOrder.Unordered);
    }
};
//test "128-bit atomic store" {
//    var foo = ClassDispatch.init;
//    const bar = ClassDispatch {.dispatch = null,.hash=0};
//    foo.store(bar);
//    try std.testing.expectEqual(foo,bar);
//}
//var classDispatch : [max_classes]ClassDispatch = undefined;
inline fn bumpSize(size:u16) u16 {
    return size*2;
}
inline fn initialSize(size:usize) u16 {
    return @import("utilities.zig").largerPowerOf2(@max(@intCast(u16,size),4));
}
const Fix = struct {index:u16,a:u16,b:u16,c:u16};
const CF = struct{size:u16,hash:u32};
const WC = struct{size:u16,hash:u32,fix:[]Fix};
const TableStructureResult = union(enum) {
    conflictFree: CF,
    withConflicts: WC,
    const Self = TableStructureResult;
    inline fn hash(self: Self) u32 {
        return switch (self) {
            .conflictFree => |cf| cf.hash,
            .withConflicts => |wc| wc.hash,
        };
    }
    inline fn size(self: Self) u16 {
        return switch (self) {
            .conflictFree => |cf| cf.size,
            .withConflicts => |wc| wc.size,
        };
    }
};
fn stage2a(process : *Process, self: Object, selector: Object, ci:ClassIndex) MethodReturns {
    //    testNormal(_: Object, self: Object, _: Object, _ : *Context, null, null);
    _ = .{process,self,selector,ci};
    @panic("stage2a");
}
// fn DispatchMethods(comptime T: type, comptime extractHash: fn(T) u32, comptime maxSize: comptime_int) type {
//     return struct{
//         const Self = @This();
//         fn findTableSize(sm: []const T, extra: ?T,fix: *[15]Fix) !TableStructureResult {
//             var minSizeConflicts: u32 = maxSize;
//             var conflictSize: u16 = 0;
//             var bestConflictRand: u32 = 0;
//             var used : [maxSize]u8 = undefined;
//             var size = initialSize(sm.len);
//             const limitSize = @min(@max(initialSize(sm.len*4),17),maxSize);
//             while (size<limitSize) : (size = bumpSize(size)) {
//                 var minConflicts: u32 = maxSize;
//                 var bestRand: u32 = 0;
//                 for (1..66) |tries| {
//                     const rand = tries *% u32_phi_inverse & ~@as(u32,31) | @truncate(u5,@clz(@as(u32,size))+1);
//                     for (used[0..size]) |*b| b.* = 0;
//                     if (extra) |key| {
//                         const hash = extractHash(key) *% rand >> @truncate(u5,rand);
//                         used[hash] = 1;
//                     }
//                     for (sm) |key| {
//                         const hash = extractHash(key) *% rand >> @truncate(u5,rand);
//                         used[hash] += 1;
//                     }
//                     var conflicts: u32 = 0;
//                     for (used[0..size]) |count| {
//                         if (count>1) conflicts += count;
//                     }
//                     if (conflicts>0) {
//                         if (minConflicts > conflicts) {
//                             minConflicts = conflicts;
//                             bestRand = rand;
//                         }
//                     } else {
//                         std.debug.print("table of size {} is {}% efficient with rand={} on try {}\n",.{sm.len,sm.len*100/size,rand,tries});
//                         return TableStructureResult{.conflictFree=.{.size=size,.hash=rand}};
//                     }
//                 }
//                 if (minSizeConflicts > minConflicts) {
//                     minSizeConflicts = minConflicts;
//                     conflictSize = size;
//                     bestConflictRand = bestRand;
//                 }
//             }
//             for (used[0..conflictSize]) |*b| b.* = 0;
//             for (sm) |key| {
//                 const hash = extractHash(key) *% bestConflictRand >> @truncate(u5,bestConflictRand);
//                 used[hash] += 1;
//                 if (used[hash]>3) return error.moreThan3WayConflict;
//             }
//             var i: u8 = 0;
//             var conflicts: u8=0;
//             var level2: u16 = 1;
//             for (sm) |key,index| {
//                 const hash = extractHash(key) *% bestConflictRand >> @truncate(u5,bestConflictRand);
//                 switch (used[hash]) {
//                     0,1 => {},
//                     2,3 => |s| {
//                         conflicts += s-1;
//                         if (conflicts>15) return error.moreThan15Conflicts;
//                         fix[i]=Fix{.index=@truncate(u16,hash),
//                                    .a=@truncate(u16,index),
//                                    .b=0,
//                                    .c=0,};
//                         used[hash] = i+16;
//                         i += 1;
//                         level2 += (s+1)&0xFE;
//                     },
//                     else => |s| {
//                         switch (s>>4) {
//                             1 => fix[s&15].b = @truncate(u16,index),
//                             else => fix[s&15].c = @truncate(u16,index),
//                         }
//                         used[hash] += 16;
//                     },
//                 }
//             }
//             const fixup = fix[0..i];
//             std.debug.print("table of size {}({}) has {} conflicts ({any})({}) with rand=0x{x:8>0}\n",.{conflictSize,sm.len,minSizeConflicts,fixup,i,bestConflictRand});
//             return TableStructureResult{.withConflicts=.{.size=conflictSize+level2,.hash=bestConflictRand,.fix=fixup}};
//         }
//         fn addDispatch(_: *Process, theClass: ClassIndex, superClass: ClassIndex, symbolMethods: []const CompiledMethodPtr) void {
//             var fixup: [15]Fix = undefined;
//             const dispatchSize = Self.findTableSize(symbolMethods,null,&fixup) catch @panic("dispatch conflicts");
//             const rand = dispatchSize.hash();
//             const size = 0;//dispatchSize.size();
//             const strct = arena.allocStruct(class.Dispatch_I,@sizeOf(Dispatch)+size*@sizeOf(CompiledMethodPtr),Dispatch,@bitCast(Object,@ptrToInt(dnu)),8) catch unreachable;
//             strct.hash = rand;
//             strct.super = superClass;
//             const methods=@ptrCast([*]CompiledMethodPtr,@alignCast(@alignOf([*]CompiledMethodPtr),&strct.methods));
//             for (symbolMethods) |*sm| {
//                 const hash = sm.selector.hash *% rand >> @truncate(u5,rand);
//                 methods[hash] = sm.method;
//             }
//             switch (dispatchSize) {
//                 .conflictFree => {},
//                 .withConflicts => |wc| {
//                     //var next = wc.size;
//                     const fix = wc.fix;
//                     var i: u8 = 0;
//                     //var bitTests: u64 = 0;
//                     var shift: u6 = 0;
//                     while (i<fix.len) : (shift+=1) {
//                         switch (fix.ptr[i+1]) {
//                         //     2 => {
//                         //         const left = fix.ptr[i+2];
//                         //         const right = fix.ptr[i+3];
//                         //         const xor = symbolMethods[left].selector.hash^symbolMethods[right].selector.hash;
//                         //         const sh = @ctz(u5,xor);
//                         //         bitTests |= sh << shift45;
//                         //         if (((fix.ptr[i+2]>>sh)&1)==0) {
//                         //             methods[next] = symbolMethods[left].method;
//                         //             methods[next+1] = symbolMethods[right].method;
//                         //         } else {
//                         //             methods[next] = symbolMethods[right].method;
//                         //             methods[next+1] = symbolMethods[left].method;
//                         //         }
//                         //         next += 2;
//                         //         shift += 1;
//                         //     },
//                             else => @panic("Not implemented"),
//                         }
//                         //methods[wc.hash]=stage2[shift];
//                     }
//                 },
//             }
//             classDispatch[theClass]=strct;
//         }
//     };
// }

// fn id_u32(x:u32) u32 {return x;}
// test "tablesize" {
//     const e1 = [_]u32{6, 518, 38, 2};
//     const e2 = [_]u32{6, 518, 38, 2, 7, 5};
//     const e3 = [_]u32{6, 518, 38, 2, 7, 8, 9, 10, 42, 46, 47};
//     const e4 = [_]u32{6, 518, 38, 2, 7, 8, 9, 10, 42, 46, 47,
//                       4518, 438, 49, 410, 442, 446, 447};
//     const e5 = [_]u32{6, 518, 38, 2, 7, 8, 9, 10, 42, 46, 47,
//                       4518, 438, 49, 410, 442, 446, 447,
//                       36, 3518, 338, 32, 37, 39, 310, 342, 346, 347,
//                       26, 2518, 238, 22, 27, 28, 29, 210, 242, 246, 247,
//                       16, 1518, 138, 12, 17, 18, 19, 110, 142, 146, 147};
//     const stdout = std.io.getStdOut().writer();
//     var fix: [15]Fix = undefined;
//     {
//         const findTableSize2=DispatchMethods(u32,id_u32,35).findTableSize;
//         try stdout.print("\n",.{});
//         try stdout.print("e1: {any}\n",.{try findTableSize2(e1[0..],null,&fix)});
//         try stdout.print("e2: {any}\n",.{try findTableSize2(e2[0..],null,&fix)});
//         try stdout.print("e3: {any}\n",.{try findTableSize2(e3[0..],null,&fix)});
//         try stdout.print("e4: {any}\n",.{try findTableSize2(e4[0..],null,&fix)});
//     }
//     {
//         const findTableSize2=DispatchMethods(u32,id_u32,128).findTableSize;
//         try stdout.print("e5: {any}\n",.{try findTableSize2(e5[0..],null,&fix)});
//     }
//     {
//         const findTableSize2=DispatchMethods(u32,id_u32,17).findTableSize;
//         var e6: [15]u32 = undefined;
//         for (e6) |*v,i| v.*=@truncate(u32,i);
//         try stdout.print("e6: {any}\n",.{try findTableSize2(e6[0..],null,&fix)});
//     }
// }
// fn id_cm(x:CompiledMethodPtr) u32 {return x.selector.hash32();}
// const dispatch=DispatchMethods(CompiledMethodPtr,id_cm,2050);

// // test "timing" {
// //     const stdout = @import("std").io.getStdOut().writer();
// //     const findTableSize=dispatch.findTableSize;
// //     var fix: [12]Fix = undefined;
// //     try stdout.print("methods1: {any}\n",.{(try findTableSize(symbolMethods1[0..],null,&fix))});
// //     try stdout.print("methods2: {any}\n",.{(try findTableSize(symbolMethods2[0..],null,&fix))});
// //     try stdout.print("methods3: {any}\n",.{(try findTableSize(symbolMethods3[0..],null,&fix))});

// // }
// // test "findTableSize" {
// //     const expectEqual = @import("std").testing.expectEqual;
// //     const findTableSize=dispatch.findTableSize;
// //     var fix: [12]Fix = undefined;
// //     try expectEqual((try findTableSize(symbolMethods1[0..],null,&fix)).size(),4);
// //     try expectEqual((try findTableSize(symbolMethods2[0..],null,&fix)).size(),8);
// //     try expectEqual((try findTableSize(symbolMethods3[0..],null,&fix)).size(),16);
// // }
// pub fn addClass(process: *Process, className: Object, instanceMethods: []const CompiledMethodPtr, classMethods: []const CompiledMethodPtr) !void {
//     const theClass_I = 42;_=className;//class.getClassIndex(className);
//     const superClass = 0;
//     const theMetaclass_I = 0;
//     dispatch.addDispatch(process, theClass_I, superClass, instanceMethods);
//     dispatch.addDispatch(process, theMetaclass_I, superClass, classMethods);
//     return error.UnImplemented;
// }
// pub inline fn call(selector: Object, self: Object, other: Object, cp: *Context) MethodReturns {
//     const theClass = self.get_class();
//     return callClass(selector, self, other, cp, theClass);
// }
// pub inline fn callClass(selector: Object, self: Object, other: Object, cp: *Context, theClass: ClassIndex) MethodReturns {
//     @setRuntimeSafety(false);
//     const disp = classDispatch[theClass];
//     const rand = disp.hash;
//     const index = selector.hash *% rand >> @truncate(u5,rand);
//     return disp.methods[index](selector, self, other, cp, disp, null);
// }
// fn dispatchTableObject(classIndex: ClassIndex) HeapPtr {
//     return @ptrCast(HeapPtr,@alignCast(@alignOf(HeapPtr),classDispatch[classIndex]));
// }
// test "addClass and call" {
// //    const expectEqual = @import("std").testing.expectEqual;
//     var process = Process.new();
//     process.init();
// //    _ = try symbol.init(&process,250,"");
//     var noMethods = [0]CompiledMethodPtr{};
//     try class.init();
//     try addClass(&process,symbols.SmallInteger,&noMethods,&noMethods);
// //     const t42 = Object.from(42);
// //     try expectEqual(t42.send(symbols.value,Nil,undefined),MethodReturns{.Normal=Nil});
// }
// // test "lookups of proper methods" {
// //     const expectEqual = @import("std").testing.expectEqual;
// //     var process = try Process.initForTest(null);
// //     _ = try symbol.init(&process,250,"");
// //     try class.init(&process);
// //     try addClass(&process,symbols.SmallInteger,symbolMethods2[0..],noMethods);
// //     const t42 = Object.from(42);
// // //    process.push(Object.from(17));
// //     try expectEqual(t42.send(symbols.value,Nil,undefined),MethodReturns{.Normal=Object.from(1)});
// //     try expectEqual(t42.send(symbols.self,Nil,undefined),MethodReturns{.Normal=Object.from(2)});
// //     try expectEqual(t42.send(symbols.yourself,Nil,undefined),MethodReturns{.Normal=Object.from(3)});
// //     try expectEqual(t42.send(symbols.@"cull:",Nil,undefined),MethodReturns{.Normal=Object.from(4)});
// //     try expectEqual(t42.send(symbols.@"value:",Nil,undefined),MethodReturns{.Normal=Object.from(5)});
// //     try expectEqual(t42.send(symbols.@"cull:cull:cull:cull:",Nil,undefined),MethodReturns{.Normal=Object.from(6)});
// // }
