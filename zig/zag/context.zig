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
pub const MethodReturns = void;
pub fn Context(comptime codeType: type, comptime compiledMethodPtr: type) type {
    return extern struct {
    header: heap.Header,
    tpc: [*]const codeType, // threaded PC
    npc: * const fn(programCounter: [*]const codeType, stackPointer: [*]Object, heapPointer: Hp, thread: *Thread, context: ContextPtr) MethodReturns, // native PC - in Continuation Passing Style
    size: u64,
    prevCtxt: ContextPtr,
    method: compiledMethodPtr,
    temps: [1]Object,
    const Self = @This();
    const ContextPtr = *Self;
    pub const ThreadedFn = * const fn(programCounter: [*]const codeType, stackPointer: [*]Object, heapPointer: Hp, thread: *Thread, context: ContextPtr) MethodReturns;
    const baseSize = @sizeOf(Self)/@sizeOf(Object) - 1;
    fn init(method: compiledMethodPtr) Self {
        return Self {
            .header = heap.header(2,Format.both,class.Context_I,@truncate(u24,@ptrToInt(method)),Age.static),
            .tpc = undefined,
            .npc = undefined,
            .size = 2,
            .prevCtxt = undefined, //@intToPtr(ContextPtr,@bitCast(u64,Nil)),
            .method = undefined, //Object.from(method),
            .temps = undefined,
        };
    }
    pub inline fn pop(self: ContextPtr, thread: *Thread, itemsToDiscard: usize) struct {
        sp: [*]Object,
        ctxt: ContextPtr,
    } {
        if (self.isInStack())
            return .{.sp=self.asObjectPtr() + baseSize + itemsToDiscard,.ctxt=self.prevCtxt};
        const itemsToKeep = self.temps[itemsToDiscard..];
        const newSp = thread.endOfStack() - itemsToKeep.len;
        for (itemsToKeep) | obj,index | {
            newSp[index] = obj;
        }
        return .{.sp=newSp,.ctxt=self.previous()};
    }
    pub inline fn push(self: ContextPtr, sp: [*]Object, hp: Hp, thread: *Thread, method: compiledMethodPtr, locals: usize, maxStackNeeded: usize)  struct {
        hp: Hp,
        ctxt: ContextPtr,
    } {
        const newSp = sp - baseSize - locals;
        if (arenas.arenaFree(newSp,hp)<5+maxStackNeeded) @panic("grow heap1");
        const ctxt = @ptrCast(ContextPtr,@alignCast(@alignOf(Self),newSp));
        ctxt.prevCtxt = self;
        ctxt.method = method;
        for (ctxt.temps[0..locals]) |*local| {local.*=Nil;}
        ctxt.header = heap.Header.partialOnStack;
        if (thread.needsCheck()) @panic("grow heap2");
        return .{.hp=hp,.ctxt=ctxt};
    }
    fn convertToProperHeapObject(self: ContextPtr, sp: [*]Object, thread: *Thread) void {
        if (self.isInStack(sp,thread)) {
            self.header = heap.header(2, Format.object, class.Self_I,0,Age.stack);
            self.size = (@ptrToInt(self.prevCtxt)-@ptrToInt(self))/@sizeOf(Object)-5;
            std.debug.print("obj = 0x{x:0>16}\n",.{Object.from(self.prevCtxt).u()});
            self.prevCtxt = @intToPtr(ContextPtr,Object.from(self.prevCtxt).u());
            self.method = @intToPtr(compiledMethodPtr,Object.from(self.method).u());
            self.prevCtxt.convertToProperHeapObject(sp, thread);
        }
    }
    pub inline fn allTemps(self: ContextPtr) []Object {
        @setRuntimeSafety(false);
        return @ptrCast(heap.HeapConstPtr,self).indexables(Object);
    }
    pub inline fn getTPc(self: ContextPtr) [*]const codeType {
        return self.tpc;
    }
    pub inline fn setTPc(self: ContextPtr, pc: [*]const codeType) void {
        self.tpc = pc;
    }
    pub inline fn getNPc(self: ContextPtr) ThreadedFn {
        return self.npc;
    }
    pub inline fn setNPc(self: ContextPtr, pc: ThreadedFn) void {
        self.npc = pc;
    }
    pub inline fn isInStack(self: ContextPtr) bool {
        return @alignCast(8,&self.header).isInStack();
    }
    pub inline fn getTemp(self: ContextPtr, n: usize) Object {
        @setRuntimeSafety(false);
        return self.temps[n];
    }
    pub inline fn setTemp(self: ContextPtr, n: usize, v: Object) void {
        @setRuntimeSafety(false);
        self.temps[n] = v;
    }
    pub inline fn previous(self: ContextPtr) ContextPtr {
        return self.prevCtxt;
    }
    pub inline fn asHeapPtr(self : ContextPtr) HeapPtr {
        return @ptrCast(HeapPtr,self);
    }
    pub inline fn asObjectPtr(self : ContextPtr) [*]Object {
        return @ptrCast([*]Object,self);
    }
    inline fn fromObjectPtr(op: [*]Object) ContextPtr {
        return @ptrCast(ContextPtr,op);
    }
    fn size(self: ContextPtr, sp: [*]Object, thread: *Thread) usize {
        return ((if (self.isInStack(sp,thread))
                     @ptrToInt(self)
                     else
                     @ptrToInt(thread.endOfStack()))-@ptrToInt(sp))/@sizeOf(Object);
    }
    fn collectNursery(pc: [*]const codeType, sp: [*]Object, hp: Hp, thread: *Thread, context: ContextPtr) MethodReturns {
        if (true) @panic("need to collect nursery");
        return @call(tailCall,push,.{pc,sp,hp,thread,context});
    }
    fn print(self: ContextPtr)void{ //,sp: [*]Object, thread: *Thread) void {
        const pr = std.debug.print;
        pr("Self: {} {any}\n",.{self.header,self.allTemps()});
        //        if (self.prevCtxt) |ctxt| {ctxt.print(sp,thread);}
    }
};
}
pub fn TestExecution(comptime codeType: type, comptime compiledMethod: type, comptime executor: * const fn(programCounter: [*]const codeType, stackPointer: [*]Object, heapPointer: Hp, thread: *Thread, context: *Context(codeType,*compiledMethod)) MethodReturns) type {
    return struct {
    thread: Thread,
    ctxt: contextType,
    sp: [*]Object,
    hp: Hp,
    pc: [*] const codeType,
    const Self = @This();
    const contextType = Context(codeType,compiledMethodPtr);
    const compiledMethodPtr = *compiledMethod;
    var endSp: [*]Object = undefined;
    var endHp: Hp = undefined;
    var endPc: [*] const codeType = undefined;
    var baseMethod = compiledMethod.init(Nil,0);
    pub fn new() Self {
        return Self {
            .thread = Thread.new(),
            .ctxt = contextType.init(&baseMethod),
            .sp = undefined,
            .hp = undefined,
            .pc = undefined,
        };
    }
    pub fn init(self: *Self) void {
        self.thread.init();
    }
    fn end(pc: [*]const codeType, sp: [*]Object, hp: Hp, _: *Thread, _: * contextType) void {
        endPc = pc;
        endHp = hp;
        endSp = sp;
    }
    pub fn run(self: *Self, source: [] const Object, method: compiledMethodPtr) []Object {
        const sp = self.thread.endOfStack() - source.len;
        for (source) |src,idx|
            sp[idx] = src;
        const pc = method.codePtr();
        const hp = self.thread.getHeap();
        self.ctxt.setNPc(Self.end);
        endSp = sp;
        endHp = hp;
        endPc = pc;
        executor(pc,sp,hp,&self.thread,&self.ctxt);
        self.sp = endSp;
        self.hp = endHp;
        self.pc = endPc;
        return self.thread.stack(self.sp);
    }
    };
}
// test "init context" {
//     const expectEqual = std.testing.expectEqual;
// //    const objs = comptime [_]Object{True,Object.from(42)};
//     var result = TestExecution.init();
//     const c = result.context;
//     c.print();
//     try expectEqual(result.o()[3].u(),4);
//     try expectEqual(result.o()[6],True);
//     var thread = result.thread;
//     var sp = thread.endOfStack();
//     c.convertToProperHeapObject(sp, &thread);
//     c.print();
// }
