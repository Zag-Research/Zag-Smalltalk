const std = @import("std");
const checkEqual = @import("utilities.zig").checkEqual;
const Process = @import("process.zig").Process;
const object = @import("zobject.zig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const heap = @import("heap.zig");
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObject = heap.HeapObject;
const Format = heap.Format;
const Age = heap.Age;
//const class = @import("class.zig");
const execute = @import("execute.zig");
const trace = execute.trace;
const TestExecution = execute.TestExecution;
const Code = execute.Code;
const tailCall = execute.tailCall;
const CompiledMethodPtr = execute.CompiledMethodPtr;
const MethodReturns = [*]Object;
pub const ContextPtr = *Context;
pub var nullContext = Context.init();
pub const Context = struct {
    header: HeapObject, // if not on stack there is also a footer
    tpc: [*]const Code, // threaded PC
    npc: ThreadedFn, // native PC - in Continuation Passing Style
    prevCtxt: ContextPtr, // note this is not an Object, so access and GC need to handle specially
    trapContextNumber: u64,
    method: CompiledMethodPtr, // note this is not an Object, so access and GC need to handle specially
    temps: [nLocals]Object,
    const Self = @This();
    const ThreadedFn = * const fn(programCounter: [*]const Code, stackPointer: [*]Object, process: *Process, context: ContextPtr, selector: Object) MethodReturns;
    const nLocals = 1;
    const baseSize = @sizeOf(Self)/@sizeOf(Object) - nLocals;
    pub fn init() Self {
        return Self {
            .header = comptime heap.footer(baseSize+nLocals,Format.header,object.Context_I,0,Age.static),
            .tpc = undefined,
            .npc = Code.end,
            .prevCtxt = undefined,
            .trapContextNumber = undefined,
            .method = undefined,
            .temps = undefined,
        };
    }
    pub fn format(
        self: *const Context,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        
        try writer.print("context: {}",.{self.header});
        try writer.print(" prev: 0x{x}",.{@intFromPtr(self.previous())});
        if (false) {
            @setRuntimeSafety(false);
            try writer.print(" temps: {any}",.{self.temps[0..self.size]});
        }
    }
    pub inline fn pop(self: *Context, process: *Process) struct { sp: [*]Object,ctxt: ContextPtr } {
        const wordsToDiscard = self.header.hash16();
        if (self.isOnStack())
            return .{.sp=self.asObjectPtr() + wordsToDiscard,.ctxt=self.previous()};
        _ = process;@panic("incomplete");
        // const itemsToKeep = self.temps[wordsToDiscard-baseSize..self.size];
        // const newSp = process.endOfStack() - itemsToKeep.len;
        // for (itemsToKeep,0..) | obj,index | {
        //     newSp[index] = obj;
        // }
        // return .{.sp=newSp,.ctxt=self.previous()};
    }
    pub fn pushStatic(self: *const Context, sp: [*]Object, process: *Process, method: CompiledMethodPtr, locals: u16, maxStackNeeded: u16, selfOffset: u16)  ContextPtr {
        return self.push(sp, process, method, locals, maxStackNeeded, selfOffset);
    }
    pub inline fn push(self: * Context, sp: [*]Object, process: *Process, method: CompiledMethodPtr, locals: u16, maxStackNeeded: u16, selfOffset: u16)  ContextPtr {
        if (@intFromPtr(self)==0) @panic("0 self");
        var contextMutable = self;
        const newSp = process.allocStack(sp,baseSize + locals + maxStackNeeded,&contextMutable)+maxStackNeeded;
        trace("\npush: {} {} {}",.{baseSize , locals, maxStackNeeded});
        trace("\npush: sp={*} newSp={*}",.{sp,newSp});
        const ctxt = @ptrCast(ContextPtr,@alignCast(@alignOf(Self),newSp));
        ctxt.prevCtxt = contextMutable;
        ctxt.trapContextNumber = process.trapContextNumber;
        ctxt.method = method;
        { @setRuntimeSafety(false);
         for (ctxt.temps[0..locals]) |*local| {local.*=Nil;}
         }
        ctxt.header = HeapObject.partialHeaderOnStack(baseSize+selfOffset);
        if (process.needsCheck()) @panic("process needsCheck");
        return ctxt;
    }
    pub fn moveToHeap(self: *const Context, sp: [*]Object, process: *Process) ContextPtr {
        _=self;_=sp;_=process;unreachable;
        // if (self.isIncomplete()) {
        //     self.header = heap.header(4, Format.bothAP, class.Context_I,0,Age.stack);
        //     self.size = self.prevCtxt.calculatedSize(process);
        //     self.addr = @ptrCast(*Object,&self.temps);
        //     self.prevCtxt.convertToProperHeapObject(sp, process);
        // }
    }
    pub inline fn isOnStack(self: * const Self) bool {
        return @alignCast(8,&self.header).isOnStack();
    }
    inline fn endOfStack(self: *const Context, process: *const Process) [*]Object {
        return if (self.isOnStack()) self.asObjectPtr() else process.endOfStack();
    }
    inline fn tempSize(self: *const Context, process: *const Process) usize {
        return (@intFromPtr(self.previous().endOfStack(process))-@intFromPtr(&self.temps))/@sizeOf(Object);
    }
    pub  fn stack(self: *const Self, sp: [*]Object, process: *Process) []Object {
        return sp[0..(@intFromPtr(self.endOfStack(process))-@intFromPtr(sp))/@sizeOf(Object)];
    }
    pub inline fn allLocals(self: *const Context, process: *const Process) []Object {
        const size = self.tempSize(process);
        @setRuntimeSafety(false);
        return @constCast(self.temps[0..size]);
    }
    pub inline fn getTPc(self: *const Context) [*]const Code {
        return self.tpc;
    }
    pub inline fn setReturnBoth(self: ContextPtr, npc: ThreadedFn, tpc: [*]const Code) void {
        self.npc = npc;
        self.tpc = tpc;
    }
    pub inline fn setReturn(self: ContextPtr, tpc: [*]const Code) void {
        self.setReturnBoth(tpc[0].prim,tpc+1);
    }
    pub inline fn getNPc(self: *const Context) Context.ThreadedFn {
        return self.npc;
    }
//    pub inline fn setNPc(self: *const Context, pc: Context.ThreadedFn) void {
//        self.npc = pc;
//    }
    pub inline fn getSelf(self: *const Context) Object {
        const wordsToDiscard = self.asHeapObjectPtr().hash16();
        return self.asObjectPtr()[wordsToDiscard];
    }
    pub inline fn getLocal(self: *const Context, n: usize) Object {
        @setRuntimeSafety(false);
        return self.temps[n];
    }
    pub inline fn setLocal(self: ContextPtr, n: usize, v: Object) void {
        @setRuntimeSafety(false);
        self.temps[n] = v;
    }
    pub inline fn previous(self: *const Context) ContextPtr {
        if (@intFromPtr(self.prevCtxt)==0) @panic("0 prev");
        return self.prevCtxt;
    }
    pub inline fn asHeapObjectPtr(self : *const Context) HeapObjectPtr {
        return &self.header;
    }
    pub inline fn asObjectPtr(self : *const Context) [*]Object {
        return @ptrCast([*]Object,@constCast(self));
    }
    inline fn fromObjectPtr(op: [*]Object) ContextPtr {
        return @ptrCast(ContextPtr,op);
    }
    pub fn print(self: *const Context, process: *const Process) void {
        const pr = std.debug.print;
        pr("Context: {*} {} {any}\n",.{self,self.header,self.allLocals(process)});
        //        if (self.prevCtxt) |ctxt| {ctxt.print(sp,process);}
    }
    pub fn call(oldPc: [*]const Code, sp: [*]Object, process: *Process, self: ContextPtr, selector: Object) [*]Object {
        self.tpc = oldPc+1;
        self.npc = oldPc[0].prim;
        trace("\ncall: N={*} T={*} {any}",.{self.getNPc(),self.getTPc(),self.stack(sp,process)});
        const method = @ptrFromInt(CompiledMethodPtr,@bitCast(u64,selector));
        const pc = @ptrCast([*]const Code,&method.code);
        _ = .{pc,oldPc,sp,process,selector};unreachable;
//        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,self,method.selector});
    }
};
const e = struct {
    usingnamespace execute.controlPrimitives;
};
// test "init context" {
// //    const expectEqual = std.testing.expectEqual;
// //    const objs = comptime [_]Object{True,Object.from(42)};
//     var result = TestExecution.new();
//     var c = result.ctxt;
//     var process = &result.process;
//     c.print(process);
// //    try expectEqual(result.o()[3].u(),4);
// //    try expectEqual(result.o()[6],True);
//     const sp = process.endOfStack();
//     const newC = c.moveToHeap(sp, process);
//     newC.print(process);
// }
test "init context" {
//    const expectEqual = std.testing.expectEqual;
//    const objs = comptime [_]Object{True,Object.from(42)};
    var result = TestExecution.new();
    var c = result.ctxt;
    var process = &result.process;
    c.print(process);
//    try expectEqual(result.o()[3].u(),4);
//    try expectEqual(result.o()[6],True);
    const sp = process.endOfStack();
    const newC = c.moveToHeap(sp, process);
    newC.print(process);
}
