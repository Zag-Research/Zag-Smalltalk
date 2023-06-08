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
const class = @import("class.zig");
const execute = @import("execute.zig");
const trace = execute.trace;
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
    nonLocalLink: ContextPtr, // note this is not an Object, so access and GC need to handle specially
    method: CompiledMethodPtr, // note this is not an Object, so access and GC need to handle specially
    temps: [nTemps]Object,
    const Self = @This();
    const ThreadedFn = * const fn(programCounter: [*]const Code, stackPointer: [*]Object, process: *Process, context: ContextPtr, selector: Object) MethodReturns;
    const nTemps = 1;
    const baseSize = @sizeOf(Self)/@sizeOf(Object) - nTemps;
    pub fn init() Self {
        return Self {
            .header = comptime heap.footer(baseSize+nTemps,Format.header,class.Context_I,0,Age.static),
            .tpc = undefined,
            .npc = undefined,
            .prevCtxt = undefined,
            .nonLocalLink = undefined,
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
        try writer.print(" prev: 0x{x}",.{@ptrToInt(self.prevCtxt)});
        if (false) {
            @setRuntimeSafety(false);
            try writer.print(" temps: {any}",.{self.temps[0..self.size]});
        }
    }
    pub inline fn pop(self: *Context, process: *Process) struct { sp: [*]Object,ctxt: *const Context } {
        const wordsToDiscard = self.header.hash16();
        if (self.isOnStack())
            return .{.sp=self.asObjectPtr() + wordsToDiscard,.ctxt=self.prevCtxt};
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
        var contextMutable = self;
        const newSp = process.allocStack(sp,baseSize + locals + maxStackNeeded,&contextMutable)+maxStackNeeded;
        const ctxt = @ptrCast(ContextPtr,@alignCast(@alignOf(Self),newSp));
        ctxt.prevCtxt = contextMutable;
        ctxt.method = method;
        { @setRuntimeSafety(false);
         for (ctxt.temps[0..locals]) |*local| {local.*=Nil;}
         }
        ctxt.header = HeapObject.partialOnStack(baseSize+selfOffset);
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
        return (@ptrToInt(self.prevCtxt.endOfStack(process))-@ptrToInt(&self.temps))/@sizeOf(Object);
    }
    pub inline fn stack(self: *const Self, sp: [*]Object, process: *Process) []Object {
        return sp[0..(@ptrToInt(self.endOfStack(process))-@ptrToInt(sp))/@sizeOf(Object)];
    }
    pub inline fn allTemps(self: *const Context, process: *const Process) []Object {
        const size = self.tempSize(process);
        @setRuntimeSafety(false);
        return @constCast(self.temps[0..size]);
    }
    pub inline fn getTPc(self: *const Context) [*]const Code {
        return self.tpc;
    }
    pub inline fn setReturn(self: ContextPtr, tpc: [*]const Code) void {
        self.npc = tpc[0].prim;
        self.tpc = tpc+1;
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
    pub inline fn getTemp(self: *const Context, n: usize) Object {
        @setRuntimeSafety(false);
        return self.temps[n];
    }
    pub inline fn setTemp(self: ContextPtr, n: usize, v: Object) void {
        @setRuntimeSafety(false);
        self.temps[n] = v;
    }
    pub inline fn previous(self: *const Context) ContextPtr {
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
        pr("Context: {*} {} {any}\n",.{self,self.header,self.allTemps(process)});
        //        if (self.prevCtxt) |ctxt| {ctxt.print(sp,process);}
    }
    pub fn call(oldPc: [*]const Code, sp: [*]Object, process: *Process, self: ContextPtr, selector: Object) [*]Object {
        self.tpc = oldPc+1;
        self.npc = oldPc[0].prim;
        trace("\ncall: N={*} T={*} {any}",.{self.getNPc(),self.getTPc(),self.stack(sp,process)});
        const method = @intToPtr(CompiledMethodPtr,@bitCast(u64,selector));
        const pc = @ptrCast([*]const Code,&method.code);
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,self,method.selector});
    }
};
