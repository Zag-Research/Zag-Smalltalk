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
const MethodReturns = void;
pub const ContextPtr = *Context;
pub var nullContext = Context.init();
pub const Context = struct {
    header: HeapObject, // only used while on stack
    tpc: [*]const Code, // processed PC
    npc: ProcessedFn, // native PC - in Continuation Passing Style
    prevCtxt: ContextPtr, // note this is not an Object, so access and GC need to handle specially
    method: CompiledMethodPtr, // note this is not an Object, so access and GC need to handle specially
    temps: [nTemps]Object,
    const Self = @This();
    const ProcessedFn = * const fn(programCounter: [*]const Code, stackPointer: [*]Object, process: *Process, context: ContextPtr, selector: Object) MethodReturns;
    const nTemps = 1;
    const baseSize = @sizeOf(Self)/@sizeOf(Object) - nTemps;
    pub fn init() Self {
        return Self {
            .header = comptime heap.footer(baseSize+nTemps,Format.header,class.Context_I,0,Age.static),
            .tpc = undefined,
            .npc = undefined,
            .prevCtxt = undefined,
            .method = undefined,
            .temps = undefined,
        };
    }
    pub fn format(
        self: ContextPtr,
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
    pub inline fn pop(self: ContextPtr, process: *Process) struct { sp: [*]Object,ctxt: ContextPtr } {
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
    pub fn push(self: ContextPtr, sp: [*]Object, process: *Process, method: CompiledMethodPtr, locals: u16, maxStackNeeded: u16, selfOffset: u16)  ContextPtr {
        const newSp = sp - baseSize - locals;
        if (process.checkStack(newSp,self,maxStackNeeded)) |grow| return grow.context.push(grow.sp,grow.process,method,locals,maxStackNeeded,selfOffset);
        const ctxt = @ptrCast(ContextPtr,@alignCast(@alignOf(Self),newSp));
        ctxt.prevCtxt = self;
        ctxt.method = method;
        { @setRuntimeSafety(false);
         for (ctxt.temps[0..locals]) |*local| {local.*=Nil;}
         }
        ctxt.header = HeapObject.partialOnStack(selfOffset+baseSize);
        if (process.needsCheck()) @panic("process needsCheck");
        return ctxt;
    }
    pub fn moveToHeap(self: ContextPtr, sp: [*]Object, process: *Process) ContextPtr {
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
    inline fn endOfStack(self: ContextPtr, process: *Process) [*]Object {
        return if (self.isOnStack()) self.asObjectPtr() else process.endOfStack();
    }
    inline fn tempSize(self: ContextPtr, process: *Process) usize {
        return (@ptrToInt(self.prevCtxt.endOfStack(process))-@ptrToInt(&self.temps))/@sizeOf(Object);
    }
    pub inline fn stack(self: *Self, sp: [*]Object, process: *Process) []Object {
        return sp[0..(@ptrToInt(self.endOfStack(process))-@ptrToInt(sp))/@sizeOf(Object)];
    }
    pub inline fn allTemps(self: ContextPtr, process: *Process) []Object {
        const size = self.tempSize(process);
        @setRuntimeSafety(false);
        return self.temps[0..size];
    }
    pub inline fn getTPc(self: ContextPtr) [*]const Code {
        return self.tpc;
    }
    pub inline fn setTPc(self: ContextPtr, pc: [*]const Code) void {
        self.tpc = pc;
    }
    pub inline fn getNPc(self: ContextPtr) Context.ProcessedFn {
        return self.npc;
    }
    pub inline fn setNPc(self: ContextPtr, pc: Context.ProcessedFn) void {
        self.npc = pc;
    }
    pub inline fn getSelf(self: ContextPtr) Object {
        const wordsToDiscard = self.asHeapObjectPtr().hash16();
        @setRuntimeSafety(false);
        return (self.asObjectPtr() + wordsToDiscard)[0];
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
    pub inline fn asHeapObjectPtr(self : ContextPtr) HeapObjectPtr {
        return &self.header;
    }
    pub inline fn asObjectPtr(self : ContextPtr) [*]Object {
        return @ptrCast([*]Object,self);
    }
    inline fn fromObjectPtr(op: [*]Object) ContextPtr {
        return @ptrCast(ContextPtr,op);
    }
    pub fn print(self: *const Context, process: *const Process) void {
        const pr = std.debug.print;
        pr("Context: {*} {} {any}\n",.{self,self.header,self.allTemps(process)});
        //        if (self.prevCtxt) |ctxt| {ctxt.print(sp,process);}
    }
    pub fn call(oldPc: [*]const Code, sp: [*]Object, process: *Process, self: ContextPtr, selector: Object) void {
        self.tpc = oldPc+1;
        self.npc = oldPc[0].prim;
        trace("\ncall: N={*} T={*} {any}",.{self.getNPc(),self.getTPc(),self.stack(sp,process)});
        const method = @intToPtr(CompiledMethodPtr,@bitCast(u64,selector));
        const pc = @ptrCast([*]const Code,&method.code);
        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,self,method.selector});
    }
};
