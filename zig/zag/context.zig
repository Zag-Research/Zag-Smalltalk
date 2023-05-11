const std = @import("std");
const checkEqual = @import("utilities.zig").checkEqual;
const Thread = @import("thread.zig").Thread;
const object = @import("object.zig");
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const heap = @import("heap.zig");
const HeapObjectPtr = heap.HeapObjectPtr;
const Format = heap.Format;
const Age = heap.Age;
const class = @import("class.zig");
const Code = @import("execute.zig").Code;
const CompiledMethodPtr = @import("execute.zig").CompiledMethodPtr;
const MethodReturns = void;
pub const ContextPtr = *Context;
pub var nullContext = Context.init();
pub const Context = struct {
    header: heap.HeapObject, // only used while on stack
    tpc: [*]const Code, // threaded PC
    npc: ThreadedFn, // native PC - in Continuation Passing Style
    prevCtxt: ContextPtr, // note this is not an Object, so access and GC need to handle specially
    method: CompiledMethodPtr, // note this is not an Object, so access and GC need to handle specially
    size: if (hasSizeField) u64 else void,
    temps: [nTemps]Object,
    const Self = @This();
    const ThreadedFn = * const fn(programCounter: [*]const Code, stackPointer: [*]Object, thread: *Thread, context: ContextPtr, selectorHash: u32) MethodReturns;
    const hasSizeField = true;
    const nTemps = 1;
    const baseSize = @sizeOf(Self)/@sizeOf(Object) - nTemps;
    pub fn init() Self {
        return Self {
            .header = comptime heap.footer(baseSize+nTemps,Format.context,class.Context_I,0,Age.static),
            .tpc = undefined,
            .npc = undefined,
            .prevCtxt = undefined,
            .method = undefined,
            .temps = undefined,
            .size = undefined,
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
        if (hasSizeField) {
            @setRuntimeSafety(false);
            try writer.print(" temps: {any}",.{self.temps[0..self.size]});
        }
    }
    pub inline fn pop(self: ContextPtr, thread: *Thread) struct { sp: [*]Object,ctxt: ContextPtr } {
        const wordsToDiscard = self.header.hash16();
        if (self.isIncomplete())
            return .{.sp=self.asObjectPtr() + wordsToDiscard,.ctxt=self.prevCtxt};
        _ = thread;@panic("incomplete");
        // const itemsToKeep = self.temps[wordsToDiscard-baseSize..self.size];
        // const newSp = thread.endOfStack() - itemsToKeep.len;
        // for (itemsToKeep,0..) | obj,index | {
        //     newSp[index] = obj;
        // }
        // return .{.sp=newSp,.ctxt=self.previous()};
    }
    pub fn push(self: ContextPtr, sp: [*]Object, thread: *Thread, method: CompiledMethodPtr, locals: u16, maxStackNeeded: u16, selfOffset: u16)  ContextPtr {
        const newSp = sp - baseSize - locals;
        if (thread.checkStack(newSp,self,5+maxStackNeeded)) |grow| return grow.context.push(grow.sp,grow.thread,method,locals,maxStackNeeded,selfOffset);
        const ctxt = @ptrCast(ContextPtr,@alignCast(@alignOf(Self),newSp));
        ctxt.prevCtxt = self;
        ctxt.method = method;
        { @setRuntimeSafety(false);
         for (ctxt.temps[0..locals]) |*local| {local.*=Nil;}
         }
        ctxt.header = heap.HeapObject.partialOnStack(selfOffset+baseSize);
        if (hasSizeField) ctxt.size = ctxt.calculatedSize(thread);
        if (thread.needsCheck()) @panic("thread needsCheck");
       return ctxt;
    }
    pub fn moveToHeap(self: ContextPtr, sp: [*]Object, thread: *Thread) ContextPtr {
        _=self;_=sp;_=thread;unreachable;
        // if (self.isIncomplete()) {
        //     self.header = heap.header(4, Format.bothAP, class.Context_I,0,Age.stack);
        //     self.size = self.prevCtxt.calculatedSize(thread);
        //     self.addr = @ptrCast(*Object,&self.temps);
        //     self.prevCtxt.convertToProperHeapObject(sp, thread);
        // }
    }
    pub inline fn isIncomplete(self: * const Self) bool {
        return @alignCast(8,&self.header).isIncompleteContext();
    }
    inline fn endOfStack(self: ContextPtr, thread: *Thread) [*]Object {
        return if (self.isIncomplete()) self.asObjectPtr() else thread.endOfStack();
    }
    inline fn calculatedSize(self: ContextPtr, thread: *Thread) usize {
        return (@ptrToInt(self.prevCtxt.endOfStack(thread))-@ptrToInt(&self.temps))/@sizeOf(Object);
    }
    pub inline fn stack(self: *Self, sp: [*]Object, thread: *Thread) []Object {
        return sp[0..(@ptrToInt(self.endOfStack(thread))-@ptrToInt(sp))/@sizeOf(Object)];
    }
    pub inline fn allTemps(self: ContextPtr, thread: *Thread) []Object {
        const size = if (self.isIncomplete()) self.calculatedSize(thread) else self.size;
        @setRuntimeSafety(false);
        return self.temps[0..size];
    }
    pub inline fn getTPc(self: ContextPtr) [*]const Code {
        return self.tpc;
    }
    pub inline fn setTPc(self: ContextPtr, pc: [*]const Code) void {
        self.tpc = pc;
    }
    pub inline fn getNPc(self: ContextPtr) Context.ThreadedFn {
        return self.npc;
    }
    pub inline fn setNPc(self: ContextPtr, pc: Context.ThreadedFn) void {
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
        return @ptrCast(HeapObjectPtr,self);
    }
    pub inline fn asObjectPtr(self : ContextPtr) [*]Object {
        return @ptrCast([*]Object,self);
    }
    inline fn fromObjectPtr(op: [*]Object) ContextPtr {
        return @ptrCast(ContextPtr,op);
    }
    pub fn print(self: ContextPtr, thread: *Thread) void {
        const pr = std.debug.print;
        pr("Self: {} {any}\n",.{self.header,self.allTemps(thread)});
        //        if (self.prevCtxt) |ctxt| {ctxt.print(sp,thread);}
    }
};
