const std = @import("std");
const config = @import("config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
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
const SendCache = execute.SendCache;
const TestExecution = execute.TestExecution;
const Code = execute.Code;
const PC = execute.PC;
const SP = execute.SP;
const CompiledMethodPtr = execute.CompiledMethodPtr;
pub const ContextPtr = *Context;
pub var nullContext = Context.init();
pub const Context = struct {
    header: HeapObject, // if not on stack there is also a footer
    method: CompiledMethodPtr, // note this is not an Object, so access and GC need to handle specially
    tpc: PC, // threaded PC
    npc: ThreadedFn, // native PC - in Continuation Passing Style
    prevCtxt: ContextPtr, // note this is not an Object, so access and GC need to handle specially
    trapContextNumber: u64,
    temps: [nLocals]Object,
    const Self = @This();
    const ThreadedFn = *const fn (programCounter: PC, stackPointer: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) SP;
    const nLocals = 1;
    const baseSize = @sizeOf(Self) / @sizeOf(Object) - nLocals;
    pub fn init() Self {
        return Self{
            .header = comptime heap.footer(baseSize + nLocals, Format.header, object.ClassIndex.Context, 0, Age.static),
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

        try writer.print("context: {}", .{self.header});
        try writer.print(" prev: 0x{x}", .{@intFromPtr(self.previous())});
        if (false) {
            @setRuntimeSafety(false);
            try writer.print(" temps: {any}", .{self.temps[0..self.size]});
        }
    }
    pub inline fn pop(self: *Context, process: *Process) struct { sp: SP, ctxt: ContextPtr } {
        const wordsToDiscard = self.header.hash16();
        trace("\npop: {x} {} {}", .{ @intFromPtr(self), self.header, wordsToDiscard });
        if (self.isOnStack())
            return .{ .sp = self.asNewSp().unreserve(wordsToDiscard+1), .ctxt = self.previous() };
        _ = process;
        std.debug.print("\npop: {*}", .{self});
        @panic("incomplete");
        // const itemsToKeep = self.temps[wordsToDiscard-baseSize..self.size];
        // const newSp = process.endOfStack() - itemsToKeep.len;
        // for (itemsToKeep,0..) | obj,index | {
        //     newSp[index] = obj;
        // }
        // return .{.sp=newSp,.ctxt=self.previous()};
    }
    pub inline fn push(self: *Context, sp: SP, process: *Process, method: CompiledMethodPtr, locals: u16, maxStackNeeded: u16, selfOffset: u16) ContextPtr {
        if (@intFromPtr(self) == 0) @panic("0 self");
        var contextMutable = self;
        const newSp = process.allocStack(sp, baseSize + locals + maxStackNeeded, &contextMutable).unreserve(maxStackNeeded);
        trace("\npush: {} {} {} {}", .{ baseSize, locals, maxStackNeeded, selfOffset });
        trace("\npush: {} sp={*} newSp={*}", .{ method.selector, sp, newSp });
        const ctxt = @as(*align(@alignOf(Self)) Context, @ptrCast(@alignCast(newSp)));
        ctxt.prevCtxt = contextMutable;
        ctxt.trapContextNumber = process.trapContextNumber;
        ctxt.method = method;
        {
            @setRuntimeSafety(false);
            for (ctxt.temps[0..locals]) |*local| {
                local.* = Nil;
            }
        }
        ctxt.header = HeapObject.partialHeaderOnStack(baseSize + selfOffset);
        trace("\npush: {}",.{ ctxt.header });
        if (process.needsCheck()) @panic("process needsCheck");
        return ctxt;
    }
    pub fn moveToHeap(self: *const Context, sp: SP, process: *Process) ContextPtr {
        _ = self;
        _ = sp;
        _ = process;
        unreachable;
        // if (self.isIncomplete()) {
        //     self.header = heap.header(4, Format.bothAP, class.Context_C,0,Age.stack);
        //     self.size = self.prevCtxt.calculatedSize(process);
        //     self.addr = @ptrCast(*Object,&self.temps);
        //     self.prevCtxt.moveToHeap(sp, process);
        // }
    }
    pub inline fn isOnStack(self: *const Self) bool {
        return self.header.isOnStack();
    }
    inline fn endOfStack(self: *const Context, process: *const Process) SP {
        return if (self.isOnStack()) self.asObjectPtr() else process.endOfStack();
    }
    inline fn tempSize(self: *const Context, process: *const Process) usize {
        return (@intFromPtr(self.previous().endOfStack(process)) - @intFromPtr(&self.temps)) / @sizeOf(Object) - 1;
    }
    pub fn stack(self: *const Self, sp: SP, process: *Process) []Object {
        if (self.isOnStack())
            return sp.slice((@intFromPtr(self.endOfStack(process)) - @intFromPtr(sp)) / @sizeOf(Object) - 1);
        return process.getStack(sp);
    }
    pub inline fn allLocals(self: *const Context, process: *const Process) []Object {
        const size = self.tempSize(process);
        @setRuntimeSafety(false);
        return @constCast(self.temps[0..size]);
    }
    pub inline fn getTPc(self: *const Context) PC {
        return self.tpc;
    }
    pub inline fn setReturnBoth(self: ContextPtr, npc: ThreadedFn, tpc: PC) void {
        self.npc = npc;
        self.tpc = tpc;
    }
    pub inline fn setReturn(self: ContextPtr, tpc: PC) void {
        self.setReturnBoth(tpc.prim, tpc.next());
    }
    pub inline fn getNPc(self: *const Context) Context.ThreadedFn {
        return self.npc;
    }
    pub inline fn setNPc(self: *Context, npc: ThreadedFn) void {
        self.npc = npc;
    }
    pub inline fn setTPc(self: *Context, tpc: PC) void {
        self.tpc = tpc;
    }
    pub inline fn getSelf(self: *const Context) Object {
        const wordsToDiscard = self.asHeapObjectPtr().hash16();
        return self.asObjectPtr()[wordsToDiscard];
    }
    pub inline fn setResult(self: *const Context, value: Object) void {
        const wordsToDiscard = self.asHeapObjectPtr().hash16();
        self.asObjectPtr()[wordsToDiscard] = value;
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
        if (@intFromPtr(self.prevCtxt) == 0) @panic("0 prev");
        return self.prevCtxt;
    }
    pub inline fn asHeapObjectPtr(self: *const Context) HeapObjectPtr {
        return &self.header;
    }
    pub inline fn asObjectPtr(self: *const Context) SP {
        return @as(SP, @ptrCast(@constCast(self)));
    }
    pub inline fn asNewSp(self: *const Context) SP {
        return @as(SP, @ptrCast(@constCast(self))).reserve(1);
    }
    pub inline fn cleanAddress(self: *const Context) u64 {
        return @intFromPtr(self);
    }
    inline fn fromObjectPtr(op: [*]Object) ContextPtr {
        return @as(ContextPtr, @ptrCast(op));
    }
    pub fn print(self: *const Context, process: *const Process) void {
        const pr = std.debug.print;
        pr("Context: {*} {} {any}\n", .{ self, self.header, self.allLocals(process) });
        //        if (self.prevCtxt) |ctxt| {ctxt.print(sp,process);}
    }
    pub fn call(oldPc: [*]const Code, sp: SP, process: *Process, self: ContextPtr, selector: Object, cache: SendCache) SP {
        self.tpc = oldPc + 1;
        self.npc = oldPc[0].prim;
        trace("\ncall: N={*} T={*} {any}", .{ self.getNPc(), self.getTPc(), self.stack(sp, process) });
        const method = @as(CompiledMethodPtr, @ptrFromInt(@as(u64, @bitCast(selector))));
        const pc = @as([*]const Code, @ptrCast(&method.code));
        _ = .{ pc, oldPc, sp, process, selector, cache, @panic("call unimplemented")};
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
