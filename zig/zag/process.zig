//! Process object layout, stack management, and nursery allocation.

const std = @import("std");
const math = std.math;
const assert = std.debug.assert;
const mem = std.mem;
const SeqCst = std.builtin.AtomicOrder.seq_cst;
const builtin = @import("builtin");

const zag = @import("zag.zig");
const config = zag.config;
const tailCall = config.tailCall;
const trace = config.trace;
const object = zag.object;
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const ClassIndex = object.ClassIndex;
const checkEqual = zag.utilities.checkEqual;
const heap = zag.heap;
const HeapHeader = heap.HeapHeader;
const HeapObject = heap.HeapObject;
const HeapObjectArray = heap.HeapObjectArray;
const footer = heap.footer;
const Age = heap.Age;
const Format = heap.Format;
const allocationInfo = heap.AllocationInfo.calc;
const AllocResult = heap.AllocResult;
const Context = zag.Context;
const Extra = Context.Extra;
const execute = zag.execute;
const Code = execute.Code;
const PC = execute.PC;
const Result = execute.Result;
const largerPowerOf2 = zag.utilities.largerPowerOf2;
const process_total_size = config.process_total_size;

threadlocal var thisThread: std.Thread = undefined;
threadlocal var staticContext: Context = undefined;
pub threadlocal var thisProcess: Process = undefined;
const Top = @This();
pub const Process = extern struct {
    next: ?*Process,
    os_target: ?OsHandle,
    threadId: *std.Thread,
    id: u64,
    trapContextNumber: u64,
    status: ProcessStatus,
    request: ProcessRequest,
    context: *Context,
    currHeap: HeapObjectArray,
    currHp: HeapObjectArray,
    currEnd: HeapObjectArray,
    otherHeap: HeapObjectArray,
    sp: Top.SP,
    nursery: [nursery_size]Object,
    stack: [stack_size]Object, // some wastage as stack must be aligned
    pub const SP = Top.SP;
    pub const StackMask = Top.StackMask;
    pub const threadedFunctions = Top.threadedFunctions;
    pub const stack_mask_shift = Top.stack_mask_shift;
    pub const stack_mask = Top.stack_mask;
    pub const check = Top.check;
    pub const getContext = Top.getContext;
    pub const setContext = Top.setContext;
    pub const getSp = Top.getSp;
    pub const setSp = Top.setSp;
    pub const endOfStack = Top.endOfStack;
    //pub const getProcess = Top.getProcess;
    pub const initProcess = Top.initProcess;
    pub const freeNursery = Top.freeNursery;
    pub const collectNursery = Top.collectNursery;
};
const fields_size = 12 * 8;
comptime {
    //@compileLog(stack_size, nursery_size);
    //@compileLog(stack_size + nursery_size + fields_size, process_total_size / 8);
    //@compileLog(@offsetOf(Process, "stack"), @offsetOf(Process, "stack") & stack_mask, @offsetOf(Process, "stack") + stack_size * 8);
    //@compileLog(fields_size, @offsetOf(Process, "nursery"));
    assert(fields_size == @offsetOf(Process, "nursery"));
}
const data_size = (process_total_size - fields_size - alignment_adjustment) / @sizeOf(Object);
const percentage_stack = 10; // must be < 25 to maintian invariant that stack can be copied to nursery
const stack_size: usize = largerPowerOf2(data_size * percentage_stack / 100) - 1;
const stack_mask_overflow: usize = largerPowerOf2(stack_size * @sizeOf(Object));
pub const stack_mask = stack_mask_overflow - @sizeOf(Object);
pub const stack_mask_shift = @ctz(stack_mask_overflow);
pub const StackMask = @import("std").meta.Int(.unsigned, stack_mask_shift);
const nursery_size = data_size - stack_size - 1;
pub fn process_stack_size() usize {
    return stack_size;
}
pub fn process_nursery_size() usize {
    return nursery_size / 2;
}
fn initProcess( //threadId: std.Thread,
    id: u64,
) void {
    trace("initProcess", .{});
    thisProcess.next = null;
    thisProcess.os_target = null;
    thisThread = undefined; //threadId;
    thisProcess.threadId = &thisThread;
    thisProcess.id = id;
    thisProcess.trapContextNumber = 0;
    thisProcess.status = .running;
    thisProcess.request = .normal;
    const nursery = HeapObject.fromObjectPtr(@ptrCast(&thisProcess.nursery));
    thisProcess.currHeap = nursery;
    thisProcess.currHp = nursery;
    thisProcess.currEnd = nursery + process_nursery_size();
    thisProcess.otherHeap = thisProcess.currEnd;
    thisProcess.context = &staticContext;
    staticContext.initStatic();
    thisProcess.sp = endOfStack();
    const offset = @intFromPtr(&thisProcess.stack) & stack_mask;
    if (offset != 0) {
        const correction = stack_mask + 8 - offset;
        std.debug.print("change alignment_adjust to {}\n", .{if (correction < alignment_adjustment) alignment_adjustment - correction else offset});
        @panic("correct alignment_adjust");
    }
}
const alignment_adjustment = switch (builtin.os.tag) {
    .macos => 8,
    .linux => 0,
    .windows => 0,
    else => @compileError("unsupported OS"),
};
pub inline fn endOfStack() SP {
    return @ptrCast(@as([*]Object, @ptrCast(&thisProcess.stack)) + stack_size);
}
pub const OsHandle = if (builtin.os.tag == .windows)
    std.os.windows.HANDLE
else
    std.c.pthread_t;
const ProcessStatus = enum(u8) {
    running, // thread is actually executing
    blocked, // waiting on I/O or calling some FFI
    gcMarking, // the process is marking reachable objects
    waiting, // waiting for return to .normal
    exited, // thread has finished
};

const ProcessRequest = enum(u8) {
    normal, // thread is alternating between running and blocking
    quit, // thread is asked to quit - if blocked, interrupted
    save, // thread is asked to save the process object to the image
    gcMark, // the GC thread is asking the process to mark reachable globals

};
const AgeSizes = [Age.lastNurseryAge]usize;
fn collectNursery(sp: SP, context: *Context, need: usize) void {
    assert(need <= process_nursery_size());
    var ageSizes: AgeSizes = [_]usize{0} ** Age.lastNurseryAge;
    collectNurseryPass(sp, context, &ageSizes, Age.lastNurseryAge + 1);
    if (freeNursery() >= need) return;
    var total: usize = 0;
    var age = Age.lastNurseryAge;
    while (age >= 0) : (age -= 1) {
        total += ageSizes[age];
        if (total >= need) {
            collectNurseryPass(sp, context, &ageSizes, age);
            return;
        }
    }
    @panic("Insufficient nursery space");
}
fn collectNurseryPass(originalSp: SP, originalContext: *Context, sizes: *AgeSizes, promoteAge: usize) void {
    const hp = collectStack(originalSp, originalContext, sizes, promoteAge);
    finishCollection(hp, thisProcess.otherHeap, sizes, promoteAge);
}
fn collectStack(originalSp: SP, originalContext: *Context, sizes: *AgeSizes, promoteAge: usize) HeapObjectArray {
    _ = .{ sizes, promoteAge };
    var hp = thisProcess.otherHeap;
    var context = originalContext;
    var sp = originalSp;
    // find references from the stacked contexts
    while (true) {
        const endSP = context.endOfStack(sp);
        while (sp.lessThan(endSP)) {
            if (sp.top.ifHeapObject()) |pointer| {
                if (pointer.isForwarded()) {
                    hp = pointer.copyTo(hp, &sp.top);
                } else if (pointer.isLocal()) {
                    hp = pointer.copyTo(hp, &sp.top);
                }
            }
            sp = sp.drop();
        }
        sp = context.callerStack(sp) orelse break;
        context = context.previous();
    }
    return hp;
}
fn copyObjectAndDependents(obj: anytype) @TypeOf(obj) {
    const result = thisProcess.currHp;
    const hp = @as(*HeapObject, @ptrCast(result)).copyTo(result, null);
    var sizes: AgeSizes = [_]usize{0} ** Age.lastNurseryAge;
    finishCollection(hp, result, &sizes, 0);
    return @ptrCast(result);
}
fn finishCollection(startingHp: HeapObjectArray, startingScan: HeapObjectArray, sizes: *AgeSizes, promoteAge: usize) void {
    _ = .{promoteAge};
    sizes[0] = 0;
    var hp = startingHp;
    var scan = startingScan;
    while (@intFromPtr(scan) < @intFromPtr(hp)) {
        if (scan[0].iterator()) |iter| {
            var it = iter;
            while (it.next()) |objPtr| {
                if (objPtr.ifHeapObject()) |pointer| {
                    if (pointer.ifForwarded()) |original| {
                        objPtr.* = original.asObject();
                    } else if (pointer.isLocal())
                        hp = pointer.copyTo(hp, objPtr);
                }
            }
        }
        scan = scan[0].skipForward();
    }
    // swap heaps
    const head = &thisProcess;
    const tempHeap = head.otherHeap;
    head.otherHeap = head.currHeap;
    head.currHeap = tempHeap;
    head.currHp = hp;
    head.currEnd = tempHeap + process_nursery_size();
    for (head.otherHeap[0..process_nursery_size()]) |*obj| {
        obj.* = undefined;
    }
}
inline fn freeNursery() usize {
    return (@intFromPtr(thisProcess.currEnd) - @intFromPtr(thisProcess.currHp)) / 8;
}
fn dumpHeap() void {
    var scan = thisProcess.currHeap;
    const hp = thisProcess.currHp;
    while (@intFromPtr(scan) < @intFromPtr(hp)) {
        std.debug.print("[{x:0>10}]: {f}\n", .{ @intFromPtr(scan), scan[0].header });
        const next = scan[0].skipForward();
        scan += 1;
        while (@intFromPtr(scan) < @intFromPtr(next)) {
            std.debug.print("[{x:0>10}]:      {f}\n", .{ @intFromPtr(scan), @as(Object, @bitCast(scan[0])) });
            scan += 1;
        }
    }
}
pub fn format(
    self: *Process,
    writer: anytype,
) !void {
    try writer.print("process: {} .stack = {any}", .{ self.id, self.getStack(self.sp) });
    try writer.print(" .heap = {any}", .{self.getHeap()});
}
pub inline fn check(prim: anytype) @TypeOf(prim) {
    return prim;
}
const Debugger = struct {
    var buf: [10]u8 = undefined;
    var stdin = std.fs.File.stdin().reader(&buf);
    var in = &stdin.interface; // must be separate bc @fieldParentPtr. Thanks @Freakman
    fn step(pc: PC, sp: SP, context: *Context, extra: Extra) Result {
        trace("step: {f}", .{pc});
        const primPC = pc.prev();
        trace(" {f}", .{primPC});
        const primitive = primPC.prim();
        const method = if (extra.getMethod()) |cm| cm else context.method;
        trace(" {*} {*}", .{ primitive, method });
        std.log.err("{f}:{d:0>3}: ", .{ method.signature, primPC.offset(method) });
        if (@import("threadedFn.zig").find(primitive)) |name| {
            std.log.err("{}", .{name});
            switch (name) {
                .push => {
                    const variable = pc.variable();
                    if (variable.stackOffset == 0) {
                        std.log.err(" self", .{});
                    } else {
                        std.log.err(" {f}", .{variable});
                    }
                },
                .pushLiteral => {
                    std.log.err(" {f}", .{pc.object()});
                },
                .branchFalse, .branchTrue, .branch => {
                    std.log.err(" {d:0>3}", .{pc.targetPC().offset(method)});
                },
                else => {},
            }
            std.log.err("\n", .{});
        } else if (zag.primitives.findPrimitiveAtPtr(primitive)) |modPrim| {
            std.log.err("{s}:{s}", .{ modPrim.module, modPrim.name });
            if (modPrim.number > 0) {
                std.log.err("({d})", .{modPrim.number});
            }
            std.log.err("\n", .{});
        } else {
            std.log.err("{x}\n", .{@intFromPtr(primitive)});
        }
        // while (in.takeDelimiterExclusive('\n')) |line| {
        //     std.log.err("you typed: {s}\n", .{line});
        // } else |err| switch (err) {
        //     error.EndOfStream => {},
        //     else =>  |_| @panic("fail read stdin"),
        // }
        return @call(tailCall, primitive, .{ pc, sp, &thisProcess, context, extra });
    }
};
pub inline fn getSp() SP {
    return thisProcess.sp;
}
pub inline fn setSp(sp: SP) void {
    thisProcess.sp = sp;
}
pub inline fn getProcess() *Process {
    return &thisProcess;
}
pub inline fn getContext() *Context {
    return thisProcess.context;
}
pub inline fn setContext(context: *Context) void {
    thisProcess.context = context;
}
pub inline fn freeStack(sp: SP) usize {
    return (@intFromPtr(sp) - @intFromPtr(&thisProcess.stack)) / 8;
}

pub inline fn getHeap() []HeapObject {
    return thisProcess.currHeap[0..((@intFromPtr(thisProcess.currHp) - @intFromPtr(thisProcess.currHeap)) / @sizeOf(Object))];
}
pub fn allocArray(slice: []const Object, sp: SP, context: *Context) HeapObjectArray {
    const len: u11 = @intCast(slice.len);
    const hop = thisProcess.allocSpace(len, sp, context);
    HeapHeader.objectInNursery(.Array, .directIndexed, len).storeAt(hop);
    const target: HeapObjectArray = @ptrCast(hop);
    @memcpy(target + 1, @as([]const HeapObject, @ptrCast(slice)));
    return target;
}
fn allocSpace(size: u11, sp: SP, context: *Context) *HeapObject {
    const result = thisProcess.currHp;
    const newHp = result + size + 1;
    if (@intFromPtr(newHp) <= @intFromPtr(thisProcess.currEnd)) {
        thisProcess.currHp = newHp;
        return @ptrCast(result);
    }
    _ = .{ sp, context, unreachable };
}
test "stack operations" {
    const ee = std.testing.expectEqual;
    Process.initProcess(0);
    const endSp = Process.endOfStack();
    try ee(1016, @intFromPtr(endSp) & stack_mask);
    try ee(endSp.endOfStack(), Process.endOfStack());
    try ee(endSp.reserve(1).?.endOfStack(), Process.endOfStack());
    try ee(endSp.reserve(10).?.endOfStack(), Process.endOfStack());
}
test "nursery allocation" {
    const ee = std.testing.expectEqual;
    const expect = std.testing.expect;
    Process.initProcess(0);
    const emptySize = process_nursery_size();
    try ee(127, process_stack_size());
    try ee(emptySize, Process.freeNursery());
    var sp = Process.endOfStack();
    const initialContext = Process.getContext();
    var ar = sp.alloc(initialContext, ClassIndex.Class, 4, null, void, false);
    _ = ar.initAll();
    const o1 = ar.allocated;
    try ee(emptySize - 6, Process.freeNursery());
    ar = sp.alloc(initialContext, ClassIndex.Class, 5, null, void, false);
    _ = ar.initAll();
    ar = sp.alloc(initialContext, ClassIndex.Class, 6, null, void, false);
    const o2 = ar.initAll();
    try ee(emptySize - 20, Process.freeNursery());
    try o1.instVarPut(0, o2.asObject());
    try o1.instVarPut(1, False());
    try o1.instVarPut(2, o2.asObject());
    try o2.instVarPut(0, o1.asObject());
    try o2.instVarPut(1, o2.asObject());
    try o2.instVarPut(2, True());
    sp = sp.push(o1.asObject()).?.push(o1.asObject()).?;
    const news, const newContext, const newExtra = sp.spillStack(initialContext, Extra.none);
    try ee(sp, news);
    try ee(initialContext, newContext);
    // ToDo: age test
    _ = newExtra;
    // sp.dumpStack("Original stack before collect", newContext, newExtra);
    // dumpHeap();
    Process.collectNursery(sp, initialContext, 0);
    // sp.dumpStack("stack after collect", newContext, newExtra);
    // dumpHeap();
    try ee(emptySize - switch (config.objectEncoding) {
        .zag => 12,
        .nan => 5,
        else => 7,
    }, Process.freeNursery());
    try ee(sp.top, sp.next);
    try expect(sp.top != o1.asObject());
    if (sp.top.ifHeapObject()) |a1| {
        const a1v = try a1.instVars();
        try ee(a1v[1], False());
        try ee(a1v[0], a1v[2]);
        if (a1v[0].ifHeapObject()) |a2| {
            const a2v = try a2.instVars();
            try ee(a2v[2], True());
            try ee(a2v[0], a1.asObject());
            try ee(a2v[1], a2.asObject());
        } else return error.ShouldBeHeapObject;
    } else return error.ShouldBeHeapObject;
}
pub const SP = *Stack;
pub const initStack = Stack.from;
const Stack = struct {
    top: Object,
    next: Object,
    third: Object,
    comptime {
        std.debug.assert(@offsetOf(Stack, "top") == 0);
        std.debug.assert(@offsetOf(Stack, "next") == @sizeOf(Object));
        std.debug.assert(@offsetOf(Stack, "third") == @sizeOf(Object) * 2);
    }
    pub inline fn lessThan(self: SP, other: anytype) bool {
        return @intFromPtr(self) < @intFromPtr(other);
    }
    pub inline fn lessThanEqual(self: SP, other: anytype) bool {
        return @intFromPtr(self) <= @intFromPtr(other);
    }
    fn from(self: anytype) SP {
        return @ptrCast(self);
    }
    pub inline fn push(self: SP, v: Object) ?SP {
        if (self.reserve(1)) |newSp| {
            newSp.top = @bitCast(v);
            return newSp;
        }
        return null;
    }
    pub inline fn pushRawInt(self: SP, v: u64) ?SP {
        if (self.reserve(1)) |newSp| {
            newSp.top = @bitCast(v);
            return newSp;
        }
        return null;
    }
    pub inline fn dropPut(self: SP, v: Object) SP {
        self.next = v;
        return self.unreserve(1);
    }
    pub inline fn drop(self: SP) SP {
        return self.unreserve(1);
    }
    pub inline fn reserve(self: SP, n: anytype) ?SP {
        if (@TypeOf(n) == comptime_int and n == 1) {
            const newP = @intFromPtr(self) - 8;
            if (newP & stack_mask == 0) {
                @branchHint(.unlikely);
                return null;
            }
            return @ptrFromInt(newP);
        } else {
            const newP = @intFromPtr(self) - @sizeOf(Object) * n;
            if (self.contains(newP)) {
                @branchHint(.likely);
                return @ptrFromInt(newP);
            }
            return null;
        }
    }
    pub inline fn safeReserve(self: SP, n: anytype) SP {
        return @ptrFromInt(@intFromPtr(self) - @sizeOf(Object) * n);
    }
    pub inline fn unreserve(self: SP, n: anytype) SP {
        return @ptrFromInt(@intFromPtr(self) + @sizeOf(Object) * n);
    }
    pub inline fn delta(self: SP, other: SP) usize {
        return (@intFromPtr(other) - @intFromPtr(self)) / @sizeOf(Object);
    }
    pub inline fn contains(self: SP, other: anytype) bool {
        return ((if (@TypeOf(other) == usize) other else @intFromPtr(other)) ^ @intFromPtr(self)) >> stack_mask_shift == 0;
    }
    pub inline fn array(self: SP) [*]Object {
        return @ptrCast(self);
    }
    pub inline fn slice(self: SP, n: usize) []Object {
        return self.array()[0..n];
    }
    pub inline //
    fn sliceTo(self: SP, a_ptr: anytype) []Object {
        return self.slice((@intFromPtr(a_ptr) - @intFromPtr(self)) / @sizeOf(Object));
    }
    pub inline fn at(self: SP, n: usize) Object {
        return self.array()[n];
    }
    pub inline fn atPut(self: SP, n: usize, o: Object) void {
        self.array()[n] = o;
    }
    pub inline //
    fn getStack(self: SP) []Object {
        return self.sliceTo(self.endOfStack());
    }
    pub fn dumpStack(self: SP, why: []const u8, context: *Context, extra: Extra) void {
        std.debug.print("dumpStack ({s})\n", .{why});
        doStack(std.debug.print, self, context, extra);
    }
    fn doStack(print: anytype, sp: SP, context: *Context, extra: Extra) void {
        const newline = if (print == trace) "" else "\n";
        const selfAddr = extra.selfAddress(sp) orelse context.selfAddress(sp);
        for (sp.getStack()) |*obj| {
            const addr = @intFromPtr(obj);
            var onStackObject: *const HeapObject = @ptrCast(context.endOfStack(sp));
            if (addr == @intFromPtr(context)) {
                print("[{x:0>10}]: 0x{x:0>16} <-- ctx{s}", .{ addr, @as(u64, @bitCast(obj.*)), newline });
                break;
            } else if (addr == @intFromPtr(onStackObject)) {
                print("[{x:0>10}]: {f} <-- on stack object{s}", .{ addr, onStackObject, newline });
                onStackObject = @ptrCast(onStackObject.skipForward());
            } else {
                print("[{x:0>10}]: {f}{s}{s}{s}", .{ addr, obj.*, if (addr == @intFromPtr(sp)) " <--sp" else "", if (addr == @intFromPtr(selfAddr)) " <--self" else "", newline });
            }
        }
    }
    pub fn traceStack(self: SP, why: []const u8, context: *Context, extra: Extra) void {
        trace("traceStack ({s}) {} {}", .{ why, @intFromPtr(self.endOfStack()) - @intFromPtr(self), self.getStack().len });
        trace("sp = {*} context = {*} extra = {f}", .{ self, context, extra });
        doStack(trace, self, context, extra);
    }
    pub inline fn getProcess(_: SP) *Process {
        return &thisProcess;
    }
    inline fn theProcess(_: SP) *Process {
        return &thisProcess;
    }
    pub fn trapContextNumber(_: SP) u64 {
        return thisProcess.trapContextNumber;
    }
    pub inline fn endOfStack(self: SP) SP {
        return @ptrFromInt((@intFromPtr(self) | stack_mask));
    }
    pub fn alloc(self: SP, context: *Context, classIndex: ClassIndex, iVars: u11, indexed: ?usize, comptime element: type, makeWeak: bool) AllocResult {
        const aI = allocationInfo(iVars, indexed, element, makeWeak);
        if (aI.objectSize(@min(HeapHeader.maxLength, process_nursery_size() / 4))) |size| {
            for (0..2) |_| {
                //                if (true) @panic("here 490");
                const result = HeapObject.alignProperBoundary(thisProcess.currHp);
                const newHp = result + size + 1;
                if (@intFromPtr(newHp) <= @intFromPtr(thisProcess.currEnd)) {
                    thisProcess.currHp = newHp;
                    const obj: *HeapObject = @ptrCast(result);
                    aI.initObjectStructure(obj, classIndex, .nursery);
                    return .{
                        .age = .nursery,
                        .allocated = obj,
                        .info = aI,
                    };
                }
                collectNursery(self, context, size + 1);
            }
            @panic("unable to collect enough space");
        }
        @panic("Need Global Allocation");
    }
    pub fn spillStackAndPush(sp: SP, value: Object, context: *Context, extra: Extra) struct { SP, *Context, Extra } {
        const newSp, const newContext, const newExtra = sp.spillStackAndReserve(1, context, extra);
        newSp.top = value;
        return .{ newSp, newContext, newExtra };
    }
    pub fn spillStackAndReserve(sp: SP, n: usize, context: *Context, extra: Extra) struct { SP, *Context, Extra } {
        const newSp, const newContext, const newExtra = sp.spillStack(context, extra);
        return .{ newSp.safeReserve(n), newContext, newExtra };
    }
    pub fn spillStack(sp: SP, context: *Context, extra: Extra) struct { SP, *Context, Extra } {
        if (!context.isOnStack(sp)) return .{ sp, context, extra };
        // if the Context is on the stack, the Context, Extra and SP will move
        const size = (@intFromPtr(sp.endOfStack()) - @intFromPtr(sp)) / @sizeOf(Object);
        dumpHeap();
        collectNursery(sp, context, size);
        dumpHeap();
        const stackToCopy = sp.sliceTo(context.endOfStack(sp));
        sp.dumpStack("original stack in spillStack <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<", context, extra);
        context.reify(sp);
        sp.dumpStack("reified stack in spillStack  ===========================================", context, extra);
        const newContext = copyObjectAndDependents(context);
        sp.dumpStack("copied stack in spillStack   >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>", context, extra);
        dumpHeap();
        var n = stackToCopy.len;
        const targetStack = (@as([*]Object, @ptrCast(sp.endOfStack())) - n)[0..n];
        while (n > 0) : (n -= 1) {
            targetStack[n - 1] = stackToCopy[n - 1];
        }
        const newSp: SP = @ptrCast(targetStack.ptr);
        newSp.dumpStack("newSp in spillStack ......................................", newContext, extra);
        @panic("spillStack unfinished");
    }
    pub fn format(
        self: *const Stack,
        writer: anytype,
    ) !void {
        try writer.print("stack(0x{x}): .top = {f} .next = {f}", .{ @intFromPtr(self), self.top, self.next });
    }
};

test "Stack" {
    Process.initProcess(0);
    const sp = Process.getSp();
    const context = Process.getContext();
    const ee = std.testing.expectEqual;
    const sp0 = sp.push(True()).?;
    const sp1 = sp0.push(False()).?;
    try ee(True(), sp1.next);
    try ee(False(), sp1.top);
    _ = sp1.drop().push(Object.from(42, sp1, context));
    try ee(sp1.top.to(i64), 42);
}
pub const threadedFunctions = struct {
    pub const pushThisProcess = struct {
        pub fn threadedFn(pc: PC, sp: SP, context: *Context, extra: Extra) Result {
            if (sp.push(Object.fromAddress(&thisProcess))) |newSp| {
                return @call(tailCall, pc.prim(), .{ pc.next(), newSp, context, extra });
            } else {
                @panic("StackOverflow");
            }
        }
    };
    // pub const debug = struct {
    //     pub fn threadedFn(pc: PC, sp: SP, context: *Context, extra: Extra) Result {
    //         const newProcess = singleStep();
    //         return @call(tailCall, pc.prim(), .{ pc.next(), sp, newProcess, context, extra });
    //     }
    // };
    // pub const enddebug = struct {
    //     pub fn threadedFn(pc: PC, sp: SP, context: *Context, extra: Extra) Result {
    //         const newProcess = unSingleStep();
    //         return @call(tailCall, pc.prim(), .{ pc.next(), sp, newProcess, context, extra });
    //     }
    // };
};
