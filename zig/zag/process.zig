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

const Self = @This();
const Process = @This();
next: ?*Process = null,
os_target: ?OsHandle = null,
threadId: std.Thread = undefined,
id: u64 = undefined,
trapContextNumber: u64 = 0,
status: ProcessStatus = .running,
request: ProcessRequest = .normal,
context: *Context = undefined,
currHeap: HeapObjectArray = undefined,
currHp: HeapObjectArray = undefined,
currEnd: HeapObjectArray = undefined,
otherHeap: HeapObjectArray = undefined,
sp: SP = undefined,
stack: [*]Object = undefined,
staticContext: Context = undefined,
data: [data_size]Object = undefined, // some wastage as stack must be aligned

const fields_size = 14 * 8 + @sizeOf(Context);
comptime {
    assert(fields_size == @offsetOf(@This(), "data"));
}
const data_size = (process_total_size - fields_size) / @sizeOf(Object);
const percentage_stack = 10; // must be < 25 to maintian invariant that stack can be copied to nursery
const stack_size: usize = largerPowerOf2(data_size * percentage_stack / 100) - 1;
const stack_mask_overflow: usize = largerPowerOf2(stack_size * @sizeOf(Object));
pub const stack_mask = stack_mask_overflow - @sizeOf(Object);
pub const stack_mask_shift = @ctz(stack_mask_overflow);
pub const StackMask = @import("std").meta.Int(.unsigned, stack_mask_shift);
pub fn process_stack_size() usize {
    return stack_size;
}
pub fn process_nursery_size() usize {
    return stackOffset() / 2;
}
fn stackOffset() usize {
    const address = @intFromPtr(&thisProcess.data[data_size - stack_size]) >> stack_mask_shift << stack_mask_shift;
    return (address - @intFromPtr(&thisProcess.data[0])) >> 3;
}
fn init(aProcess: *Process, threadId: std.Thread, id: u64) void {
    aProcess.threadId = threadId;
    aProcess.id = id;
    initProcess();
}
pub fn initProcess(_: *Process) void {
    thisProcess.currHeap = HeapObject.fromObjectPtr(@ptrCast(&thisProcess.data));
    thisProcess.currEnd = thisProcess.currHeap + process_nursery_size();
    thisProcess.currHp = thisProcess.currHeap;
    thisProcess.otherHeap = HeapObject.fromObjectPtr(@ptrCast(&thisProcess.data[process_nursery_size()]));
    thisProcess.context = &thisProcess.staticContext;
    thisProcess.staticContext.initStatic();
    thisProcess.stack = @as([*]Object, &thisProcess.data) + stackOffset();
    thisProcess.sp = thisProcess.endOfStack();
    if (@intFromPtr(thisProcess.stack) & stack_mask != 0) {
        @panic("stack not properly aligned");
    }
}
pub inline fn endOfStack(_: *Process) SP {
    return @ptrCast(@as([*]Object, @ptrCast(thisProcess.stack)) + stack_size);
}
pub const OsHandle = if (builtin.os.tag == .windows)
    std.os.windows.HANDLE
else
    std.c.pthread_t;
const ProcessStatus = enum {
    running, // thread is actually executing
    blocked, // waiting on I/O or calling some FFI
    gcMarking, // the process is marking reachable objects
    waiting, // waiting for return to .normal
    exited, // thread has finished
};

const ProcessRequest = enum {
    normal, // thread is alternating between running and blocking
    quit, // thread is asked to quit - if blocked, interrupted
    save, // thread is asked to save the process object to the image
    gcMark, // the GC thread is asking the process to mark reachable globals

};

fn collectNursery(_: *Process, sp: SP, context: *Context, need: usize) void {
    assert(need <= process_nursery_size());
    var ageSizes = [_]usize{0} ** Age.lastNurseryAge;
    thisProcess.collectNurseryPass(sp, context, &ageSizes, Age.lastNurseryAge + 1);
    if (thisProcess.freeNursery() >= need) return;
    var total: usize = 0;
    var age = Age.lastNurseryAge;
    while (age >= 0) : (age -= 1) {
        total += ageSizes[age];
        if (total >= need) {
            thisProcess.collectNurseryPass(sp, context, &ageSizes, age);
            return;
        }
    }
    @panic("Insufficient nursery space");
}
fn collectNurseryPass(_: *Process, originalSp: SP, originalContext: *Context, sizes: []usize, promoteAge: usize) void {
    const hp = thisProcess.collectStack(originalSp, originalContext, sizes, promoteAge);
    thisProcess.finishCollection(hp, thisProcess.otherHeap, sizes, promoteAge);
}
fn collectStack(_: *Process, originalSp: SP, originalContext: *Context, sizes: []usize, promoteAge: usize) HeapObjectArray {
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
fn copyObjectAndDependents(_: *Process, obj: anytype) @TypeOf(obj) {
    const result = thisProcess.currHp;
    const hp = @as(*HeapObject, @ptrCast(result)).copyTo(result, null);
    var sizes = [_]usize{0} ** Age.lastNurseryAge;
    thisProcess.finishCollection(hp, result, &sizes, 0);
    return @ptrCast(result);
}
fn finishCollection(_: *Process, startingHp: HeapObjectArray, startingScan: HeapObjectArray, sizes: []usize, promoteAge: usize) void {
    _ = .{ sizes, promoteAge };
    var hp = startingHp;
    var scan = startingScan;
    while (@intFromPtr(scan) < @intFromPtr(hp)) {
        if (scan[0].iterator()) |iter| {
            var it = iter;
            while (it.next()) |objPtr| {
                if (objPtr.ifHeapObject()) |pointer| {
                    if (pointer.isForwarded()) {
                        @panic("Forwarded object found in nursery");
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
inline fn freeNursery(_: *Process) usize {
    return (@intFromPtr(thisProcess.currEnd) - @intFromPtr(thisProcess.currHp)) / 8;
}
fn dumpHeap(_: *Process) void {
    var scan = thisProcess.currHeap;
    const hp = thisProcess.currHp;
    std.debug.print("heap: {*} {*}\n", .{ scan, hp });
    while (@intFromPtr(scan) < @intFromPtr(hp)) {
        std.debug.print("[{x:0>10}]: {f}\n", .{ @intFromPtr(scan), scan[0].header });
        scan = scan[0].skipForward();
    }
}
pub fn format(
    self: *Process,
    writer: anytype,
) !void {
    try writer.print("process: {} .stack = {any}", .{ self.id, self.getStack(self.sp) });
    try writer.print(" .heap = {any}", .{self.getHeap()});
}
pub threadlocal var thisProcess: @This() = undefined;
pub inline fn check(_: *Process, prim: anytype) @TypeOf(prim) {
    return prim;
}
const Debugger = struct {
    var buf: [10]u8 = undefined;
    var stdin = std.fs.File.stdin().reader(&buf);
    var in = &stdin.interface; // must be separate bc @fieldParentPtr. Thanks @Freakman
    fn step(pc: PC, sp: SP, _: *Process, context: *Context, extra: Extra) Result {
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
pub inline fn getSp(_: *Process) SP {
    return thisProcess.sp;
}
pub inline fn setSp(_: *Process, sp: SP) void {
    thisProcess.sp = sp;
}
pub inline fn getContext(_: *Process) *Context {
    return thisProcess.context;
}
pub inline fn setContext(_: *Process, context: *Context) void {
    thisProcess.context = context;
}
pub inline fn freeStack(_: *Process, sp: SP) usize {
    return (@intFromPtr(sp) - @intFromPtr(&thisProcess)) / 8;
}

pub inline fn getHeap(_: *Process) []HeapObject {
    return thisProcess.currHeap[0..((@intFromPtr(thisProcess.currHp) - @intFromPtr(thisProcess.currHeap)) / @sizeOf(Object))];
}
pub fn allocArray(_: *Process, slice: []const Object, sp: SP, context: *Context) HeapObjectArray {
    const len: u11 = @intCast(slice.len);
    const hop = thisProcess.allocSpace(len, sp, context);
    HeapHeader.objectInNursery(.Array, .directIndexed, len).storeAt(hop);
    const target: HeapObjectArray = @ptrCast(hop);
    @memcpy(target + 1, @as([]const HeapObject, @ptrCast(slice)));
    return target;
}
fn allocSpace(_: *Process, size: u11, sp: SP, context: *Context) *HeapObject {
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
    thisProcess.initProcess();
    const endSp = thisProcess.endOfStack();
    try ee(1016, @intFromPtr(endSp) & stack_mask);
    try ee(endSp.endOfStack(), thisProcess.endOfStack());
    try ee(endSp.reserve(1).?.endOfStack(), thisProcess.endOfStack());
    try ee(endSp.reserve(10).?.endOfStack(), thisProcess.endOfStack());
}
test "nursery allocation" {
    const ee = std.testing.expectEqual;
    thisProcess.initProcess();
    const emptySize = process_nursery_size();
    try ee(127, process_stack_size());
    try ee(emptySize, thisProcess.freeNursery());
    var sp = thisProcess.endOfStack();
    const initialContext = thisProcess.getContext();
    var ar = sp.alloc(initialContext, ClassIndex.Class, 4, null, void, false);
    _ = ar.initAll();
    const o1 = ar.allocated;
    try ee(emptySize - 5, thisProcess.freeNursery());
    ar = sp.alloc(initialContext, ClassIndex.Class, 5, null, void, false);
    _ = ar.initAll();
    ar = sp.alloc(initialContext, ClassIndex.Class, 6, null, void, false);
    const o2 = ar.initAll();
    try ee(emptySize - 19, thisProcess.freeNursery());
    try o1.instVarPut(0, o2.asObject());
    sp = sp.push(o1.asObject()).?;
    const news, const newContext, _ = sp.spillStack(initialContext, Extra.none);
    try ee(sp, news);
    try ee(initialContext, newContext);
    thisProcess.collectNursery(sp, initialContext, 0);
    try ee(emptySize - switch (config.objectEncoding) {
        .zag => 0,
        .nan => 5,
        else => 7,
    }, thisProcess.freeNursery());
    // age test
    // o1 still contains corrected address of o2
    // add second reference to o2 and circulare ref to o1
    // sp.top should be updated
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
            if (newP < @intFromPtr(thisProcess.stack)) {
                @branchHint(.unlikely);
                return null;
            }
            return @ptrFromInt(newP);
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
        return (@intFromPtr(other) ^ @intFromPtr(self)) >> stack_mask_shift == 0;
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
    pub inline fn getProcess(_: SP) *Self {
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
                thisProcess.collectNursery(self, context, size + 1);
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
        thisProcess.dumpHeap();
        thisProcess.collectNursery(sp, context, size);
        thisProcess.dumpHeap();
        const stackToCopy = sp.sliceTo(context.endOfStack(sp));
        sp.dumpStack("original stack in spillStack <<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<<", context, extra);
        context.reify(sp);
        sp.dumpStack("reified stack in spillStack  ===========================================", context, extra);
        const newContext = thisProcess.copyObjectAndDependents(context);
        sp.dumpStack("copied stack in spillStack   >>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>>", context, extra);
        thisProcess.dumpHeap();
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
        self: *const @This(),
        writer: anytype,
    ) !void {
        try writer.print("stack(0x{x}): .top = {f} .next = {f}", .{ @intFromPtr(self), self.top, self.next });
    }
};

test "Stack" {
    thisProcess.initProcess();
    const sp = thisProcess.getSp();
    const context = thisProcess.getContext();
    const ee = std.testing.expectEqual;
    const sp0 = sp.push(True()).?;
    const sp1 = sp0.push(False()).?;
    try ee(True(), sp1.next);
    try ee(False(), sp1.top);
    _ = sp1.drop().push(Object.from(42, sp1, context));
    try config.skipForDebugging();
    try ee(sp1.top.to(i64), 42);
}
pub const threadedFunctions = struct {
    pub const pushThisProcess = struct {
        pub fn threadedFn(pc: PC, sp: SP, _: *Process, context: *Context, extra: Extra) Result {
            if (sp.push(Object.fromAddress(&thisProcess))) |newSp| {
                return @call(tailCall, thisProcess.check(pc.prim()), .{ pc.next(), newSp, &thisProcess, context, extra });
            } else {
                @panic("StackOverflow");
            }
        }
    };
    // pub const debug = struct {
    //     pub fn threadedFn(pc: PC, sp: SP, _: *Process, context: *Context, extra: Extra) Result {
    //         const newProcess = thisProcess.singleStep();
    //         return @call(tailCall, newProcess.check(pc.prim()), .{ pc.next(), sp, newProcess, context, extra });
    //     }
    // };
    // pub const enddebug = struct {
    //     pub fn threadedFn(pc: PC, sp: SP, _: *Process, context: *Context, extra: Extra) Result {
    //         const newProcess = thisProcess.unSingleStep();
    //         return @call(tailCall, newProcess.check(pc.prim()), .{ pc.next(), sp, newProcess, context, extra });
    //     }
    // };
};
