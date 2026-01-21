const std = @import("std");
const math = std.math;
const assert = std.debug.assert;
const mem = std.mem;
const builtin = @import("builtin");
const zag = @import("zag.zig");
const config = zag.config;
const tailCall = config.tailCall;
const trace = config.trace;
const SeqCst = std.builtin.AtomicOrder.seq_cst;
const object = zag.object;
const Object = object.Object;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const ClassIndex = object.ClassIndex;
const checkEqual = zag.utilities.checkEqual;
//const dispatch = @import("dispatch.zig");
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

/// this is really a Process object with the low bits encoding additional information
const Self = @This();
const process_total_size = config.process_total_size;
m: [process_total_size]u8 align(1), // alignment explicitly stated to emphasize the difference from Process
pub const alignment = @max(stack_mask_overflow * 2, flagMask + 1);
const alignment_mask = @as(u64, @bitCast(-@as(i64, alignment)));
const stack_mask_overflow: usize = zag.utilities.largerPowerOf2(Process.stack_size * @sizeOf(Object));
pub const stack_mask = stack_mask_overflow - @sizeOf(Object);
pub const stack_mask_shift = @ctz(stack_mask_overflow);
pub const process_stack_size = Process.stack_size;
pub const process_nursery_size = Process.nursery_size;
const Process = struct {
    stack: [stack_size]Object align(alignment),
    h: Fields,
    staticContext: Context,
    _fill: [fill_size]u64,
    nursery0: [nursery_size]HeapObject,
    nursery1: [nursery_size]HeapObject,
    const Fields = struct {
        next: ?*Self,
        id: u64,
        context: *Context,
        trapContextNumber: u64,
        debugFn: ?*const fn (programCounter: PC, stackPointer: SP, process: *Self, context: *Context, signature: Extra) Result,
        sp: SP,
        currHeap: HeapObjectArray,
        currHp: HeapObjectArray,
        currEnd: HeapObjectArray,
        otherHeap: HeapObjectArray,
        singleStepping: bool,
        foo: [3]u64,
    };
    const processAvail = (process_total_size - @sizeOf(Fields) - @sizeOf(Context)) / @sizeOf(Object);
    const approx_nursery_size = (processAvail - processAvail / 16) / 2;
    const approx_stack_size = processAvail - approx_nursery_size * 2;
    const stack_size: usize = zag.utilities.largerPowerOf2(approx_stack_size) - 1;
    const nursery_size = (processAvail - stack_size) / 2;
    const fill_size = processAvail - stack_size - nursery_size * 2;
    comptime {
        // @compileLog("Process size: ", @sizeOf(Process));
        // @compileLog("process_total_size: ", process_total_size);
        // @compileLog("@sizeOf(Fields)", @sizeOf(Fields));
        // @compileLog("@sizeOf(Context)", @sizeOf(Context));
        // @compileLog("processAvail:", processAvail);
        // @compileLog("approx_nursery_size:", approx_nursery_size);
        // @compileLog("stack_size:", stack_size);
        // @compileLog("nursery_size:", nursery_size);
        // @compileLog("alignment:", alignment);
        // @compileLog("stack_mask_overflow:", stack_mask_overflow);
        assert(stack_size <= nursery_size);
    }
    const maxNurseryObjectSize = @min(HeapHeader.maxLength, nursery_size / 4);

    fn collectNursery(self: *Process, sp: SP, context: *Context, need: usize) void {
        assert(need <= Process.nursery_size);
        var ageSizes = [_]usize{0} ** Age.lastNurseryAge;
        self.collectNurseryPass(sp, context, &ageSizes, Age.lastNurseryAge + 1);
        if (self.freeNursery() >= need) return;
        var total: usize = 0;
        var age = Age.lastNurseryAge;
        while (age >= 0) : (age -= 1) {
            total += ageSizes[age];
            if (total >= need) {
                self.collectNurseryPass(sp, context, &ageSizes, age);
                return;
            }
        }
        @panic("Insufficient nursery space");
    }
    fn collectNurseryPass(self: *Process, originalSp: SP, originalContext: *Context, sizes: []usize, promoteAge: usize) void {
        const hp = self.collectStack(originalSp, originalContext, sizes, promoteAge);
        self.finishCollection(hp, self.h.otherHeap, sizes, promoteAge);
    }
    fn collectStack(self: *Process, originalSp: SP, originalContext: *Context, sizes: []usize, promoteAge: usize) HeapObjectArray {
        _ = .{ sizes, promoteAge };
        var hp = self.h.otherHeap;
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
    fn copyObject(self: *Process, obj: anytype) @TypeOf(obj) {
        const result = self.h.currHp;
        const hp = @as(*HeapObject, @ptrCast(result)).copyTo(result, null);
        var sizes = [_]usize{0} ** Age.lastNurseryAge;
        self.finishCollection(hp, result, &sizes, 0);
        return @ptrCast(result);
    }
    fn finishCollection(self: *Process, startingHp: HeapObjectArray, startingScan: HeapObjectArray, sizes: []usize, promoteAge: usize) void {
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
        const head = &self.h;
        const tempHeap = head.otherHeap;
        head.otherHeap = head.currHeap;
        head.currHeap = tempHeap;
        head.currHp = hp;
        head.currEnd = tempHeap + Process.nursery_size;
        for (head.otherHeap[0..Process.nursery_size]) |*obj| {
            obj.* = undefined;
        }
    }
    inline fn freeNursery(self: *Process) usize {
        return (@intFromPtr(self.h.currEnd) - @intFromPtr(self.h.currHp)) / 8;
    }
    fn dumpHeap(self: *Process) void {
        var scan = self.h.currHeap;
        const hp = self.h.currHp;
        while (@intFromPtr(scan) < @intFromPtr(hp)) {
            std.debug.print("[{x:0>10}]: {f}\n", .{ @intFromPtr(scan), scan[0].header });
            scan = scan[0].skipForward();
        }
    }
};
pub fn format(
    orig: *const @This(),
    writer: anytype,
) !void {
    const self = orig.ptr();
    try writer.print("process: {} .stack = {any}", .{ orig.header().id, orig.getStack(self.h.sp) });
    try writer.print(" .heap = {any}", .{orig.getHeap()});
}
comptime {
    assert(process_total_size == @sizeOf(Process));
    assert(@offsetOf(Process, "stack") == 0);
}
var allProcesses: ?*Self = null;
inline fn ptr(self: *align(1) const Self) *Process {
    return @ptrFromInt(@intFromPtr(self) & alignment_mask);
}
inline fn header(self: *align(1) const Self) *Process.Fields {
    return &self.ptr().h;
}
pub fn new() align(alignment) Self {
    return undefined;
}
pub fn init(origin: *align(alignment) Self) void {
    const self = origin.ptr();
    self.h.sp = origin.endOfStack();
    self.h.currHeap = HeapObject.fromObjectPtr(@ptrCast(&self.nursery0));
    self.h.currEnd = self.h.currHeap + Process.nursery_size;
    self.h.currHp = self.h.currHeap;
    self.h.otherHeap = HeapObject.fromObjectPtr(@ptrCast(&self.nursery1));
    self.h.context = &self.staticContext;
    self.staticContext.initStatic();
    self.h.trapContextNumber = 0;
    self.h.singleStepping = false;
    while (true) {
        self.h.next = allProcesses;
        self.h.id = if (allProcesses) |p| p.header().id + 1 else 1;
        if (@cmpxchgWeak(?*Self, &allProcesses, self.h.next, origin, SeqCst, SeqCst) == null) break;
    }
}
pub fn deinit(self: *align(1) Self) void {
    self.ptr().* = undefined;
}
fn freeNursery(self: *align(1) Self) usize {
    return self.ptr().freeNursery();
}
const countType = u5;
const countMask: usize = math.maxInt(countType);
const countOverflowFlag = countMask + 1;
const nonCount = ~(countOverflowFlag + countMask);
const othersFlag = countOverflowFlag << 1;
const checkFlags = othersFlag | countOverflowFlag;
const flagMask = checkFlags | countMask;
const nonFlags = ~flagMask;
pub inline fn check(self: *align(1) const Self, next: *const fn (PC, SP, *Self, *Context, Extra) Result) *const fn (PC, SP, *Self, *Context, Extra) Result {
    return if (config.singleSteppable and self.needsCheck()) &fullCheck else next;
}
pub inline fn branchCheck(self: *align(1) const Self, next: *const fn (PC, SP, *Self, *Context, Extra) Result) *const fn (PC, SP, *Self, *Context, Extra) Result {
    return if (self.needsCheck()) &fullCheck else next;
}
inline fn needsCheck(self: *align(1) const Self) bool {
    return (@intFromPtr(self) & checkFlags) != 0;
}
fn fullCheck(pc: PC, sp: SP, process: *align(1) Self, context: *Context, extra: Extra) Result {
    trace("fullCheck: {f} {}", .{ extra, process.header().singleStepping });
    // if (process.header().singleStepping)
    //     return @call(tailCall, Debugger.step, .{ pc, sp, process, context, extra });
    return @call(tailCall, pc.prev().prim(), .{ pc, sp, process, context, extra });
}
pub inline fn checkBump(self: *align(1) Self) *align(1) Self {
    if (self.needsCheck()) return self;
    return @ptrFromInt(@intFromPtr(self) + 1);
}
pub inline fn maxCount(self: *align(1) const Self) *align(1) Self {
    return @ptrFromInt(@intFromPtr(self) | countMask);
}
pub inline fn clearCount(self: *align(1) const Self) *align(1) Self {
    return @ptrFromInt(@intFromPtr(self) & nonCount);
}
pub inline fn singleStep(self: *align(1) const Self) *align(1) Self {
    self.header().singleStepping = true;
    return @ptrFromInt(@intFromPtr(self) | othersFlag);
}
pub inline fn unSingleStep(self: *align(1) const Self) *align(1) Self {
    self.header().singleStepping = false;
    return @ptrFromInt(@intFromPtr(self) & ~othersFlag);
}
const Debugger = struct {
    var buf: [10]u8 = undefined;
    var stdin = std.fs.File.stdin().reader(&buf);
    var in = &stdin.interface; // must be separate bc @fieldParentPtr. Thanks @Freakman
    fn step(pc: PC, sp: SP, process: *align(1) Self, context: *Context, extra: Extra) Result {
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
        return @call(tailCall, primitive, .{ pc, sp, process, context, extra });
    }
};
pub inline fn endOfStack(self: *align(1) const Self) SP {
    return @ptrCast(@as([*]Object, @ptrCast(&self.ptr().stack[0])) + Process.stack_size);
}
pub inline fn getSp(self: *align(1) const Self) SP {
    return self.header().sp;
}
pub inline fn setSp(self: *align(1) Self, sp: SP) void {
    self.header().sp = sp;
}
pub inline fn getContext(self: *align(1) const Self) *Context {
    return self.header().context;
}
pub inline fn setContext(self: *align(1) Self, context: *Context) void {
    self.header().context = context;
}
pub inline fn freeStack(self: *align(1) const Self, sp: SP) usize {
    return (@intFromPtr(sp) - @intFromPtr(self.ptr())) / 8;
}

pub inline fn getHeap(self: *align(1) const Self) []HeapObject {
    return self.header().currHeap[0..((@intFromPtr(self.header().currHp) - @intFromPtr(self.header().currHeap)) / @sizeOf(Object))];
}
pub fn allocArray(self: *align(1) Self, slice: []const Object, sp: SP, context: *Context) HeapObjectArray {
    const len: u11 = @intCast(slice.len);
    const hop = self.allocSpace(len, sp, context);
    hop.header.objectInNursery(.Array, len);
    const target: HeapObjectArray = @ptrCast(hop);
    @memcpy(target + 1, @as([]const HeapObject, @ptrCast(slice)));
    return target;
}
fn allocSpace(self: *align(1) Self, size: u11, sp: SP, context: *Context) *HeapObject {
    const head = self.header();
    const result = head.currHp;
    const newHp = result + size + 1;
    if (@intFromPtr(newHp) <= @intFromPtr(head.currEnd)) {
        head.currHp = newHp;
        return @ptrCast(result);
    }
    _ = .{ sp, context, unreachable };
}
pub fn resetForTest() void {
    allProcesses = null;
}
test "stack operations" {
    const ee = std.testing.expectEqual;
    var process: Self align(alignment) = undefined;
    process.init();
    const endSp = process.endOfStack();
    try ee(120, @intFromPtr(endSp) & stack_mask);
    try ee(endSp.endOfStack(), process.endOfStack());
    try ee(endSp.reserve(1).?.endOfStack(), process.endOfStack());
    try ee(endSp.reserve(10).?.endOfStack(), process.endOfStack());
}
test "nursery allocation" {
    const ee = std.testing.expectEqual;
    var process: Self align(alignment) = undefined;
    process.init();
    const emptySize = Process.nursery_size;
    try ee(15, Process.stack_size);
    try ee(emptySize, process.freeNursery());
    var sp = process.endOfStack();
    const initialContext = process.getContext();
    var ar = sp.alloc(initialContext, ClassIndex.Class, 4, null, void, false);
    _ = ar.initAll();
    const o1 = ar.allocated;
    try ee(emptySize - 5, process.freeNursery());
    ar = sp.alloc(initialContext, ClassIndex.Class, 5, null, void, false);
    _ = ar.initAll();
    ar = sp.alloc(initialContext, ClassIndex.Class, 6, null, void, false);
    const o2 = ar.initAll();
    try ee(emptySize - 19, process.freeNursery());
    try o1.instVarPut(0, o2.asObject());
    sp = sp.push(o1.asObject()).?;
    const news, const newContext, _ = sp.spillStack(initialContext, Extra.none);
    try ee(sp, news);
    try ee(initialContext, newContext);
    process.ptr().collectNursery(sp, initialContext, 0);
    try ee(emptySize - switch (config.objectEncoding) {
        .zag, .nan => 12,
        else => 7,
    }, process.freeNursery());
    // age test
    // o1 still contains corrected address of o2
    // add second reference to o2 and circulare ref to o1
    // sp.top should be updated
}
test "check flag" {
    const testing = std.testing;
    var process: Self align(alignment) = undefined;
    process.init();
    var pr: *Self align(1) = &process;
    try testing.expect(!pr.needsCheck());
    const origEOS = pr.endOfStack();
    try testing.expect(!pr.needsCheck());
    for (0..countMask) |_| {
        pr = pr.checkBump();
    }
    try testing.expect(!pr.needsCheck());
    try testing.expectEqual(pr.endOfStack(), origEOS);
    pr = pr.checkBump();
    try testing.expect(pr.needsCheck());
    pr = pr.clearCount().maxCount();
    try testing.expect(!pr.needsCheck());
    pr = pr.checkBump();
    try testing.expect(pr.needsCheck());
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
        }
        const newP = @intFromPtr(self) - @sizeOf(Object) * n;
        if (newP < @intFromPtr(&self.theProcess().stack)) {
            @branchHint(.unlikely);
            return null;
        }
        return @ptrFromInt(newP);
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
    pub inline fn dumpStack(self: SP, why: []const u8, context: *Context, extra: Extra) void {
        std.debug.print("dumpStack ({s})\n", .{why});
        const selfAddr = extra.selfAddress(self) orelse context.selfAddress(self);
        for (self.getStack()) |*obj| {
            const addr = @intFromPtr(obj);
            std.debug.print("[{x:0>10}]: {f}{s}{s}{s}\n",
                .{ addr, obj.*,
                   if (addr == @intFromPtr(self)) " <--sp" else "",
                   if (addr == @intFromPtr(context)) " <--ctx" else "",
                   if (addr == @intFromPtr(selfAddr)) " <--self" else "" });
        }
    }
    pub inline fn traceStack(self: SP, why: []const u8, context: *Context, extra: Extra) void {
        if (!config.show_trace) return;
        trace("traceStack ({s})", .{why});
        const selfAddr = extra.selfAddress(self) orelse context.selfAddress(self);
        for (self.getStack()) |*obj| {
            const addr = @intFromPtr(obj);
            trace("[{x:0>10}]: {f}{s}{s}{s}",
                .{ addr, obj.*,
                   if (addr == @intFromPtr(self)) " <--sp" else "",
                   if (addr == @intFromPtr(context)) " <--ctx" else "",
                   if (addr == @intFromPtr(selfAddr)) " <--self" else "" });
        }
    }
    inline fn theProcess(self: SP) *Process {
        return @ptrFromInt(@intFromPtr(self) & alignment_mask);
    }
    pub fn trapContextNumber(self: SP) u64 {
        return self.theProcess().h.trapContextNumber;
    }
    pub inline fn endOfStack(self: SP) SP {
        return @ptrFromInt((@intFromPtr(self) | stack_mask));
    }
    pub fn alloc(self: SP, context: *Context, classIndex: ClassIndex, iVars: u11, indexed: ?usize, comptime element: type, makeWeak: bool) AllocResult {
        const aI = allocationInfo(iVars, indexed, element, makeWeak);
        const process = self.theProcess();
        if (aI.objectSize(Process.maxNurseryObjectSize)) |size| {
            for (0..2) |_| {
                //                if (true) @panic("here 490");
                const result = HeapObject.alignProperBoundary(process.h.currHp);
                const newHp = result + size + 1;
                if (@intFromPtr(newHp) <= @intFromPtr(process.h.currEnd)) {
                    process.h.currHp = newHp;
                    const obj: *HeapObject = @ptrCast(result);
                    aI.initObjectStructure(obj, classIndex, .nursery);
                    return .{
                        .age = .nursery,
                        .allocated = obj,
                        .info = aI,
                    };
                }
                process.collectNursery(self, context, size + 1);
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
        const process = sp.theProcess();
        const size = (@intFromPtr(sp.endOfStack()) - @intFromPtr(sp)) / @sizeOf(Object);
        process.collectNursery(sp, context, size);
        const stackToCopy = sp.sliceTo(context.endOfStack(sp));
        context.reify(sp);
        sp.dumpStack("in spillStack", context, extra);
        const newContext = process.copyObject(context);
        var n = stackToCopy.len;
        const newSp = @as([*]Stack, @ptrCast(sp.endOfStack())) - n;
        const targetStack = @as(SP, @ptrCast(newSp)).slice(n);
        while (n > 0) : (n -= 1) {
            targetStack[n - 1] = stackToCopy[n - 1];
        }
        sp.dumpStack("at end of spillStack", newContext, extra);
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
    var process: Self align(alignment) = undefined;
    process.init();
    const sp = process.getSp();
    const context = process.getContext();
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
        pub fn threadedFn(pc: PC, sp: SP, process: *Self, context: *Context, extra: Extra) Result {
            if (sp.push(Object.fromAddress(process.ptr()))) |newSp| {
                return @call(tailCall, process.check(pc.prim()), .{ pc.next(), newSp, process, context, extra });
            } else {
                @panic("StackOverflow");
            }
        }
    };
    pub const debug = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Self, context: *Context, extra: Extra) Result {
            const newProcess = process.singleStep();
            return @call(tailCall, newProcess.check(pc.prim()), .{ pc.next(), sp, newProcess, context, extra });
        }
    };
    pub const enddebug = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Self, context: *Context, extra: Extra) Result {
            const newProcess = process.unSingleStep();
            return @call(tailCall, newProcess.check(pc.prim()), .{ pc.next(), sp, newProcess, context, extra });
        }
    };
};
