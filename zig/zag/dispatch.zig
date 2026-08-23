//! Method dispatch tables and signature-based lookup.

const std = @import("std");
const assert = std.debug.assert;
const expectEqual = std.testing.expectEqual;

const smallestPrimeAtLeast = @import("utilities.zig").smallestPrimeAtLeast;
const zag = @import("zag.zig");
const config = zag.config;
const trace = config.trace;
const tailCall = config.tailCall;
const object = zag.object;
const Object = object.Object;
const True = object.True;
const False = object.False;
const ClassIndex = object.ClassIndex;
const execute = zag.execute;
const PC = execute.PC;
const Result = execute.Result;
const Signature = execute.Signature;
const Execution = execute.Execution;
const CompiledMethod = execute.CompiledMethod;
const Process = zag.Process;
const Context = zag.Context;
const Extra = Context.Extra;
const globalArena = zag.globalArena;
const symbol = zag.symbol;
const symbols = symbol.Symbols;
const HeapHeader = zag.heap.HeapHeader;
var n_classes: u16 = config.max_classes;
const SP = Process.SP;

const SIMD_bytes = 64; // bytes that can be handled in 1 cycle
const DispatchPtr = *align(SIMD_bytes) Dispatch;

const o0 = object.testObjects[0];
// note that self and other could become invalid after any method call if they are heap objects, so will need to be re-loaded from context.fields if needed thereafter

pub const lookupMethodForClass = DispatchHandler.lookupMethodForClass;
pub const addMethod = DispatchHandler.addMethod;
pub const fail = threadedFunctions.fail;
const static_classes = config.max_classes > 0;
const DispatchHandler = struct {
    var dispatches: if (static_classes) [config.max_classes]DispatchPtr else [*]Dispatch =
        if (static_classes) [_]DispatchPtr{&Dispatch.empty} ** config.max_classes else undefined;
    inline //
    fn lookupMethodForClass(ci: ClassIndex, signature: Signature) *const CompiledMethod {
        if (dispatches[@intFromEnum(ci)].lookupMethod(signature)) |method|
            return method;
        return @call(.never_inline, loadMethodForClass, .{ ci, signature });
    }
    fn loadMethodForClass(ci: ClassIndex, signature: Signature) *const CompiledMethod {
        if (defaultForTest != void)
            return defaultForTest.loadMethodForClass(ci, signature);
        std.log.err("Class: {} signature: {f}) - ", .{ ci, signature });
        @panic("Method not found");
    }
    fn stats(index: ClassIndex) Dispatch.Stats {
        return dispatches[@intFromEnum(index)].stats();
    }
    fn addMethod(ci: ClassIndex, method: *const CompiledMethod) void {
        const index = @intFromEnum(ci);
        if (index > n_classes) {
            trace("addMethod: index {} exceeds n_classes {}", .{ index, n_classes });
            @panic("addMethod: index exceeds n_classes");
        }
        trace("addMethod({f} {}) {} {*}", .{ method.signature, method.signature.fullHash(), index, dispatches[index] });
        if (dispatches[index].addIfAllocated(method)) return;
        while (true) {
            const dispatch = @atomicLoad(DispatchPtr, &dispatches[index], .acquire);
            if (dispatch.state.lockTry()) {
                // Automatically runs on BOTH 'continue' (mismatch) and 'return' (success)
                defer dispatch.retire();

                // Double-check: if swapped before locking, 'continue' triggers defer and restarts
                if (@atomicLoad(DispatchPtr, &dispatches[index], .monotonic) != dispatch) {
                    continue;
                }
                var numMethods: usize = 3;
                while (true) {
                    numMethods = @max(numMethods, dispatch.nMethods + 1) * 3 / 2;
                    const newDispatch: DispatchPtr = @alignCast(alloc(numMethods));
                    if (dispatch.addMethodsTo(newDispatch, method)) {
                        @atomicStore(DispatchPtr, &dispatches[index], newDispatch, .release);
                        return; // triggers defer and returns cleanly
                    }
                }
            } else {
                std.atomic.spinLoopHint();
            }
        }
    }
    fn requiredSpace(nMethods: usize) usize {
        const keysPlusHeader = (16 + nMethods * @sizeOf(u32) + 63) / 64 * 16;
        return keysPlusHeader - 4 + keysPlusHeader / 2;
    }
    fn alloc(nMethods: usize) DispatchPtr {
        const nInstVars = Dispatch.requiredSpace(nMethods) - 1;
        //(DispatchElement.size(nMethods) + @offsetOf(Dispatch, "matches")) / @sizeOf(Object) - 1;
        const aR = globalArena.aHeapAllocator().alloc(.Dispatch, @intCast(nInstVars), null, Object, false);
        const newDispatch: DispatchPtr = @ptrCast(@alignCast(aR.allocated));
        newDispatch.initialize(nMethods);
        return newDispatch;
    }
};
const DispatchState = enum(u32) {
    clean,
    beingUpdated,
    dead,

    inline fn unlock(self: *@This()) void {
        @atomicStore(DispatchState, self, .clean, .release);
    }

    inline fn kill(self: *@This()) void {
        @atomicStore(DispatchState, self, .dead, .release);
    }

    inline fn lockSpin(self: *@This()) void {
        while (@cmpxchgWeak(DispatchState, self, .clean, .beingUpdated, .acquire, .monotonic)) |notClean| {
            if (notClean == .dead) @panic("DeadDispatch");
            // Spin hint for the CPU
            std.atomic.spinLoopHint();
        }
    }

    // this has the potential for a false negative
    // so must be used where that is OK (in other words, where we will retry)
    // the else arm of the test should use `std.atomic.spinLoopHint();` to prevent a huge performance hit
    inline fn lockTry(self: *@This()) bool {
        if (@cmpxchgWeak(DispatchState, self, .clean, .beingUpdated, .acquire, .monotonic)) |_| {
            return false;
        }
        return true;
    }
};
comptime {
    std.debug.assert(@offsetOf(Dispatch, "header") == 0);
    std.debug.assert(@offsetOf(Dispatch, "matches") == 16);
}
const Dispatch = extern struct {
    header: HeapHeader,
    nMethods: u16,
    nAllocated: u16,
    state: DispatchState,
    matches: [1]D.Element, // this is just the empty size... normally a larger array
    const Self = @This();
    const D = DispatchSIMD;
    const lookupMethod = D.lookupMethod;
    const addIfAllocated = D.addIfAllocated;
    var empty: Dispatch align(SIMD_bytes) = .{
        // don't count header, but do count one element of methods
        .header = HeapHeader.staticHeaderWithClassStructHash(.Dispatch, Self, 0),
        .nMethods = 0,
        .nAllocated = 0,
        .state = .clean,
        .matches = .{D.empty},
    };
    inline fn retire(self: DispatchPtr) void {
        // If nMethods == 0 (or self == &Dispatch.empty), unlock for reuse.
        // empty is the only dispatch table that will have a nMethods == 0
        // Otherwise, mark the superseded table as dead.
        if (self.nMethods == 0) {
            self.state.unlock();
        } else {
            self.state.kill();
        }
    }
    const Stats = struct {
        total: usize,
        active: usize,
        nMethods: usize,
        percent: usize,
    };
    fn stats(self: *Self) Stats {
        var total: usize = 0;
        var active: usize = 0;
        for (self.methodsAllocatedSlice()) |de| {
            total += 1;
            if (!de.isEmpty()) active += 1;
        }
        return .{ .total = total, .active = active, .nMethods = self.nMethods, .percent = active * 100 / @max(total, 1) };
    }
    fn initialize(self: *Self, nMethods: usize) void {
        self.state = .clean;
        self.nAllocated = nMethods;
        D.initialize(self);
        for (self.methodsAllocatedSlice()) |*p|
            p.* = empty;
    }
    inline //
    fn methods(self: *const Self) [*]D.Element {
        return @as([*]D.Element, @ptrCast(@alignCast(@constCast(&self.matches))));
    }
    inline //
    fn methodSlice(self: *Self) []D.Element {
        return self.methods()[0..self.nMethods];
    }
    inline //
    fn methodsAllocatedSlice(self: *Self) []D.Element {
        return self.methods()[0..self.nAllocated];
    }
    fn addMethodsTo(self: *Self, newDispatch: *Self, method: *const CompiledMethod) bool {
        for (self.methodSlice()) |de| {
            if (de.activeMethod()) |ptr|
                if (!newDispatch.add(ptr)) return false;
        }
        return newDispatch.add(method);
    }
};
const DispatchSIMD = struct {
    const Self = Dispatch;
    const intermingled = false;
    const doHash = false;
    const Element = if (intermingled) u128 else u32;
    const empty: Element = 0;
    inline fn round(n: u16) u16 {
        const VEC_LEN: u16 = SIMD_bytes / @sizeOf(Element);
        return (n & ~VEC_LEN) + VEC_LEN;
    }
    fn initialize(dispatch: DispatchPtr) void {
        dispatch.nMethods = round(0);
    }
    fn addIfAllocated(dispatch: DispatchPtr, cmp: *const CompiledMethod) bool {
        if (dispatch.nMethods >= dispatch.nAllocated) return false;
        return setMethod(dispatch, cmp);
    }
    inline fn lookupMethod(
        dispatch: DispatchPtr,
        selector: Signature,
    ) ?*const CompiledMethod {
        // Retain the 64-byte alignment on the multi-pointer
        const base_ptr: [*]align(SIMD_bytes) u32 = @ptrCast(dispatch);

        // Slicing it now yields a slice with type `[]align(SIMD_bytes) u32`
        const base = base_ptr[0..dispatch.nMethods];
        const key = selector.fullHash();
        if (if (intermingled) searchIntermingled(key, 0, base) else search(key, 0, base)) |index|
            return getMethodSlot(base, index, *const CompiledMethod).*;
        return null;
    }
    inline fn setMethod(dispatch: DispatchPtr, method: *const CompiledMethod) void {
        const selector = method.signature;
        const base = @as([*]align(SIMD_bytes) u32, @ptrCast(dispatch))[0..dispatch.nMethods];
        const key = selector.fullHash();
        if (if (intermingled) searchIntermingled(key, 0, base) else search(key, 0, base)) |index| {
            // 1. Get the address of the slot and write the new method pointer
            const slot = getMethodSlot(base, index, *CompiledMethod);
            slot.* = method;

            // 2. Publish the key with RELEASE semantics to ensure slot write is visible first
            @atomicStore(u32, &base[index], key, .release);
        } else @panic("setMethod");
        dispatch.state.unlock();
    }
    // given a slice of the header+keys, return the address of the corresponding method pointer
    inline fn getMethodSlot(base: anytype, k: usize, comptime T: type) *T {
        // Cast to multi-pointer of T
        const methods_ptr: [*]T = @ptrCast(@alignCast(base.ptr + base.len));
        // Return the memory address of the k-th slot
        return &methods_ptr[k - 4];
    }

    // Search using dense keys
    // search to find a key with search(k,k,theSlice)
    // search to find a free spot for a key with `search(k,0,theSlice)
    inline fn search(
        route_hash: anytype, // The hash used to calculate the starting block
        key: @TypeOf(route_hash),
        array: []align(SIMD_bytes) const @TypeOf(key),
    ) ?usize {
        const T = @TypeOf(key);
        const VEC_LEN = SIMD_bytes / @sizeOf(T);
        const Vec = @Vector(VEC_LEN, T);
        const MaskT = std.meta.Int(.unsigned, VEC_LEN);

        const size = array.len;
        const target_vec: Vec = @splat(key);

        const base: [*]align(SIMD_bytes) const T = array.ptr;
        const end = base + size;

        // Comptime calculation: e.g., if ignoreFirst=4, maskFirst is 0xFFF0
        const ignoreFirst = @offsetOf(Dispatch, "matches") / @sizeOf(T);
        const maskFirst: MaskT = @intCast((1 << VEC_LEN) - (1 << ignoreFirst));

        const block_idx = if (doHash) blk: {
            // Subtract 1 to reserve the final block for overflow for hashing
            const num_blocks = (size / VEC_LEN) - 1;
            const DoubleT = std.meta.Int(.unsigned, @bitSizeOf(T) * 2);
            break :blk @as(usize, @intCast((@as(DoubleT, route_hash) * num_blocks) >> @bitSizeOf(T)));
        } else 0;

        var keys = base + (block_idx * VEC_LEN);

        if (keys == base) {
            // --- First Block ---
            const chunk: Vec = @as(*align(SIMD_bytes) const Vec, @ptrCast(keys)).*;
            const match = @as(MaskT, @bitCast(chunk == target_vec)) & maskFirst;

            if (match != 0) {
                // Because keys == base, the offset is 0. Just return the ctz!
                return @ctz(match);
            }
            keys += VEC_LEN;
        }

        // --- Subsequent Blocks ---
        // The hot loop contains zero integer math other than pointer advancing.
        while (@intFromPtr(keys) < @intFromPtr(end)) : (keys += VEC_LEN) {
            const chunk: Vec = @as(*const Vec, @ptrCast(keys)).*;
            const match: MaskT = @bitCast(chunk == target_vec);

            if (match != 0) {
                // This math only executes on the exit path.
                const element_offset = (@intFromPtr(keys) - @intFromPtr(base)) / @sizeOf(T);
                return element_offset + @ctz(match);
            }
        }

        return null;
    }

    // Search using intermingled keys and pointers
    inline fn searchIntermingled(
        dispatch: DispatchPtr,
        selector: anytype,
        target_key: u64,
        return_type: anytype,
    ) return_type {
        const VEC_LEN = SIMD_bytes / (@sizeOf(target_key) + @sizeOf(return_type));
        const Vec = @Vector(VEC_LEN, u64);
        const MaskT = std.meta.Int(.unsigned, VEC_LEN);
        const size = dispatch.nMethods; // always a non-zero multiple of VEC_LEN
        const return_match = return_type == *u64;
        const target_vec: Vec = @splat(target_key);

        const base: [*]const u64 = @ptrCast(dispatch);
        const end = &base[size];
        const offset = if (doHash) Dispatch.getIndex(selector, size - VEC_LEN) * VEC_LEN else 0;

        var keys = base + offset;

        if (keys == base) {
            // --- First Block (Includes Header at slots 0 & 1) ---
            const chunk: Vec = @as(*const Vec, @ptrCast(keys)).*;
            // Mask out odd bits (Values) AND bits 0 & 1 (Header)
            const match = @as(MaskT, @bitCast(chunk == target_vec)) & 0x54;
            if (match != 0)
                return if (return_match) &keys[@ctz(match)] else @ptrFromInt(keys[@ctz(match) + 1]);
            // --- Subsequent Blocks (Pure Key/Value pairs) ---
            keys += VEC_LEN;
        }
        while (@intFromPtr(keys) < @intFromPtr(end)) : (keys += VEC_LEN) {
            const chunk: Vec = @as(*const Vec, @ptrCast(keys)).*;
            // Mask out odd bits (Values)
            const match = @as(MaskT, @bitCast(chunk == target_vec)) & 0x55;
            if (match != 0)
                return if (return_match) &keys[@ctz(match)] else @ptrFromInt(keys[@ctz(match) + 1]);
        }
        return null;
    }
};
const DispatchOriginal = struct {
    const Self = Dispatch;
    const matchSize = DispatchMatch.matchSize;
    const overAllocate = matchSize - 1;
    const Match = DispatchMatch;
    const empty = DispatchMatch.empty;
    const Element = DispatchElement;
    fn initialize(self: *Self, nMethods: usize) void {
        _ = nMethods; // autofix
        for (self.methodsAllocatedSlice()) |*ptr|
            ptr.initUpdateable();
    }
    inline //
    fn lookupMethod(self: *const Self, signature: Signature) ?*const CompiledMethod {
        const dm = self.dispatchMatch(signature);
        return dm.match(signature);
    }
    inline //
    fn dispatchMatch(self: *const Self, signature: Signature) *DispatchMatch {
        const index = getIndex(signature, self.nMethods);
        return @ptrCast(self.methods() + index);
    }
    inline //
    fn getIndex(signature: Signature, size: u64) u64 {
        return signature.fullHash() * size >> 32;
    }
    fn addIfAllocated(self: *Self, cmp: *const CompiledMethod) bool {
        if (self.nMethods == 0) return false;
        return self.add(cmp);
    }
    fn add(self: *Self, cmp: *const CompiledMethod) bool {
        const signature = cmp.signature;
        self.state.lockSpin();
        defer {
            self.state.unlock();
        }
        for (&self.dispatchMatch(signature).elements) |*element| {
            if (element.match(signature)) |_| {
                element.storeMethod(cmp); // replace this
                return true;
            } else if (element.isEmpty()) {
                element.storeMethod(cmp);
                return true;
            }
        }
        return false;
    }
    fn fail(programCounter: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        _ = .{ programCounter, sp, process, context, extra };
        if (programCounter.uint() == 0)
            @panic("called fail function");
        @panic("fail with non-zero next");
    }
    fn testDnu(programCounter: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        _ = .{ programCounter, sp, process, context, extra, @panic("testDnu") };
    }
    fn testGrow(programCounter: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        _ = .{ programCounter, sp, process, context, extra, @panic("testGrow") };
    }
    fn testIncrement(programCounter: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        _ = .{ process, context, extra };
        @as(*usize, @ptrFromInt(programCounter.uint())).* += 1;
        return sp;
    }
};
const DispatchElementType = enum { method, signature, function, simd };
const dispatchElementType = DispatchElementType.method;
const DispatchElement = switch (dispatchElementType) {
    .method => DispatchMethod,
    else => unreachable,
};
const DispatchMethod = struct {
    method: *const CompiledMethod,
    const Self = @This();
    const IntSelf = u64;
    comptime {
        std.debug.assert(@sizeOf(Self) == @sizeOf(IntSelf));
    }
    fn initUpdateable(self: *Self) void {
        self.* = empty;
    }
    fn new(compiledMethod: *const CompiledMethod) Self {
        return .{ .method = compiledMethod };
    }
    const emptyMethod = dummyCompiledMethod(Signature.empty);
    const empty = new(&emptyMethod);
    inline //
    fn cas(self: *Self, replacement: *const CompiledMethod) ?Self {
        const current = self.asInt();
        const replace = new(replacement).asInt();
        if (@cmpxchgWeak(IntSelf, self.asIntPtr(), current, replace, .seq_cst, .seq_cst)) |notClean|
            return @bitCast(notClean);
        return null;
    }
    inline //
    fn storeMethod(self: *Self, replacement: *const CompiledMethod) void {
        self.method = replacement;
    }
    inline //
    fn match(self: *DispatchMethod, signature: Signature) ?*const CompiledMethod {
        const method = self.method;
        if (method.signature.equals(signature))
            return method;
        trace("match {*} {f} {f} ({x} {x})", .{ self, method.signature, signature, @as(u64, @bitCast(method.signature)), @as(u64, @bitCast(signature)) });
        return null;
    }
    inline //
    fn activeMethod(self: *const Self) ?*const CompiledMethod {
        if (self.isEmpty())
            return null;
        return self.method;
    }
    inline //
    fn isEmpty(self: *const Self) bool {
        return self.method == &emptyMethod;
    }
    inline //
    fn asInt(self: Self) IntSelf {
        return @bitCast(self);
    }
    inline //
    fn asIntPtr(self: *Self) *IntSelf {
        return @ptrCast(@alignCast(self));
    }
};
const DispatchMatch = struct {
    elements: [matchSize]DispatchElement,
    const matchSize = 3;
    const empty = DispatchMatch{ .elements = [_]DispatchElement{DispatchElement.empty} ** matchSize };
    inline //
    fn match(self: *DispatchMatch, signature: Signature) ?*const CompiledMethod {
        inline for (&self.elements) |*element| {
            if (element.match(signature)) |method| {
                return method;
            }
        }
        return null;
    }
    inline //
    fn matchOrEmpty(self: *DispatchMatch, signature: Signature) ?*DispatchMethod {
        inline for (&self.elements) |*element| {
            if (element.isEmpty())
                return element;
            if (element.match(signature)) |_|
                return element;
        }
        return null;
    }
};
fn dummyCompiledMethod(signature: Signature) CompiledMethod {
    return .{
        .header = undefined,
        .stackStructure = undefined,
        .executeFn = undefined,
        .jitted = undefined,
        .code = undefined,
        .signature = signature,
    };
}
const nullMethod = dummyCompiledMethod(Signature.empty);
const defaultForTest = if (config.is_test) struct {
    var called: bool = false;
    const dummyMethod = dummyCompiledMethod(Signature.fromNameClass(symbols.value, ClassIndex.Object));
    fn loadMethodForClass(ci: ClassIndex, signature: Signature) *const CompiledMethod {
        called = true;
        _ = .{ ci, signature };
        return &dummyMethod;
    }
    fn reset() void {
        called = false;
    }
} else void;
test "add/lookup" {
    const selector = symbols.@"value:";
    const class = ClassIndex.Object;
    const sig = Signature.fromNameClass(selector, class);
    const emptyMethod = dummyCompiledMethod(sig);
    addMethod(&emptyMethod);
    try std.testing.expectEqual(lookupMethodForClass(class, sig), &emptyMethod);
    const altMethod = dummyCompiledMethod(Signature.fromNameClass(selector, class));
    addMethod(&altMethod);
    try std.testing.expectEqual(lookupMethodForClass(class, sig), &altMethod);
    const stats = DispatchHandler.stats(class);
    try std.testing.expectEqual(1, stats.active);
    try std.testing.expectEqual(5, stats.nMethods);
    try std.testing.expectEqual(7, stats.total);
    defaultForTest.called = false;
    try std.testing.expectEqual(lookupMethodForClass(class, Signature.fromNameClass(symbols.@"new:", class)), &defaultForTest.dummyMethod);
    try std.testing.expectEqual(true, defaultForTest.called);
}
pub const threadedFunctions = struct {
    const tf = zag.threadedFn.Enum;
    pub const returnSelf = struct {
        pub fn threadedFn(_: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            sp.traceStack("returnSelf", context, extra);
            trace("returnSelf extra=0x{x:0>16}", .{@as(u64, @bitCast(extra))});
            if (extra.selfAddress(sp)) |address| {
                const newSp: SP = @ptrCast(address);
                const newExtra = Extra.fromContextData(context.contextDataPtr(sp));
                newSp.traceStack("returnSelf after", context, newExtra);
                return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, newExtra });
            }
            const newSp, const callerContext = context.pop(sp);
            const newExtra = Extra.fromContextData(callerContext.contextDataPtr(sp));
            newSp.traceStack("returnSelf after pop", context, newExtra);
            return @call(tailCall, process.branchCheck(callerContext.getNPc()), .{ callerContext.getTPc(), newSp, process, callerContext, newExtra });
        }
        test {
            if (true) return config.skipForDebugging();
            var exe = Execution.initTest("returnSelf", .{
                tf.pushLiteral,
                91,
                tf.pushLiteral,
                17,
                tf.returnSelf,
                2,
                tf.pushLiteral,
                99,
            });
            try exe.runTest(
                &[_]Object{exe.object(42)},
                &[_]Object{exe.object(42)},
            );
        }
    };
    pub const returnTop = struct {
        pub fn threadedFn(_: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            const top = sp.top;
            sp.traceStack("returnTop", context, extra);
            if (extra.selfAddress(sp)) |address| {
                const newSp: SP = @ptrCast(address);
                newSp.top = top;
                newSp.traceStack("returnTop after", context, extra);
                return @call(tailCall, process.check(context.npc), .{ context.tpc, newSp, process, context, Extra.fromContextData(context.contextDataPtr(sp)) });
            }
            const newSp, const callerContext = context.pop(sp);
            newSp.top = top;
            newSp.traceStack("returnTop after pop", context, extra);
            return @call(tailCall, process.branchCheck(callerContext.npc), .{ callerContext.tpc, newSp, process, callerContext, Extra.fromContextData(callerContext.contextDataPtr(sp)) });
        }
        test {
            if (true) return config.skipForDebugging();
            var exe = Execution.initTest("returnTopNoContext", .{
                tf.pushLiteral,
                91,
                tf.pushLiteral,
                o0,
                tf.returnTop,
                2,
                tf.pushLiteral,
                99,
            });
            try exe.runTest(
                &[_]Object{True()},
                &[_]Object{exe.object(42)},
            );
        }
    };
    pub const returnTopNonLocal = struct {
        pub fn threadedFn(_: PC, _: SP, _: *Process, _: *Context, _: Extra) Result {
            @panic("unreachable");
        }
    };
    inline fn getMethod(pc: PC, signature: Signature, receiver: Object) *const CompiledMethod {
        const class = receiver.which_class();
        const requiredSignature = signature.withClass(class);
        trace("getMethod: {} {f} {f} {f}", .{ class, signature, receiver, requiredSignature });
        if (signature == requiredSignature) {
            return pc.next().method();
        }
        const method = lookupMethodForClass(class, requiredSignature);
        if (@intFromEnum(signature.getClass()) == 0) {
            trace("getMethod: patch {f} {any}", .{ requiredSignature, method });
            pc.patchPtr().patchMethod(requiredSignature, method);
        } else {
            //trace("getMethod: alt {f} {any}", .{ signature, method });
        }
        return method;
    }
    pub const send = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            sp.traceStack("send", context, extra);
            const signature = pc.signature();
            const numArgs = signature.numArgs();
            const selfAddr = sp.unreserve(numArgs);
            const method = getMethod(pc, signature, selfAddr.top);
            trace("method: {f}", .{method});
            const newPc = method.codePc();
            trace("newPc: {f}", .{newPc});
            if (extra.installContextIfNone(sp, process, context)) |new| {
                const newSp = new.sp;
                const newContext = new.context;
                newContext.setReturn(pc.next2());
                const newExtra = Extra.forMethod(method, newSp.unreserve(numArgs));
                trace("newExtra {x} {f}", .{ @as(u64, @bitCast(newExtra)), newExtra });
                newSp.traceStack("send new stack", newContext, newExtra);
                trace("newPc: {f} {?}", .{ newPc, @import("threadedFn.zig").find(method.executeFn) });
                return @call(tailCall, method.executeFn, .{ newPc.next(), newSp, process, newContext, newExtra });
            }
            context.setReturn(pc.next2());
            //method.dump();
            return @call(tailCall, method.executeFn, .{ newPc.next(), sp, process, context, Extra.forMethod(method, selfAddr) });
        }
    };
    pub fn fail(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        sp.traceStack("fail primitive", context, extra);
        const signature = pc.signature();
        const numArgs = signature.numArgs();
        const selfAddr = sp.unreserve(numArgs);
        const method = getMethod(pc, signature, selfAddr.top);
        trace("method: {f}", .{method});
        const newPc = method.codePc();
        trace("newPc: {f}", .{newPc});
        if (extra.installContextIfNone(sp, process, context)) |new| {
            const newSp = new.sp;
            const newContext = new.context;
            newContext.setReturn(pc.next2());
            const newExtra = Extra.forMethod(method, newSp.unreserve(numArgs));
            trace("newExtra {x} {f}", .{ @as(u64, @bitCast(newExtra)), newExtra });
            newSp.traceStack("send new stack", newContext, newExtra);
            trace("newPc: {f} {?}", .{ newPc, @import("threadedFn.zig").find(method.executeFn) });
            return @call(tailCall, method.executeFn, .{ newPc.next(), newSp, process, newContext, newExtra });
        }
        context.setReturn(pc.next2());
        //method.dump();
        return @call(tailCall, method.executeFn, .{ newPc.next(), sp, process, context, Extra.forMethod(method, selfAddr) });
    }
    pub const send0 = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            sp.traceStack("send0", context, extra);
            const signature = pc.signature();
            const method = getMethod(pc, signature, sp.top);
            const newPc = method.codePc();
            if (extra.installContextIfNone(sp, process, context)) |new| {
                const newSp = new.sp;
                const newContext = new.context;
                newContext.setReturn(pc.next2());
                return @call(tailCall, method.executeFn, .{ newPc.next(), newSp, process, newContext, Extra.forMethod(method, newSp) });
            }
            context.setReturn(pc.next2());
            return @call(tailCall, method.executeFn, .{ newPc.next(), sp, process, context, Extra.forMethod(method, sp) });
        }
    };
    pub const tailSend = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            const signature = pc.signature();
            const method = getMethod(pc, signature, sp.at(signature.numArgs()));
            const newPc = method.codePc();
            _ = extra; // have to move parameters to self position
            if (true) @panic("unreachable");
            // return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, Extra.forMethod(method) });
            // const method = tailGetMethod(pc, sp);
            // const newPc = method.codePc();
            return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, Extra.forMethod(method) });
        }
    };
};
