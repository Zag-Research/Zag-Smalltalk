const std = @import("std");
const config = @import("config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const zag = @import("zag.zig");
const checkEqual = zag.utilities.checkEqual;
const Process = zag.Process;
const ProcessPtr = *Process;
const object = zag.object;
const Object = object.Object;
const PackedObject = object.PackedObject;
const ClassIndex = object.ClassIndex;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const Context = zag.Context;
const heap = zag.heap;
const HeapHeader = heap.HeapHeader;
const HeapObject = heap.HeapObject;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
const Format = heap.Format;
const Age = heap.Age;
const globalArena = zag.globalArena;
const HeapAllocationPtr = globalArena.HeapAllocationPtr;
//const class = zag.class;
const symbol = zag.symbol;
const Sym = symbol.symbols;
const phi32 = zag.utilities.inversePhi(u32);
const threadedFn = zag.threadedFn;
const tf = threadedFn.Enum;

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
    fn from(self: anytype) SP {
        return @ptrCast(self);
    }
    pub inline fn push(self: SP, v: Object) SP {
        const newSp = self.reserve(1);
        newSp.top = v;
        return newSp;
    }
    pub inline fn pushRawInt(self: SP, v: u64) SP {
        const newSp = self.reserve(1);
        newSp.top = @bitCast(v);
        return newSp;
    }
    pub inline fn dropPut(self: SP, v: Object) SP {
        self.next = v;
        return self.unreserve(1);
    }
    pub inline fn drop(self: SP) SP {
        return self.unreserve(1);
    }
    pub inline fn reserve(self: SP, n: usize) SP {
        return @ptrFromInt(@intFromPtr(self) - @sizeOf(Object) * n);
    }
    pub inline fn unreserve(self: SP, n: usize) SP {
        return @ptrFromInt(@intFromPtr(self) + @sizeOf(Object) * n);
    }
    pub inline fn delta(self: SP, other: SP) usize {
        return (@intFromPtr(other) - @intFromPtr(self)) / @sizeOf(Object);
    }
    pub inline fn array(self: SP) [*]Object {
        return @ptrCast(self);
    }
    pub inline fn slice(self: SP, n: usize) []Object {
        return self.array()[0..n];
    }
    pub //inline
    fn sliceTo(self: SP, ptr: anytype) []Object {
        const i_ptr = @intFromPtr(ptr);
        return self.slice(((i_ptr - @intFromPtr(self))) / @sizeOf(Object));
    }
    pub inline fn at(self: SP, n: usize) Object {
        return self.array()[n];
    }
    pub inline fn atPut(self: SP, n: usize, o: Object) void {
        self.array()[n] = o;
    }
};
test "Stack" {
    std.debug.print("Test: Stack\n", .{});
    var process: Process align(Process.alignment) = Process.new();
    process.init(Nil());
    const ee = std.testing.expectEqual;
    var stack: [11]Object = undefined;
    const sp0 = @as(SP, @ptrCast(&stack[10]));
    sp0.top = True();
    try ee(True(), stack[10]);
    const sp1 = sp0.push(False());
    try ee(True(), stack[10]);
    try ee(False(), stack[9]);
    _ = sp1.drop().push(Object.from(42, &process));
    try ee(Object.from(42, &process).to(i64), 42);
    try ee(stack[9].to(i64), 42);
}
pub const Extra = union {
    method: *CompiledMethod,
    object: Object,
    signature: Signature,
    contextData: *Context.ContextData,
    pub fn encoded(self: Extra) Extra {
        if (Object.from(self.method, null).tagMethod()) |obj| {
            return .{ .object = obj };
        } else {
            std.debug.print("encoded: {} {x:0>16}\n", .{ self, @intFromPtr(self.method) });
            @panic("weird method");
        }
    }
    pub fn decoded(self: Extra) Extra {
        return .{ .object = self.object.tagMethodValue() };
    }
    pub fn isEncoded(self: Extra) bool {
        @setRuntimeSafety(false);
        return Object.from(self.method, null).isTaggedMethod();
    }
    pub fn primitiveFailed(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        if (config.logThreadExecution)
            std.debug.print("primitiveFailed: {} {}\n", .{ extra, pc });
        return @call(tailCall, process.check(pc.prev().prim()), .{ pc, sp, process, context, extra.encoded() });
    }
    pub fn formatX(
        self: Extra,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = .{ fmt, options };
        switch (self) {
            .method => |m| try writer.print("Extra{{.method = {*}}}", .{m}),
            .object => |o| try writer.print("Extra{{.object = {}}}", .{o}),
            .signature => |s| try writer.print("Extra{{.method = {}}}", .{s}),
            .contextData => |l| try writer.print("Extra{{.contextData = {}}}", .{l}),
        }
    }
};
pub const Result = SP;
pub const ThreadedFn = struct {
    f: Fn,
    pub const Fn = *const fn (programCounter: PC, stackPointer: SP, process: *Process, context: *Context, signature: Extra) Result;
};
pub const Signature = packed struct {
    int: u64,
    const Internal = packed struct {
        selector: u40,
        class: ClassIndex,
        padding: u8 = 0,
    };
    const nil: Signature = .{ .int = @bitCast(Nil) };
    pub fn isNil(self: Signature) bool {
        return self.equals(nil);
    }
    pub fn hash(self: Signature) u32 {
        return @truncate(self.int);
    }
    pub fn from(selector: Object, class: ClassIndex) Signature {
        return .{ .int = @bitCast(Internal{ .selector = selector.symbol40(), .class = class }) };
    }
    pub fn fromNameClass(name: anytype, class: ClassIndex) Signature {
        return .{ .int = @bitCast(Internal{ .selector = @as(u40, name.numArgs()) << 32 | @as(u40, @truncate(name.symbolHash().?)) << 8 | 10 << 3 | 1, .class = class }) };
    }
    fn equals(self: Signature, other: Signature) bool {
        return self.int == other.int;
    }
    pub fn numArgs(self: Signature) u8 {
        return @truncate(self.int >> 32);
    }
    fn isIndexSymbol(self: Signature) bool {
        return self.numArgs() == 0xff;
    }
    fn indexNumber(self: Signature) u24 {
        return @truncate(self.int >> 8);
    }
    fn asObject(self: Signature) Object {
        return @bitCast(self.int);
    }
    fn asSymbol(self: Signature) Object {
        return @bitCast(self.int & 0xffffffffff);
    }
    fn getClassIndex(self: Signature) u64 {
        return self.int >> 40;
    }
    fn getClass(self: Signature) ClassIndex {
        return @as(Internal, @bitCast(self.int)).class;
    }
    fn setClass(self: *Signature, class: ClassIndex) void {
        self.int = (self.int & 0xffffffffff) + (@as(u64, @intFromEnum(class)) << 40);
    }
    pub fn format(
        self: Signature,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = .{ fmt, options };
        try writer.print("Signature({},{})", .{ self.asSymbol(), self.getClass() });
    }
};
pub const PC = packed struct {
    code: *const Code,
    pub const baseType = Code;
    const Self = @This();
    const logging = config.logThreadExecution;
    pub fn init(code: *const Code) PC { // don't inline this as it triggers a zig bug!
        return .{ .code = code };
    }
    pub //inline
    fn packedObject(self: PC) PackedObject {
        if (logging) {
            @setRuntimeSafety(false);
            std.debug.print("PC_packed:      {x:0>16}: {}\n", .{ @intFromPtr(self.code), self.code.packedObject });
            return self.code.packedObject;
        }
        return self.code.packedObject;
    }
    pub //inline
    fn method(self: PC) *CompiledMethod {
        if (logging) {
            @setRuntimeSafety(false);
            std.debug.print("PC_method:      {x:0>16}: {}\n", .{ @intFromPtr(self.code), self.code.method });
            return self.code.method;
        }
        return self.code.method;
    }
    pub //inline
    fn codeAddress(self: PC) *const Code {
        if (logging) {
            @setRuntimeSafety(false);
            std.debug.print("PC_codeAddress: {x:0>16}: {}\n", .{ @intFromPtr(self.code), self.code.codePtr });
            return self.code.codePtr;
        }
        return self.code.codePtr;
    }
    pub //inline
    fn targetPC(self: PC) PC {
        return .{ .code = self.codeAddress() };
    }
    pub inline fn asThreadedFn(self: PC) ThreadedFn {
        return self.code.asThreadedFn();
    }
    pub //inline
    fn prim(self: PC) ThreadedFn.Fn {
        if (logging) {
            @setRuntimeSafety(false);
            std.debug.print("PC_prim:        {x:0>16}: {}\n", .{ @intFromPtr(self.code), @import("threadedFn.zig").find(self.code.threadedFn) });
            return self.code.threadedFn;
        }
        return self.code.threadedFn;
    }
    pub //inline
    fn object(self: PC) Object {
        if (logging) {
            @setRuntimeSafety(false);
            std.debug.print("PC_object:      {x:0>16}: {}\n", .{ @intFromPtr(self.code), self.code.object });
            return self.code.object;
        }
        return self.code.object;
    }
    pub inline fn asCode(self: PC) Code {
        return self.code.*;
    }
    pub inline fn asCodePtr(self: PC) *const Code {
        return self.code;
    }
    pub inline fn structure(self: PC) StackStructure {
        return self.code.structure;
    }
    pub //inline
    fn uint(self: PC) u64 {
        if (logging) {
            @setRuntimeSafety(false);
            std.debug.print("PC_uint:        {x:0>16}: {}\n", .{ @intFromPtr(self.code), self.code.object });
            return self.code.object.to(u64);
        }
        return self.code.object.to(u64);
    }
    pub //inline
    fn int(self: PC) i64 {
        if (logging) {
            @setRuntimeSafety(false);
            std.debug.print("PC_int:         {x:0>16}: {}\n", .{ @intFromPtr(self.code), self.code.object });
            return self.code.object.to(i64);
        }
        return self.code.object.to(i64);
    }
    pub inline fn next(self: PC) PC {
        return asPC(self.array() + 1);
    }
    pub inline fn prev(self: PC) PC {
        return asPC(self.array() - 1);
    }
    pub inline fn prim2(self: PC) ThreadedFn.Fn {
        return self.array()[1].prim();
    }
    pub inline fn next2(self: PC) PC {
        return asPC(self.array() + 2);
    }
    pub inline fn skip(self: PC, n: usize) PC {
        return asPC(self.array() + n);
    }
    pub inline fn returnOffset(self: PC) PC {
        return self.skip(config.picSize * 2);
    }
    pub inline fn offsetFor(self: PC, n: usize) PC {
        return @bitCast(@intFromPtr(@as([*]const Code, @ptrCast(self.code)) + n));
    }
    pub inline fn offsetBytes(self: PC, n: usize) PC {
        return @bitCast(@intFromPtr(self.code) + n);
    }
    pub inline fn back(self: PC, n: usize) PC {
        return @bitCast(@intFromPtr(@as([*]const Code, @ptrCast(self.code)) - n));
    }
    inline fn asPC(ptr: [*]const Code) PC {
        return .{ .code = @ptrCast(ptr) };
    }
    inline fn array(self: PC) [*]const Code {
        return @ptrCast(self.code);
    }
    pub fn format(
        self: PC,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = .{ fmt, options };
        try writer.print("{{PC {x}}}", .{@intFromPtr(self.code)});
    }
};
pub const Code = union {
    object: Object,
    threadedFn: ThreadedFn.Fn,
    method: *CompiledMethod,
    codePtr: *Code,
    offset: i64,
    packedObject: PackedObject,
    structure: StackStructure,
    pub inline fn primOf(pp: ThreadedFn.Fn) Code {
        //@compileLog(pp);
        const result = Code{ .threadedFn = pp };
        return result;
    }
    pub //inline
    fn asObject(self: Code) Object {
        //        @setRuntimeSafety(false);
        return self.object;
    }
    pub inline fn objectOf(o: anytype) Code {
        return Code{ .object = Object.from(o, null) };
    }
    pub inline fn packedObjectOf(po: PackedObject) Code {
        return Code{ .packedObject = po };
    }
    pub inline fn methodOf(method: *CompiledMethod) Code {
        return Code{ .method = method };
    }
    pub inline fn structureOf(structure: StackStructure) Code {
        return Code{ .structure = structure };
    }
    pub inline fn codePtrOf(code: *Code, offs: i64) Code {
        const addr = @as([*]Code, @ptrCast(code)) + 1;
        return Code{ .codePtr = @ptrCast(addr + @as(u64, @bitCast(offs))) };
    }
    pub inline fn prim(self: Code) ThreadedFn.Fn {
        return self.threadedFn;
    }
    pub inline fn asThreadedFn(self: Code) ThreadedFn {
        return .{ .f = self.threadedFn };
    }
    pub fn end(_: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result { // not embedded
        process.setContext(context);
        process.setSp(sp);
        return sp;
    }
    pub fn panic(_: PC, _: SP, _: *Process, _: *Context, _: Extra) Result { // not embedded
        @panic("not implemented");
    }
    fn noOp(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) Result {
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, signature });
    }
    pub fn formatX(
        self: *const Code,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        @setRuntimeSafety(false);
        _ = fmt;
        _ = options;
        const obj = self.object;
        if (!obj.isDouble()) {
            try writer.print("{}", .{obj});
        } else if (self.object.toIntNoCheck() >= -100 and self.object.toIntNoCheck() < 100) {
            try writer.print("({})", .{self.object.toIntNoCheck()});
        } else try writer.print("0x{x}", .{self.object.toNatNoCheck()});
    }
};
pub const StackStructure = packed struct {
    tag: object.Object.LowTagType = object.Object.lowTagSmallInteger,
    locals: u11 = 0,
    selfOffset: u11 = 0,
    reserve: u11 = 0,
    _filler: std.meta.Int(.unsigned, 64 - 33 - @bitSizeOf(Object.LowTagType) - @bitSizeOf(Object.HighTagType)) = 0,
    hightTag: object.Object.HighTagType = object.Object.highTagSmallInteger,
};
pub const StackAndContext = struct { sp: SP, context: *Context };
pub const endMethod = CompiledMethod.init(Nil(), Code.end);
pub const CompiledMethod = struct {
    header: HeapHeader,
    signature: Signature,
    stackStructure: StackStructure,
    executeFn: ThreadedFn.Fn,
    jitted: ?ThreadedFn.Fn,
    code: [codeSize]Code, // the threaded version of the method
    const Self = @This();
    const codeSize = 2;
    pub const codeOffset = @offsetOf(CompiledMethod, "code");
    comptime {
        std.debug.assert(@offsetOf(Self, "header") == 0);
    }
    const codeOffsetInObjects = codeOffset / 8;
    pub fn init(name: Object, methodFn: ThreadedFn.Fn) Self {
        return Self{
            .header = HeapHeader.calc(.CompiledMethod, codeOffsetInObjects + codeSize, 42 //name.hash24()
                , .static, null, Object, false) catch unreachable,
            .stackStructure = StackStructure{},
            .signature = Signature.from(name, .testClass),
            .executeFn = methodFn,
            .jitted = methodFn,
            .code = .{ Code.primOf(methodFn), undefined },
        };
    }
    pub fn initInfalliblePrimitive(name: Object, class: ClassIndex, methodFn: ThreadedFn.Fn) Self {
        return Self{
            .header = HeapHeader.calc(ClassIndex.CompiledMethod, codeOffsetInObjects + codeSize, name.hash24(), Age.static, null, Object, false) catch unreachable,
            .stackStructure = StackStructure{},
            .signature = Signature.from(name, class),
            .executeFn = methodFn,
            .jitted = methodFn,
            .code = .{ Code.primOf(methodFn), undefined }, //TODO: should be something like primitiveFailed
        };
    }
    pub inline fn reserve(spaceToReserve: u11, sp: SP, process: *Process, context: *Context) StackAndContext {
        if (!process.canAllocStackSpace(sp, spaceToReserve))
            return process.spillStack(sp, context);
        return .{ .sp = sp, .context = context };
    }
    pub fn execute(self: *Self, sp: SP, process: *Process, context: *Context) Result {
        const new = reserve(self.stackStructure.reserve, sp, process, context);
        const pc = PC.init(&self.code[0]);
        trace("\nexecute: {}", .{pc});
        //        trace(" {}", .{ new.sp });
        trace(" {x}\n", .{@as(u64, @bitCast(self.signature))});
        return pc.prim()(pc.next(), new.sp, process, new.context, .{ .method = self });
    }
    // pub fn forDispatch(self: *Self, class: ClassIndex) void {
    //     self.signature.setClass(class);
    //     addMethod(self);
    // }
    inline fn asHeapObjectPtr(self: *const Self) HeapObjectConstPtr {
        return @ptrCast(self);
    }
    pub inline fn codePtr(self: *const Self) *const Code {
        return &self.code[0];
    }
    pub inline fn codePc(self: *const Self) PC {
        return PC.init(@ptrCast(&self.code[0]));
    }
    pub inline fn startPc(self: *const Self) PC {
        return PC.init(@ptrCast(&self.code[1]));
    }
    pub inline fn selectorHash32X(self: *const Self) u32 {
        return @truncate(self.signature.rawU());
    }
    pub fn formatXXX(
        self: *const Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        return self.write(writer);
    }
    pub fn write(
        self: *const Self,
        writer: anytype,
    ) !void {
        const realHO = self.header().realHeapObject();
        const all = @as([]Code, @ptrCast(realHO.asSlice() catch unreachable));
        const refs = realHO.arrayAsSlice(Object) catch unreachable;
        const locals = self.stackStructure.h0;
        const maxStackNeeded = self.stackStructure.h1;
        const selfOffset = @intFromEnum(self.stackStructure.classIndex);
        try writer.print("\nCMethod: {} locals:{} maxStack:{} selfOffset:{} realHO:{} {any} ({any})", .{ self.selector, locals, maxStackNeeded, selfOffset, realHO, all[codeOffsetInObjects .. all.len - refs.len], refs });
    }
    pub fn asFakeObject(self: *const Self) Object {
        return @as(Object, @bitCast(@intFromPtr(self)));
    }
};
fn intOf(comptime str: []const u8) u12 {
    comptime var n: u12 = 0;
    inline for (str) |c| {
        if (c > '9') return n;
        n = n * 10 + (c - '0');
    }
    return n;
}
test "intOf" {
    std.debug.print("Test: intOf\n", .{});
    const expectEqual = std.testing.expectEqual;
    try expectEqual(comptime intOf("012Abc"), 12);
    try expectEqual(comptime intOf("1230Abc"), 1230);
}
fn countNonLabels(comptime tup: anytype) usize {
    comptime var c = 0;
    inline for (tup) |field| {
        switch (@typeInfo(@TypeOf(field))) {
            .pointer => |pointer| {
                switch (@typeInfo(pointer.child)) {
                    .array => switch (field[0]) {
                        ':' => {},
                        else => c += 1,
                    },
                    .@"fn" => {
                        c += 1;
                    },
                    else => {
                        @compileLog(field, ThreadedFn);
                        unreachable;
                    },
                }
            },
            else => {
                c += 1;
            },
        }
    }
    return c;
}
test "countNonLabels" {
    std.debug.print("Test: countNonLabels\n", .{});
    const expectEqual = std.testing.expectEqual;
    try expectEqual(8, countNonLabels(.{
        ":abc",
        &Code.noOp,
        "def",
        0,
        ":def",
        "abc",
        3,
        "1mref",
        null,
        ClassIndex.True,
    }));
}
fn CompileTimeMethod(comptime counts: usize) type {
    const codes = counts + (if (config.is_test) 1 else 0);
    return struct { // structure must exactly match CompiledMethod
        header: HeapHeader,
        signature: Signature,
        stackStructure: StackStructure, // f1 - locals, f2 - maxStackNeeded, f3 - selfOffset
        executeFn: ThreadedFn.Fn,
        jitted: ThreadedFn.Fn,
        code: [codes]Code,
        offsets: [codes]bool align(8),
        const codeOffsetInUnits = CompiledMethod.codeOffsetInObjects;
        const Self = @This();
        // comptime {
        //     std.debug.assert(@offsetOf(Self, "header") == 0);
        //     if (checkEqual(@offsetOf(CompiledMethod, "code"), @offsetOf(Self, "code"))) |s|
        //         @compileError("CompiledMethod prefix not the same as CompileTimeMethod == " ++ s);
        // }
        const cacheSize = 0; //@sizeOf(SendCacheStruct) / @sizeOf(Code);
        pub fn init(name: anytype, comptime locals: u11, comptime maxStack: u16, function: ?ThreadedFn.Fn, class: ClassIndex, tup: anytype) Self {
            const header = comptime HeapHeader.calc(.CompiledMethod, codeOffsetInUnits + codes, 0, Age.static, null, Object, false) catch @compileError("method too big");
            const f = function orelse &Code.noOp;
            var method = Self{
                .header = header,
                .signature = Signature.fromNameClass(name, class),
                .stackStructure = .{ .locals = locals, .reserve = maxStack, .selfOffset = locals + name.numArgs() },
                .executeFn = f,
                .jitted = f,
                .code = undefined,
                .offsets = [_]bool{false} ** codes,
            };
            const code = method.code[0..];
            comptime var n = 0;
            inline for (tup) |field| {
                switch (@TypeOf(field)) {
                    ThreadedFn.Fn => {
                        //@compileLog(field);
                        code[n] = Code.primOf(field);
                        n = n + 1;
                    },
                    threadedFn.Enum => {
                        //@compileLog(field);
                        code[n] = Code.primOf(threadedFn.threadedFn(field));
                        n = n + 1;
                    },
                    Object, bool, @TypeOf(null), comptime_int => {
                        code[n] = Code.objectOf(field);
                        n = n + 1;
                    },
                    PackedObject => {
                        code[n] = Code.packedObjectOf(field);
                        n = n + 1;
                    },
                    ClassIndex => {
                        code[n] = Code.objectOf(@intFromEnum(field));
                        n = n + 1;
                    },
                    StackStructure => {
                        code[n] = Code.structureOf(field);
                        n = n + 1;
                    },
                    else => {
                        if (field[0] != ':') {
                            code[n] = Code{ .offset = if (field[0] >= '0' and field[0] <= '9')
                                comptime intOf(field[0..]) << 1
                            else
                                ((lookupLabel(tup, field) - n - 1) << 1) + 1 };
                            method.offsets[n] = true;
                            n = n + 1;
                        }
                    },
                }
            }
            if (config.is_test) code[n] = Code.primOf(&Code.end);
            return method;
        }
        fn cacheOffset(_: *Self, codeOffs: usize, cacheOffs: usize) u32 {
            return @truncate((codes - codeOffs) + (cacheOffs * cacheSize));
        }
        pub inline fn asCompiledMethodPtr(self: *const Self) *CompiledMethod {
            return @as(*CompiledMethod, @ptrCast(@constCast(self)));
        }
        pub fn resolve(self: *Self, resolution: ?[]const Object) !void {
            for (&self.code, &self.offsets, 0..) |*c, *isOffset, n| {
                if (isOffset.*) {
                    isOffset.* = false;
                    const i = c.offset;
                    trace("\nc[{}] = {}", .{ n, i });
                    if (i & 1 != 0) {
                        c.* = Code.codePtrOf(c, i >> 1);
                    } else {
                        const index: usize = @bitCast(i >> 1);
                        trace(" index: {x}", .{index});
                        if (resolution) |literals| {
                            if (index >= literals.len) return error.Unresolved;
                            c.* = Code.objectOf(literals[index]);
                        }
                    }
                }
            }
        }
        pub fn getCodeSize(_: *Self) usize {
            return codes;
        }
        pub fn findObjectX(self: *Self, search: Object) usize {
            var index = self.references.len - 1;
            while (index >= 0) : (index -= 1) {
                const v = self.references[index];
                if (search.equals(v))
                    return index;
                if (object.NotAnObject.equals(v)) {
                    self.references[index] = search;
                    return index;
                }
            }
            unreachable;
        }
    };
}
test "CompileTimeMethod" {
    std.debug.print("Test: CompileTimeMethod\n", .{});
    const expectEqual = std.testing.expectEqual;
    const c1 = CompileTimeMethod(countNonLabels(.{
        ":abc",
        //        &p.setupSend,
        "def",
        "0True",
        ":def",
        "abc",
        "*",
        3,
        "1mref",
        null,
    }));
    var r1 = c1.init(Sym.value, 2, 3, null, .testClass, .{});
    //TODO    r1.setLiterals(Object.empty, &[_]Object{Nil, True});
    try expectEqual(8, r1.getCodeSize());
}
fn compiledMethodType(comptime codeSize: comptime_int) type {
    return CompileTimeMethod(.{ .codes = codeSize });
}
pub fn compileMethod(name: anytype, comptime locals: comptime_int, comptime maxStack: comptime_int, comptime class: ClassIndex, comptime tup: anytype) CompileTimeMethod(countNonLabels(tup)) {
    return compileMethodWith(name, locals, maxStack, class, null, tup);
}
fn compileMethodWith(name: anytype, comptime locals: comptime_int, comptime maxStack: comptime_int, comptime class: ClassIndex, comptime verifier: ?ThreadedFn.Fn, comptime tup: anytype) CompileTimeMethod(countNonLabels(tup)) {
    @setEvalBranchQuota(100000);
    const MethodType = CompileTimeMethod(countNonLabels(tup));
    return MethodType.init(name, locals, maxStack, verifier, class, tup);
}
fn lookupLabel(tup: anytype, comptime field: []const u8) i56 {
    comptime var lp = 0;
    inline for (tup) |t| {
        if (@TypeOf(t) == ThreadedFn) {
            lp += 1;
            if (t == null) lp += 4;
        } else switch (@typeInfo(@TypeOf(t))) {
            .pointer => |tPointer| {
                switch (@typeInfo(tPointer.child)) {
                    .array => {
                        if (t[0] == ':') {
                            if (comptime std.mem.endsWith(u8, t, field)) {
                                return lp;
                            }
                        } else lp = lp + 1;
                    },
                    else => lp += 1,
                }
            },
            else => {
                lp = lp + 1;
            },
        }
    }
    @compileError("missing label: \"" ++ field ++ "\"");
}
test "LookupLabel" {
    std.debug.print("Test: LookupLabel\n", .{});
    const expectEqual = std.testing.expectEqual;
    const def: []const u8 = "def";
    const c1 = lookupLabel(.{
        ":abc",
        "def",
        "0True",
        ":def",
        "abc",
        3,
        null,
    }, def);
    try expectEqual(2, c1);
}
// const stdout = std.io.getStdOut().writer(); // outside of a functions, stdout causes error on Windows
const print = std.io.getStdOut().writer().print;
const p = @import("threadedFn.zig").Enum;
test "compiling method" {
    std.debug.print("Test: compiling method\n", .{});
    const expectEqual = std.testing.expectEqual;
    //@compileLog(&p.send);
    var m = compileMethod(Sym.yourself, 0, 0, .testClass, .{
        ":abc", p.branch,
        "def",  "0True",
        42,     ":def",
        "abc",  3,
        null,
    });
    //TODO    m.setLiterals(&[_]Object{Sym.value}, &[_]Object{Object.from(42)});
    const t = m.code[0..];
    try expectEqual(8, t.len);
    if (config.debugging) {
        @setRuntimeSafety(false);
        trace("\nm: 0x{x:0>16}", .{@intFromPtr(&m)});
        for (t, 0..) |tv, idx|
            trace("\nt[{}]: 0x{x:0>16}", .{ idx, @as(u64, @bitCast(tv.object)) });
    }
    try m.resolve(&.{True()});
    if (config.debugging) {
        @setRuntimeSafety(false);
        for (t, 0..) |*tv, idx|
            trace("\nt[{}]=0x{x:0>8}: 0x{x:0>16}", .{ idx, @intFromPtr(tv), tv.object.testU() });
    }
    //    try expectEqual(t.prim,controlPrimitives.noop);
    try expectEqual(t[0].prim(), threadedFn.threadedFn(.branch));
    try expectEqual(t[1].codePtr, &m.code[4]);
    try expectEqual(t[2].object, True());
    try expectEqual(t[3].object, Object.from(42, null));
    try expectEqual(t[4].codePtr, &m.code[0]);
    try expectEqual(t[5].object, Object.from(3, null));
    try expectEqual(t[6].object, Nil());
}

fn CompileTimeObject(comptime counts: usize) type {
    const codes = counts;
    return extern struct {
        objects: [codes]Object align(8),
        offsets: [codes]bool align(8),
        const Self = @This();
        pub fn init(comptime tup: anytype, comptime raw: bool) Self {
            var obj = Self{
                .objects = undefined,
                .offsets = [_]bool{false} ** codes,
            };
            const objects = obj.objects[0..];
            comptime var last = -1;
            comptime var n = 0;
            comptime var hash: i24 = 0;
            inline for (tup) |field| {
                const o: Object = switch (@TypeOf(field)) {
                    Object, bool, @TypeOf(null) => Object.from(field, null),
                    comptime_int => blk: {
                        hash = field;
                        break :blk if (raw) Object.rawFromU(@bitCast(@as(i64, field))) else Object.from(field, null);
                    },
                    comptime_float => if (raw) Object.rawFromU(@bitCast(@as(f64, field))) else Object.from(field, null),
                    ClassIndex => blk: {
                        if (last >= 0)
                            objects[last] = @as(HeapHeader, @bitCast(objects[last]))
                                .withLength(n - last - 1)
                                .withHash(@bitCast(hash)).o();
                        const header = HeapHeader.calc(field, 0, 0, Age.static, null, Object, false) catch unreachable;
                        hash = 0;
                        obj.offsets[n] = true;
                        last = n;
                        break :blk header.o();
                    },
                    else => blk: {
                        if (field.len >= 1 and field[0] >= '0' and field[0] <= '9') {
                            obj.offsets[n] = true;
                            break :blk Object.rawFromU(comptime intOf(field[0..]) << 1);
                        } else if (field[0] != ':') {
                            obj.offsets[n] = true;
                            break :blk Object.rawFromU((comptime lookupLabel(tup, field) << 1) + 1);
                        } else continue;
                    },
                };
                objects[n] = o; //  if (raw) @compileLog(o);
                n += 1;
            }
            if (last >= 0)
                objects[last] = @as(HeapHeader, @bitCast(objects[last])).withLength(n - last - 1)
                    .withHash(@bitCast(hash)).o();
            return obj;
        }
        pub fn setLiterals(self: *Self, replacements: []const Object, classes: []const ClassIndex) void {
            var lastHeader: ?*HeapHeader = null;
            for (&self.objects, &self.offsets) |*o, *isOffset| {
                if (isOffset.*) {
                    isOffset.* = false;
                    var header: *HeapHeader = @ptrCast(o);
                    if (header.length < 1024 and
                        header.format == .notIndexable and
                        header.age == .static)
                    {
                        if (@intFromEnum(header.classIndex) >= @intFromEnum(ClassIndex.ReplacementIndices)) {
                            header.classIndex = classes[@intFromEnum(ClassIndex.replace0) - @intFromEnum(header.classIndex)];
                        }
                        header.hash ^= @truncate(@intFromPtr(o) *% phi32);
                        lastHeader = header;
                    } else {
                        const ob: u64 = @bitCast(o.*);
                        o.* = if (ob & 1 != 0) Object.from(&self.objects[ob >> 1], null) else replacements[ob >> 1];
                        if (o.isMemoryAllocated()) {
                            if (lastHeader) |h| h.format = .notIndexableWithPointers;
                        }
                    }
                }
            }
        }
        pub inline fn asHeapObjectPtr(self: *Self) HeapObjectPtr {
            return @ptrCast(&self.objects[0]);
        }
        pub inline fn asObjectArray(self: *Self) [*]Object {
            return @ptrCast(self);
        }
        pub inline fn asObject(self: *Self) Object {
            return Object.from(self.asHeapObjectPtr(), null);
        }
    };
}
pub fn compileObject(comptime tup: anytype) CompileTimeObject(countNonLabels(tup)) {
    @setEvalBranchQuota(100000);
    const objType = CompileTimeObject(countNonLabels(tup));
    return objType.init(tup, false);
}
test "compileObject" {
    var process: Process align(Process.alignment) = Process.new();
    process.init(Nil());
    std.debug.print("Test: compileObject\n", .{});
    const expectEqual = std.testing.expectEqual;
    const expect = std.testing.expect;
    const c = ClassIndex;
    var o = compileObject(.{
        ":def",
        c.Class, // first HeapObject
        "second", // pointer to second object
        42,
        "1mref", // reference to replacement Object #1
        "third", // pointer to third object
        "0mref",
        ":second",
        c.replace0, // second HeapObject - runtime ClassIndex #0
        ":third",
        c.Dispatch, // third HeapObject
        "2True",
        "def",
        "3float",
    });
    const debugging = false;
    if (debugging) {
        for (&o.objects, 0..) |*ob, idx|
            std.debug.print("\no[{}]: 0x{x:0>16}", .{ idx, @as(u64, @bitCast(ob.*)) });
    }
    o.setLiterals(&.{ True(), Nil(), True(), Object.from(42.0, &process) }, &.{@enumFromInt(0xdead)});
    if (debugging) {
        for (&o.objects, 0..) |*ob, idx|
            std.debug.print("\no[{}]=0x{x:0>8}: 0x{x:0>16}", .{ idx, @intFromPtr(ob), @as(u64, @bitCast(ob.*)) });
        std.debug.print("\nTrue=0x{x:0>8}", .{@as(u64, @bitCast(True()))});
    }

    try expect(o.asObject().isHeapObject());
    //try expect(o.objects[9].equals(o.asObject()));
    //    try expectEqual(@as(u48, @truncate(o.asObject().rawU())), @as(u48, @truncate(@intFromPtr(&o.objects[8]))));
    try expect(o.objects[2].equals(Object.from(42, null)));
    try expectEqual(o.objects[10].to(f64), Object.from(42.0, &process).to(f64));
    try expect(o.objects[3].equals(Nil()));
    try expect(o.objects[5].equals(True()));
    const h1: HeapObjectConstPtr = @ptrCast(&o.objects[0]);
    try expectEqual(5, h1.header.length);
    try expect(!h1.header.isIndexable());
    try expect(h1.header.isStatic());
    try expect(h1.header.isUnmoving());
    const h2: HeapObjectConstPtr = @ptrCast(&o.objects[6]);
    try expectEqual(@intFromEnum(h2.header.classIndex), 0xdead);
    try expectEqual(h2.header.length, 0);
    const h3: HeapObjectConstPtr = @ptrCast(&o.objects[7]);
    try expectEqual(h3.header.classIndex, c.Dispatch);
    try expectEqual(h3.header.length, 3);
    try expectEqual(h3.header.age, .static);
    try expectEqual(h3.header.format, .notIndexableWithPointers);
}
pub fn compileRaw(comptime tup: anytype) CompileTimeObject(countNonLabels(tup)) {
    @setEvalBranchQuota(100000);
    const objType = CompileTimeObject(countNonLabels(tup));
    return objType.init(tup, true);
}
test "compileRaw" {
    std.debug.print("Test: compileRaw\n", .{});
    const expectEqual = std.testing.expectEqual;
    const expect = std.testing.expect;
    const c = ClassIndex;
    var o = compileRaw(.{
        c.SmallInteger,
        42,
        42.0,
    });
    const debugging = true;
    if (debugging) {
        @setRuntimeSafety(false);
        for (&o.objects, 0..) |*ob, idx|
            std.debug.print("\no[{}]: 0x{x:0>16}", .{ idx, @as(u64, @bitCast(ob.*)) });
    }
    try expectEqual(@as(i64, @bitCast(o.objects[1])), 42);
    try expectEqual(@as(f64, @bitCast(o.objects[2])), 42.0);
    const h1: HeapObjectConstPtr = @ptrCast(&o.objects[0]);
    try expectEqual(2, h1.header.length);
    try expectEqual(42, h1.header.hash);
    try expect(!h1.header.isIndexable());
    try expect(h1.header.isStatic());
    try expect(h1.header.isUnmoving());
}

pub const Execution = struct {
    const failedObject: packed struct { x: u64 } = .{ .x = undefined };
    pub const failed: Object = @bitCast(failedObject);
    fn Executer(size: comptime_int) type {
        return struct {
            process: Process align(Process.alignment),
            ctxt: Context,
            method: MethodType,
            const MethodType = CompileTimeMethod(size);
            const Self = @This();
            pub fn new(method: MethodType) Self {
                return Self{
                    .process = Process.new(),
                    .ctxt = Context.init(),
                    .method = method,
                };
            }
            pub fn init(self: *Self, stackObjects: ?[]const Object) void {
                self.process.init(Nil());
                if (stackObjects) |source| {
                    self.initStack(source);
                }
            }
            fn initStack(self: *Self, source: []const Object) void {
                const sp = self.process.endOfStack().reserve(source.len);
                self.process.setSp(sp);
                for (source, sp.slice(source.len)) |src, *stck|
                    stck.* = src;
            }
            pub fn resolve(self: *Self, objects: []const Object) !void {
                try self.method.resolve(objects);
                self.process.setContext(&self.ctxt);
            }
            pub fn stack(self: *const Self) []Object {
                return self.process.getContext().stack(self.process.getSp(), &self.process);
            }
            pub fn getHeap(self: *const Self) []HeapObject {
                return self.process.getHeap();
            }
            pub fn getProcess(self: *const Self) *Process {
                return &self.process;
            }
            pub fn getContext(self: *const Self) *Context {
                return self.process.getContext();
            }
            pub fn execute(self: *Self, source: []const Object) !void {
                const method: *CompiledMethod = self.method.asCompiledMethodPtr();
                self.init(source);
                try self.resolve(Object.empty);
                if (false) {
                    const ptr: [*]u64 = @ptrFromInt(@intFromPtr(&self.method));
                    for (ptr[0 .. @sizeOf(MethodType) / 8], 0..) |*v, idx|
                        std.debug.print("[{:>2}:{x:0>16}]: {x:0>16}\n", .{ idx, @intFromPtr(v), v.* });
                }
                _ = method.execute(self.process.getSp(), &self.process, self.process.getContext());
            }
            pub fn matchStack(self: *const Self, expected: []const Object) !void {
                const result = self.stack();
                try std.testing.expectEqualSlices(Object, expected, result);
            }
        };
    }
    pub fn initTest(title: []const u8, tup: anytype) Executer(countNonLabels(tup)) {
        std.debug.print("ExecutionTest: {s}\n", .{title});
        return init(tup);
    }
    fn init(tup: anytype) Executer(countNonLabels(tup)) {
        const ExeType = Executer(countNonLabels(tup));
        return ExeType.new(compileMethod(Sym.yourself, 0, 0, .testClass, tup));
    }
    pub fn runTest(title: []const u8, tup: anytype, source: []const Object, expected: []const Object) !void {
        return runTestWithObjects(title, tup, Object.empty, source, expected);
    }
    pub fn runTestWithObjects(title: []const u8, tup: anytype, objects: []const Object, source: []const Object, expected: []const Object) !void {
        try runWithValidator(title, tup, &validate, objects, source, expected);
    }
    pub const ValidateErrors = error{
        TestAborted,
        TestExpectedEqual,
    };
    pub fn runTestWithValidator(title: []const u8, tup: anytype, validator: *const fn (anytype, []const Object) ValidateErrors!void, source: []const Object, expected: []const Object) !void {
        return runWithValidator(title, tup, validator, Object.empty, source, expected);
    }
    fn runWithValidator(title: []const u8, tup: anytype, validator: *const fn (anytype, []const Object) ValidateErrors!void, objects: []const Object, source: []const Object, expected: []const Object) !void {
        std.debug.print("ExecutionTest: {s}\n", .{title});
        var exe align(Process.alignment) = init(tup);
        exe.process.init(Nil());
        const t = exe.method.code[0..];
        if (config.debugging) {
            @setRuntimeSafety(false);
            for (t, 0..) |tv, idx|
                trace("t[{}]: 0x{x:0>16}\n", .{ idx, @as(u64, @bitCast(tv.object)) });
        }
        try exe.resolve(objects);
        if (config.debugging) {
            @setRuntimeSafety(false);
            for (t, 0..) |*tv, idx|
                trace("t[{}]=0x{x:0>8}: 0x{x:0>16}\n", .{ idx, @intFromPtr(tv), @as(u64, @bitCast(tv.object)) });
        }
        try exe.execute(source);
        try std.testing.expect(exe.getContext() == &exe.ctxt);
        try validator(&exe, expected);
    }
    fn validate(exe: anytype, expected: []const Object) ValidateErrors!void {
        const result = exe.stack();
        if (result.len > 0 and result[0] == failed) return error.TestAborted;
        trace(
            \\run:
            \\  expected: {any}
            \\  result: {any}
            \\
        , .{ expected, result });
        try std.testing.expectEqualSlices(Object, expected, result);
    }

    // fn mainSendTo(selector: Object, target: Object) !void {
    //     std.debug.print("Sending: {} to {}\n", .{ selector, target });
    //     var exec = Self.new();
    //     const args = [_]Object{target};
    //     exec.initStack(&args);
    //     exec.ctxt.setReturn(Code.endThread);
    //     return error.NotImplemented;
    // }
};
