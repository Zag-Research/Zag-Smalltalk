const std = @import("std");
const config = @import("config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const zag = @import("zag.zig");
const checkEqual = zag.utilities.checkEqual;
const Process = zag.Process;
const ProcessPtr = *Process;
const SP = Process.SP;
const object = zag.object;
const Object = object.Object;
const o0 = object.testObjects[0];
const o1 = object.testObjects[1];
const PackedObject = object.PackedObject;
const ClassIndex = object.ClassIndex;
const Nil = object.Nil;
const True = object.True;
const False = object.False;
const Context = zag.Context;
const ContextData = Context.ContextData;
const Variable = Context.Variable;
const Extra = Context.Extra;
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
const threadedFn = zag.threadedFn;
const tf = threadedFn.Enum;

pub const Result = SP;
pub const Signature = packed struct {
    int: u64,
    const Internal = packed struct {
        selector: u40,
        class: ClassIndex,
        prim: u8 = 0,
    };
    const Create = packed struct {
        tag: u8 = sigTag,
        selector: u32,
        class: ClassIndex = .none,
        prim: u8 = 0,
        const sigTag: u8 = @intFromEnum(ClassIndex.Compact.Signature) << 3 | Object.immediatesTag;
        fn isTagged(self: Create) bool {
            return self.tag == sigTag;
        }
    };
    pub const empty: Signature = .{ .int = Create.sigTag };
    pub fn isEmpty(self: Signature) bool {
        return self.int == empty.int;
    }
    pub fn hash(self: Signature) u32 {
        return @truncate(self.int);
    }
    pub fn from(selector: Object, class: ClassIndex) Signature {
        return .{ .int = @bitCast(Create{ .selector = selector.hash32(), .class = class }) };
    }
    pub fn fromHashPrimitive(selector: u32, primitiveNumber: u8) Signature {
        return .{ .int = @bitCast(Create{ .selector = selector, .prim = primitiveNumber }) };
    }
    pub fn fromPrimitive(primitiveNumber: u8) Signature {
        return .{ .int = @bitCast(Create{ .selector = 0, .prim = primitiveNumber }) };
    }
    pub fn fromNameClass(name: anytype, class: ClassIndex) Signature {
        return .{ .int = @bitCast(Create{ .selector = @as(u32, name.numArgs()) << 24 | @as(u32, (name.symbolHash().?)), .class = class }) };
    }
    pub fn equals(self: Signature, other: Signature) bool {
        return self.int == other.int;
    }
    pub fn numArgs(self: Signature) u8 {
        return @truncate(self.int >> 32);
    }
    pub fn signature(self: Object) ?Signature {
        const sig: Create = @bitCast(self);
        if (sig.isTagged()) {
            trace("sig = {}", .{sig});
            return @bitCast(self);
        }
        return null;
    }
    pub fn asObject(self: Signature) Object {
        return @bitCast(self);
    }
    pub inline fn asSymbol(self: Signature) Object {
        return symbol.fromHash((self.int & 0xffffff00) >> 8);
    }
    pub inline fn primitive(self: Signature) u8 {
        return @as(Internal, @bitCast(self)).prim;
    }
    pub inline fn getClassIndex(self: Signature) u16 {
        return @intFromEnum(@as(Internal, @bitCast(self)).class);
    }
    pub inline fn getClass(self: Signature) ClassIndex {
        return @as(Internal, @bitCast(self.int)).class;
    }
    pub fn withClass(self: Signature, class: ClassIndex) Signature {
        return .{ .int = (self.int & 0xffffffffff) + (@as(u64, @intFromEnum(class)) << 40) };
    }
    pub fn format(
        self: Signature,
        writer: anytype,
    ) !void {
        if (self.isEmpty()) {
            try writer.print("Signature{{empty}}", .{});
        } else {
            switch (self.getClass()) {
                .none => switch (self.primitive()) {
                    0 => try writer.print("?", .{}),
                    else => |prim| try writer.print("{}", .{prim}),
                },
                else => |class| try writer.print("{}", .{class}),
            }
            try writer.print(" #{s}", .{ symbol.asStringFromHash(@truncate((self.int & 0xffffff00) >> 8)).arrayAsSlice(u8) catch "???" });
        }
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
    pub var exitCode: [1]Code = undefined;
    pub fn exit() PC {
        exitCode[0] = Code.endCode;
        return init(@ptrCast(&exitCode[0]));
    }
    pub inline //
    fn packedObject(self: PC) PackedObject {
        if (logging) {
            @setRuntimeSafety(false);
            std.log.err("PC_packed:       {x:0>12}: {}\n", .{ @intFromPtr(self.code), self.code.packedObject });
        }
        return self.code.packedObject;
    }
    pub fn offset(self: PC, cm: *const CompiledMethod) usize {
        return (@intFromPtr(self.code) - @intFromPtr(&cm.code[0])) / @sizeOf(Code);
    }
    pub inline //
    fn method(self: PC) *const CompiledMethod {
        if (logging) {
            @setRuntimeSafety(false);
            std.log.err("PC_method:       {x:0>12}: {f}\n", .{ @intFromPtr(self.code), self.code.method });
        }
        return self.code.method;
    }
    pub inline //
    fn codeAddress(self: PC) *const Code {
        if (logging) {
            @setRuntimeSafety(false);
            std.log.err("PC_codeAddress:  {x:0>12}: {f}\n", .{ @intFromPtr(self.code), self.code.codePtr });
        }
        return self.code.codePtr;
    }
    pub inline //
    fn targetPC(self: PC) PC {
        return .{ .code = self.codeAddress() };
    }
    pub inline //
    fn asThreadedFn(self: PC) *const fn (PC, SP, *Process, *Context, Extra) Result {
        if (logging) {
            std.log.err("PC_asThreadedFn: {x:0>12}: {f}\n", .{ @intFromPtr(self.code), self });
        }
        return self.code.prim();
    }
    pub inline //
    fn prim(self: PC) *const fn (PC, SP, *Process, *Context, Extra) Result {
        return primOf(self.code);
    }
    inline fn primOf(code: *const Code) *const fn (PC, SP, *Process, *Context, Extra) Result {
        if (logging) {
            @setRuntimeSafety(false);
            if (@import("threadedFn.zig").find(code.prim())) |name| {
                std.log.err("PC_prim:         {x:0>12}: {}\n", .{ @intFromPtr(code), name });
            } else {
                std.log.err("PC_prim:         {x:0>12}: {x}\n", .{ @intFromPtr(code), @intFromPtr(code.prim()) });
            }
        }
        return code.prim();
    }
    pub //inline
    fn object(self: PC) Object {
        if (logging) {
            @setRuntimeSafety(false);
            std.log.err("PC_object:       {x:0>12}: {f}\n", .{ @intFromPtr(self.code), self.code.object });
        }
        return self.code.object;
    }
    pub //inline
    fn variable(self: PC) Variable {
        if (logging) {
            @setRuntimeSafety(false);
            std.log.err("PC_variable:     {x:0>12}: {f}\n", .{ @intFromPtr(self.code), self.code.variable });
        }
        return self.code.variable;
    }
    pub inline //
    fn signature(self: PC) Signature {
        if (logging) {
            @setRuntimeSafety(false);
            std.log.err("PC_signature:    {x:0>12}: {f}\n", .{ @intFromPtr(self.code), self.code.signature });
        }
        return self.code.signature;
    }
    pub inline fn asCode(self: PC) Code {
        return self.code.*;
    }
    pub inline fn asCodePtr(self: PC) *const Code {
        return self.code;
    }
    pub inline fn patchPtr(self: PC) *Code {
        return @constCast(self.code);
    }
    pub inline fn structure(self: PC) StackStructure {
        return self.code.structure;
    }
    pub inline //
    fn uint(self: PC) u64 {
        if (logging) {
            @setRuntimeSafety(false);
            std.log.err("PC_uint:         {x:0>12}: {f}\n", .{ @intFromPtr(self.code), self.code.object });
        }
        return self.code.object.to(u64);
    }
    pub inline //
    fn int(self: PC) i64 {
        if (logging) {
            @setRuntimeSafety(false);
            std.log.err("PC_int:          {x:0>12}: {f}\n", .{ @intFromPtr(self.code), self.code.object });
        }
        return self.code.object.to(i64);
    }
    pub inline fn next(self: PC) PC {
        return asPC(self.array() + 1);
    }
    pub inline fn prev(self: PC) PC {
        return asPC(self.array() - 1);
    }
    pub inline fn prim2(self: PC) *const fn (PC, SP, *Process, *Context, Extra) Result {
        return primOf(&self.array()[1]);
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
        writer: anytype,
    ) !void {
        try writer.print("{{PC {x}}}", .{@intFromPtr(self.code)});
    }
};
pub const Code = union(enum) {
    object: Object,
    variable: Variable,
    threadedFn: *const fn (PC, SP, *Process, *Context, Extra) Result,
    method: *const CompiledMethod,
    codePtr: *Code,
    offset: i64,
    packedObject: PackedObject,
    structure: StackStructure,
    signature: Signature,
    pub inline fn primOf(pp: *const fn (PC, SP, *Process, *Context, Extra) Result) Code {
        //@compileLog(pp);
        const result = Code{ .threadedFn = pp };
        return result;
    }
    pub inline fn patchPrim(self: *Code, pp: *const fn (PC, SP, *Process, *Context, Extra) Result) void {
        self.* = Code{ .threadedFn = pp };
    }
    pub inline fn patchMethod(self: *Code, method: *const CompiledMethod) void {
        self.* = Code{ .method = method };
    }
    pub inline //
    fn asVariable(self: Code) Variable {
        //        @setRuntimeSafety(false);
        return self.variable;
    }
    pub inline fn variableOf(v: Variable) Code {
        return Code{ .variable = v };
    }
    pub inline //
    fn asObject(self: Code) Object {
        //        @setRuntimeSafety(false);
        return self.object;
    }
    pub inline fn objectOf(o: Object) Code {
        return Code{ .object = o };
    }
    pub inline fn packedObjectOf(po: PackedObject) Code {
        return Code{ .packedObject = po };
    }
    pub inline fn methodOf(method: *const CompiledMethod) Code {
        return Code{ .method = method };
    }
    pub inline fn structureOf(structure: StackStructure) Code {
        return Code{ .structure = structure };
    }
    pub inline fn signatureOf(signature: Signature) Code {
        return Code{ .signature = signature };
    }
    pub inline fn codePtrOf(code: *Code, offs: i64) Code {
        const addr = @as([*]Code, @ptrCast(code)) + 1;
        return Code{ .codePtr = @ptrCast(addr + @as(u64, @bitCast(offs))) };
    }
    pub inline //
    fn prim(self: Code) *const fn (PC, SP, *Process, *Context, Extra) Result {
        return self.threadedFn;
    }
    fn endM(_: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result { // not embedded
        trace("endM: {*} {?*}", .{ context, context.prevCtxt });
        process.setContext(context);
        process.setSp(sp);
        return sp;
    }
    pub const endMain = primOf(endM);
    pub fn end(_: PC, sp: SP, process: *Process, context: *Context, _: Extra) Result { // not embedded
        process.setContext(context);
        process.setSp(sp);
        return sp;
    }
    pub const endCode = primOf(end);
    pub fn panic(_: PC, _: SP, _: *Process, _: *Context, _: Extra) Result { // not embedded
        @panic("not implemented");
    }
    fn noOp(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, extra });
    }
    pub fn format(
        self: *const Code,
        writer: anytype,
    ) !void {
        if (false) {
            try writer.print("({x})", .{@as(*const u64, @ptrCast(self)).*});
            //return;
        }
        switch (self.*) {
            .object => |obj| try writer.print("object({f})", .{obj}),
            .variable => |variable| try writer.print("variable   ({f})", .{variable}),
            .threadedFn => |tFn| {
                if (@import("threadedFn.zig").find(tFn)) |name| {
                    try writer.print("{}", .{name});
                } else if (@import("primitives.zig").findPrimitiveAtPtr(tFn)) |primitive| {
                    try writer.print("primitive({s}>>#{s} : {}) {*}", .{primitive.module, primitive.name, primitive.number, tFn });
                } else {
                    try writer.print("{*}", .{tFn});
                }
            },
            .method => |method| try writer.print("method({f})", .{method.signature}),
            .codePtr => |codePtr| try writer.print("codePtr({x:0>12})", .{@intFromPtr(codePtr)}),
            .offset => |offset| try writer.print("offset({})", .{offset}),
            .packedObject => |packedObject| try writer.print("packedObject({})", .{packedObject}),
            .structure => |structure| try writer.print("structure({})", .{structure}),
            .signature => |signature| try writer.print("signature({f})", .{signature}),
        }
    }
};
pub const StackStructure = packed struct {
    tag: Object.LowTagType = Object.lowTagSmallInteger,
    _fillerLow: std.meta.Int(.unsigned, 16 - @bitSizeOf(Object.LowTagType)) = 0,
    locals: u16 = 0,
    selfOffset: u16 = 0,
    _fillerHigh: std.meta.Int(.unsigned, 64 - 48 - @bitSizeOf(Object.HighTagType)) = 0,
    highTag: Object.HighTagType = Object.highTagSmallInteger,
};
pub const endMethod = CompiledMethod.init(Sym.value, Code.end);
pub const CompiledMethod = struct {
    header: HeapHeader,
    signature: Signature,
    stackStructure: StackStructure,
    executeFn: *const fn (PC, SP, *Process, *Context, Extra) Result,
    jitted: ?*const fn (PC, SP, *Process, *Context, Extra) Result,
    code: [codeSize]Code, // the threaded version of the method
    const Self = @This();
    const codeSize = 2;
    pub const codeOffset = @offsetOf(CompiledMethod, "code");
    comptime {
        std.debug.assert(@offsetOf(Self, "header") == 0);
    }
    const codeOffsetInObjects = codeOffset / 8;
    pub fn format(
        self: CompiledMethod,
        writer: anytype,
    ) !void {
        try writer.print("CompiledMethod{{.stackStructure={}, .signature={f}, .executeFn={x}}}", .{ self.stackStructure, self.signature, @intFromPtr(self.executeFn) });
    }
    pub fn dumpHeader(self: *const Self) u32 {
        std.debug.print("Header:           {f}\n", .{self.header});
        std.debug.print("Stack Structure:  {}\n", .{self.stackStructure});
        std.debug.print("Signature:        {f}\n", .{self.signature});
        std.debug.print("Execute Function: {}\n", .{self.executeFn});
        std.debug.print("Jitted Function:  {?}\n", .{self.jitted});
        const methodSize = self.header.length - codeOffsetInObjects;
        std.debug.print("methodSize:       {}\n", .{methodSize});
        return methodSize;
    }
    pub fn dump(self: *const Self) void {
        const methodSize = self.dumpHeader();
        const code: [*]const Code = @ptrCast(&self.code);
        for (code[0..methodSize]) |*instruction| {
            std.debug.print("[{x:0>12}]: {f}\n", .{ @intFromPtr(instruction), instruction.* });
        }
    }
    pub fn init(name: anytype, methodFn: *const fn (PC, SP, *Process, *Context, Extra) Result) Self {
        return Self{
            .header = HeapHeader.calc(.CompiledMethod, codeOffsetInObjects + codeSize, 42 //name.hash24()
                , .static, null, Object, false) catch unreachable,
            .stackStructure = StackStructure{},
            .signature = Signature.fromNameClass(name, .testClass),
            .executeFn = methodFn,
            .jitted = methodFn,
            .code = .{ Code.primOf(methodFn), undefined },
        };
    }
    pub fn execute(self: *const Self, sp: SP, process: *Process, context: *Context) Result {
        const newExtra = Extra.forMethod(self, sp);
        const pc = PC.init(&self.code[1]);
        return self.executeFn(pc, sp, process, context, newExtra);
    }
    inline fn asHeapObjectPtr(self: *const Self) HeapObjectConstPtr {
        return @ptrCast(self);
    }
    pub inline fn codePtr(self: *const Self) *const Code {
        return &self.code[0];
    }
    pub inline fn codePc(self: *const Self) PC {
        return PC.init(@ptrCast(&self.code[0]));
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
                        if (@TypeOf(field) == *const CompiledMethod) {
                            c += 1;
                        } else {
                            @compileLog(field);
                            @panic("unreachable");
                        }
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
        stackStructure: StackStructure, // f1 - locals, f3 - selfOffset
        executeFn: *const fn (PC, SP, *Process, *Context, Extra) Result,
        jitted: ?*const fn (PC, SP, *Process, *Context, Extra) Result,
        code: [codes]Code,
        offsets: [codes]OffsetType align(8),
        const OffsetType = enum(u2) {
            none,
            offset,
            object,
        };
        const codeOffsetInUnits = CompiledMethod.codeOffsetInObjects;
        const Self = @This();
        // comptime {
        //     std.debug.assert(@offsetOf(Self, "header") == 0);
        //     if (checkEqual(@offsetOf(CompiledMethod, "code"), @offsetOf(Self, "code"))) |s|
        //         @compileError("CompiledMethod prefix not the same as CompileTimeMethod == " ++ s);
        // }
        pub fn dump(self: *const Self) void {
            const methodSize = @as(*const CompiledMethod, @ptrCast(self)).dumpHeader();
            const code: [*]const Code = @ptrCast(&self.code);
            for (0..methodSize) |i| {
                std.debug.print("[{x:0>12}]: {f}{s}\n", .{ @intFromPtr(&code[i]), code[i], switch (self.offsets[i]) {
                    .none => "",
                    .offset => " - offset",
                    .object => " - object",
                } });
            }
        }
        pub fn init(name: anytype, comptime locals: u11, function: ?*const fn (PC, SP, *Process, *Context, Extra) Result, class: ClassIndex, tup: anytype) Self {
            const header = comptime HeapHeader.calc(.CompiledMethod, codeOffsetInUnits + codes, 0, Age.static, null, Object, false) catch @compileError("method too big");
            var method = Self{
                .header = header,
                .signature = Signature.fromNameClass(name, class),
                .stackStructure = StackStructure{ .locals = locals, .selfOffset = locals + name.numArgs() + 1 },
                .executeFn = &Code.panic,
                .jitted = function,
                .code = undefined,
                .offsets = [_]OffsetType{.none} ** codes,
            };
            const code = method.code[0..];
            comptime var n = 0;
            comptime var needFunction = true;
            inline for (tup) |field| {
                switch (@TypeOf(field)) {
                    *const fn (PC, SP, *Process, *Context, Extra) Result => {
                        //@compileLog(field);
                        if (needFunction) {
                            method.executeFn = field;
                            needFunction = false;
                        }
                        code[n] = Code.primOf(field);
                        n = n + 1;
                    },
                    *const CompiledMethod => {
                        code[n] = Code.methodOf(field);
                        n = n + 1;
                    },
                    threadedFn.Enum => {
                        //@compileLog(field);
                        const func = threadedFn.threadedFn(field);
                        if (needFunction) {
                            method.executeFn = func;
                            needFunction = false;
                        }
                        code[n] = Code.primOf(func);
                        n = n + 1;
                    },
                    Object => {
                        code[n] = Code.objectOf(field);
                        n = n + 1;
                    },
                    bool => {
                        code[n] = Code.objectOf(if (field) True() else False());
                        n = n + 1;
                    },
                    @TypeOf(null) => {
                        code[n] = Code.objectOf(Nil());
                        n = n + 1;
                    },
                    PackedObject => {
                        code[n] = Code.packedObjectOf(field);
                        n = n + 1;
                    },
                    // ClassIndex => {
                    //     code[n] = Code.objectOf(@intFromEnum(field));
                    //     n = n + 1;
                    // },
                    StackStructure => {
                        code[n] = Code.structureOf(field);
                        n = n + 1;
                    },
                    Signature => {
                        code[n] = Code.signatureOf(field);
                        n = n + 1;
                    },
                    Variable => {
                        code[n] = Code.variableOf(field);
                        n = n + 1;
                    },
                    else => {
                        if (field[0] != ':') {
                            if (field[0] >= '0' and field[0] <= '9') {
                                code[n] = Code{ .offset = comptime intOf(field[0..]) };
                                method.offsets[n] = .object;
                            } else {
                                code[n] = Code{ .offset = lookupLabel(tup, field) - n - 1 };
                                method.offsets[n] = .offset;
                            }
                            n = n + 1;
                        }
                    },
                }
            }
            if (config.is_test) code[n] = Code.primOf(&Code.end);
            return method;
        }
        pub inline fn asCompiledMethodPtr(self: *const Self) *CompiledMethod {
            return @as(*CompiledMethod, @ptrCast(@constCast(self)));
        }
        pub fn initExecute(self: *Self) void {
            self.executeFn = self.code[0].prim();
        }
        pub fn resolve(self: *Self, resolution: ?[]const Object) !void {
            for (&self.code, &self.offsets) |*c, *offset| {
                switch (offset.*) {
                    .offset => {
                        const offs = c.offset;
                        c.* = Code.codePtrOf(c, offs);
                        offset.* = .none;
                    },
                    .object => {
                        const index: usize = @bitCast(c.offset);
                        if (resolution) |literals| {
                            if (index >= literals.len) return error.Unresolved;
                            c.* = Code.objectOf(literals[index]);
                            offset.* = .none;
                        }
                    },
                    .none => {},
                }
            }
        }
        pub fn getCodeSize(_: *Self) usize {
            return codes;
        }
        fn execute(self: *Self, sp: SP, process: *Process, context: *Context) Result {
            return @as(*CompiledMethod, @ptrCast(self)).execute(sp, process, context);
        }
    };
}
test "CompileTimeMethod" {
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
    var r1 = c1.init(Sym.value, 2, null, .testClass, .{});
    //TODO    r1.setLiterals(Object.empty, &[_]Object{Nil, True});
    try expectEqual(8, r1.getCodeSize());
}
pub fn compileMethod(name: anytype, comptime locals: comptime_int, comptime class: ClassIndex, comptime tup: anytype) CompileTimeMethod(countNonLabels(tup)) {
    @setEvalBranchQuota(100000);
    const MethodType = CompileTimeMethod(countNonLabels(tup));
    return MethodType.init(name, locals, null, class, tup);
}
fn lookupLabel(tup: anytype, comptime field: []const u8) i56 {
    comptime var lp = 0;
    inline for (tup) |t| {
        if (@TypeOf(t) == *const fn (PC, SP, *Process, *Context, Extra) Result) {
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
    const expectEqual = std.testing.expectEqual;
    //@compileLog(&p.send);
    const exe = Execution.init( .{
        ":abc", p.branch,
        "def",  "0True",
        o0,     ":def",
        "abc",  o1,
        null,
    });
    var m = exe.method;
    //TODO    m.setLiterals(&[_]Object{Sym.value}, &[_]Object{Object.from(42)});
    const t = m.code[0..];
    try expectEqual(8, t.len);
    try m.resolve(&.{True()});
    //    try expectEqual(t.prim,controlPrimitives.noop);
    try expectEqual(t[0].prim(), threadedFn.threadedFn(.branch));
    try expectEqual(t[1].codePtr, &m.code[4]);
    try expectEqual(t[2].object, True());
    try expectEqual(t[3].object, o0);
    try expectEqual(t[4].codePtr, &m.code[0]);
    try expectEqual(t[5].object, o1);
    try expectEqual(t[6].object, Nil());
}

fn CompileTimeObject(comptime counts: usize) type {
    const codes = counts;
    return extern struct {
        objects: [codes]Object align(8),
        offsets: [codes]bool align(8),
        const Self = @This();
        pub fn init(comptime tup: anytype, raw: bool) Self {
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
                    Object => field,
                    bool => if (field) True() else False(),
                    @TypeOf(null) => Nil(),
                    comptime_int => blk: {
                        hash = field;
                        break :blk if (raw) Object.rawFromU(@bitCast(@as(i64, field))) else @panic("unreachable");
                    },
                    comptime_float => if (raw) Object.rawFromU(@bitCast(@as(f64, field))) else unreachable,
                    ClassIndex => blk: {
                        if (last >= 0)
                            objects[last] = @as(HeapHeader, @bitCast(objects[last]))
                                .withLength(n - last - 1)
                                .withHash(@bitCast(hash)).o();
                        const header = HeapHeader.calc(field, 0, 0, Age.static, null, Object, false) catch @panic("unreachable");
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
                        header.objectFormat == .notIndexable and
                        header.age == .static)
                    {
                        if (@intFromEnum(header.classIndex) >= @intFromEnum(ClassIndex.ReplacementIndices)) {
                            header.classIndex = classes[@intFromEnum(ClassIndex.replace0) - @intFromEnum(header.classIndex)];
                        }
                        header.hash ^= zag.utilities.ProspectorHash.hash24(@truncate(@intFromPtr(o)));
                        lastHeader = header;
                    } else {
                        const ob: u64 = @bitCast(o.*);
                        o.* = if (ob & 1 != 0) Object.fromAddress(&self.objects[ob >> 1]) else replacements[ob >> 1];
                        if (o.isMemoryAllocated()) {
                            if (lastHeader) |h| h.objectFormat = .notIndexableWithPointers;
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
            return Object.fromAddress(self.asHeapObjectPtr());
        }
    };
}
pub fn compileObject(comptime tup: anytype) CompileTimeObject(countNonLabels(tup)) {
    @setEvalBranchQuota(100000);
    const objType = CompileTimeObject(countNonLabels(tup));
    return objType.init(tup, false);
}
test "compileObject" {
    var process: Process align(Process.alignment) = undefined;
    process.init();
    const expectEqual = std.testing.expectEqual;
    const expect = std.testing.expect;
    const c = ClassIndex;
    var o = compileObject(.{
        ":def",
        c.Class, // first HeapObject
        "second", // pointer to second object
        o0,
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
            trace("o[{}]: 0x{x:0>16}", .{ idx, @as(u64, @bitCast(ob.*)) });
    }
    o.setLiterals(&.{ True(), Nil(), True(), Object.from(42.0, process.getSp(), process.getContext()) }, &.{@enumFromInt(0xdead)});
    if (debugging) {
        for (&o.objects, 0..) |*ob, idx|
            trace("o[{}]=0x{x:0>8}: 0x{x:0>16}", .{ idx, @intFromPtr(ob), @as(u64, @bitCast(ob.*)) });
        trace("True=0x{x:0>8}", .{@as(u64, @bitCast(True()))});
    }

    try expect(o.asObject().isHeapObject());
    //try expect(o.objects[9].equals(o.asObject()));
    //    try expectEqual(@as(u48, @truncate(o.asObject().rawU())), @as(u48, @truncate(@intFromPtr(&o.objects[8]))));
    try expect(o.objects[2].equals(o0));
    try expectEqual(o.objects[10].to(f64), Object.from(42.0, process.getSp(), process.getContext()).to(f64));
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
    try expectEqual(h3.header.objectFormat, .notIndexableWithPointers);
}
pub fn compileRaw(comptime tup: anytype) CompileTimeObject(countNonLabels(tup)) {
    @setEvalBranchQuota(100000);
    const objType = CompileTimeObject(countNonLabels(tup));
    return objType.init(tup, true);
}
test "compileRaw" {
    const expectEqual = std.testing.expectEqual;
    const expect = std.testing.expect;
    const c = ClassIndex;
    var o = compileRaw(.{
        c.SmallInteger,
        42,
        42.0,
    });
    const debugging = false;
    if (debugging) {
        @setRuntimeSafety(false);
        for (&o.objects, 0..) |*ob, idx|
            trace("o[{}]: 0x{x:0>16}", .{ idx, @as(u64, @bitCast(ob.*)) });
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
    const failedObject: packed struct { x: u64 } = .{ .x = 2 };
    pub const failed: Object = @bitCast(failedObject);
    fn Executer(MethodType: type) type {
        return struct {
            process: Process align(Process.alignment),
            method: MethodType,
            process_initted: bool = false,
            const Self = @This();
            pub fn new(method: MethodType) Self {
                return Self{
                    .process = Process.new(),
                    .method = method,
                };
            }
            pub fn initProcess(self: *Self) *Process {
                if (!self.process_initted) {
                    self.process.init();
                    self.process_initted = true;
                }
                return &self.process;
            }
            pub fn init(self: *Self, stackObjects: ?[]const Object) void {
                _ = self.initProcess();
                if (stackObjects) |source| {
                    self.initStack(source);
                }
            }
            fn initStack(self: *Self, source: []const Object) void {
                const sp = self.initProcess().endOfStack().safeReserve(source.len);
                self.process.setSp(sp);
                for (source, sp.slice(source.len)) |src, *stck|
                    stck.* = src;
            }
            pub fn resolve(self: *Self, objects: []const Object) !void {
                try self.method.resolve(objects);
            }
            pub fn stack(self: *Self) []Object {
                return self.getProcess().getContext().stack(self.process.getSp());
            }
            pub fn fullStack(self: *Self) []Object {
                return self.getSp().getStack();
            }
            pub fn getHeap(self: *Self) []HeapObject {
                return self.getProcess().getHeap();
            }
            pub fn getProcess(self: *Self) *Process {
                return self.initProcess();
            }
            pub fn getContext(self: *Self) *Context {
                return self.getProcess().getContext();
            }
            pub fn getSp(self: *Self) SP {
                return self.getProcess().getSp();
            }
            pub inline fn object(self: *Self, value: anytype) Object {
                return Object.from(value, self.getSp(), self.getContext());
            }
            pub fn execute(self: *Self, stackObjects: ?[]const Object) void {
                self.init(stackObjects);
                _ = self.method.execute(self.getSp(), self.getProcess(), self.getContext());
                self.getSp().traceStack("return from execution");
            }
            pub fn matchStack(self: *Self, expected: []const Object) !void {
                const result = self.stack();
                try std.testing.expectEqualSlices(Object, expected, result);
            }
            pub fn runTest(self: *Self, source: []const Object, expected: []const Object) !void {
                return self.runTestWithObjects(Object.empty, source, expected);
            }
            pub fn runTestWithObjects(self: *Self, objects: []const Object, source: []const Object, expected: []const Object) !void {
                try self.resolve(objects);
                try self.runWithValidator(&validate, source, expected);
            }
            pub const ValidateErrors = error{
                TestAborted,
                TestExpectedEqual,
            };
            pub fn runTestWithValidator(self: *Self, validator: *const fn (*Self, []const Object) ValidateErrors!void, source: []const Object, expected: []const Object) !void {
                return self.runWithValidator(validator, source, expected);
            }
            fn runWithValidator(self: *Self, validator: *const fn (*Self, []const Object) ValidateErrors!void, source: []const Object, expected: []const Object) !void {
                self.process.init();
                self.execute(source);
                try validator(self, expected);
            }
            fn validate(self: *Self, expected: []const Object) ValidateErrors!void {
                const result = self.stack();
                if (result.len > 0 and result[0] == failed) return error.TestAborted;
                try expectEqualSlices(expected, result);
            }
        };
    }
    pub fn initTest(title: []const u8, tup: anytype) Executer(CompileTimeMethod(countNonLabels(tup))) {
        trace("ExecutionTest: {s}", .{title});
        return init(tup);
    }
    fn init(comptime tup: anytype) Executer(CompileTimeMethod(countNonLabels(tup))) {
        const ExeType = Executer(CompileTimeMethod(countNonLabels(tup)));
        const method = compileMethod(Sym.yourself, 0, .testClass, tup);
        return ExeType.new(method);
    }

    pub const MainExecutor = struct {
        exe: Executer(*const CompiledMethod) align(Process.alignment),
        pub fn new() align(Process.alignment) MainExecutor {
            return .{ .exe = Executer(*const CompiledMethod).new(undefined) };
        }
        pub inline fn object(self: *MainExecutor, value: anytype) Object {
            return self.exe.object(value);
        }
        pub fn sendTo(self: *MainExecutor, selector: Object, receiver: Object) !Object {
            var exe = &self.exe;
            trace("Sending: {f} to {f}", .{ selector, receiver });
            exe.init(Object.empty);
            exe.getContext().setReturn(PC.exit());
            trace("SendTo: context {*} {*} {f}", .{ exe.getContext(), exe.getContext().npc, exe.getContext().tpc });
            const class = receiver.get_class();
            const signature = Signature.from(selector, class);
            exe.method = class.lookupMethodForClass(signature);
            exe.execute(&[_]Object{receiver});
            return exe.fullStack()[0];
        }
    };
};
fn expectEqualSlices(expected: []const Object, result: []const Object) !void {
    const min = @min(expected.len, result.len);
    const index = blk: {
        for (0..min) |i| {
            if (!expected[i].equals(result[i])) break :blk i;
        } else {
            if (expected.len != result.len) break :blk min + 1;
        }
        return;
    };
    std.debug.print("first difference at index {d}\n", .{index});
    std.debug.print("expected:  {{", .{});
    for (expected, 0..) |obj,i| {
        if (i > 0) std.debug.print(", ", .{});
        std.debug.print("{f}", .{obj});
    }
    std.debug.print("}}\n", .{});
    std.debug.print("but found: {{", .{});
    for (result, 0..) |obj,i| {
        if (i > 0) std.debug.print(", ", .{});
        std.debug.print("{f}", .{obj});
    }
    std.debug.print("}}\n", .{});
    return error.TestExpectedEqual;
}
