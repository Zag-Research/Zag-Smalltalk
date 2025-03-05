const std = @import("std");
const config = @import("config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const checkEqual = @import("utilities.zig").checkEqual;
const Process = @import("process.zig");
const ProcessPtr = *Process;
const object = @import("zobject.zig");
const Object = object.Object;
const ClassIndex = object.ClassIndex;
const Nil = object.Nil;
const NotAnObject = object.NotAnObject;
const True = object.True;
const False = object.False;
const u64_MINVAL = object.u64_MINVAL;
const indexSymbol0 = object.Object.indexSymbol0;
const indexSymbol1 = object.Object.indexSymbol1;
const Context = @import("context.zig");
const heap = @import("heap.zig");
const HeapHeader = heap.HeapHeader;
const HeapObject = heap.HeapObject;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
const Format = heap.Format;
const Age = heap.Age;
const globalArena = @import("globalArena.zig");
const HeapAllocationPtr = globalArena.HeapAllocationPtr;
//const class = @import("class.zig");
const symbol = @import("symbol.zig");
const Sym = symbol.symbols;
const phi32 = @import("utilities.zig").inversePhi(u32);

pub const SP = *Stack;
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
    pub inline fn array(self: SP) [*]Object {
        return @ptrCast(self);
    }
    pub inline fn slice(self: SP, n: usize) []Object {
        return self.array()[0..n];
    }
    pub inline fn sliceTo(self: SP, ptr: anytype) []Object {
        return self.slice((@intFromPtr(ptr) - @intFromPtr(self)) / @sizeOf(Object));
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
    const ee = std.testing.expectEqual;
    var stack: [11]Object = undefined;
    const sp0 = @as(SP, @ptrCast(&stack[10]));
    sp0.top = True;
    try ee(True, stack[10]);
    const sp1 = sp0.push(False);
    try ee(True, stack[10]);
    try ee(False, stack[9]);
    _ = sp1.drop().push(Object.from(42));
    try ee(stack[9].to(i64), 42);
}
pub fn check(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP { // not embedded
    if (process.debugger()) |debugger|
        return @call(tailCall, debugger, .{ pc, sp, process, context, undefined });
    return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined });
}
pub const Extra = Object;
pub const ThreadedFn = packed struct {
    f: Fn,
    pub const Fn = *const fn (programCounter: PC, stackPointer: SP, process: *Process, context: *Context, signature: Extra) callconv(stdCall) SP;
};
pub const Signature = struct {
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
        return .{ .int = @bitCast(Internal{ .selector = @truncate(selector.rawU()), .class = class }) };
    }
    fn equals(self: Signature, other: Signature) bool {
        return self.int == other.int;
    }
    fn numArgs(self: Signature) u8 {
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
pub const CodeContextPtr = *Context;
pub const CompiledMethodPtr = *CompiledMethod;
pub const CompiledMethod = struct {
    header: HeapHeader,
    stackStructure: Object, // number of local values beyond the parameters
    signature: Signature,
    executeFn: ThreadedFn.Fn,
    jitted: ?ThreadedFn.Fn,
    code: [codeSize]Code, // will typically be a lot more then 1, as it will be the threaded version of the method
    const Self = @This();
    const codeSize = 1;
    pub const codeOffset = @offsetOf(CompiledMethod, "code");
    comptime {
        std.debug.assert(@offsetOf(Self, "header") == 0);
    }
    const codeOffsetInObjects = codeOffset / 8;
    pub fn init(name: Object, methodFn: ThreadedFn.Fn) Self {
        return Self{
            .header = HeapHeader.calc(ClassIndex.CompiledMethod, codeOffsetInObjects + codeSize, name.hash24(), Age.static, null, Object, false) catch unreachable,
            .stackStructure = Object.from(0),
            .signature = Signature.from(name, .none),
            .executeFn = methodFn,
            .jitted = methodFn,
            .code = .{Code.primOf(methodFn)},
        };
    }
    pub fn execute(self: *Self, sp: SP, process: *Process, context: *Context) callconv(stdCall) SP {
        const pc = PC.init(&self.code[0]);
        trace("\nexecute: {} {} {}", .{ pc, sp, self.signature });
        return pc.prim()(pc.next(), sp, process, context, self.signature.asObject());
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
pub const PC = packed struct {
    code: *const Code,
    pub const baseType = Code;
    const Self = @This();
    pub inline fn init(code: *const Code) PC {
        return .{ .code = code };
    }
    pub //inline
    fn method(self: PC) CompiledMethodPtr {
        return self.code.method;
    }
    pub inline fn codeAddress(self: PC) *const Code {
        return self.code.codePtr;
    }
    pub inline fn targetPC(self: PC) PC {
        return .{ .code = self.codeAddress() };
    }
    pub inline fn asThreadedFn(self: PC) ThreadedFn {
        return self.code.asThreadedFn();
    }
    pub //inline
    fn prim(self: PC) ThreadedFn.Fn {
        return self.code.threadedFn;
    }
    pub //inline
    fn object(self: PC) Object {
        return self.code.object;
    }
    pub inline fn asCode(self: PC) Code {
        return self.code.*;
    }
    pub inline fn asCodePtr(self: PC) *const Code {
        return self.code;
    }
    pub inline fn uint(self: PC) u64 {
        return self.code.object.to(u64);
    }
    pub inline fn int(self: PC) i64 {
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
};
pub const Code = union {
    object: Object,
    threadedFn: ThreadedFn.Fn,
    method: *CompiledMethod,
    codePtr: *Code,
    int: u64,
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
        return Code{ .object = Object.from(o) };
    }
    pub inline fn methodOf(method: *CompiledMethod) Code {
        return Code{ .method = method };
    }
    pub inline fn codePtrOf(code: *Code, offs: i64) Code {
        const addr = @as([*]Code, @ptrCast(code)) + 1;
        return Code{ .codePtr = @ptrCast(addr + @as(u64, @bitCast(offs))) };
    }
    pub inline fn offset(o: i56) Code {
        return Code{ .object = Object.thunkImmediate(Object.from(o)).? };
    }
    pub inline fn prim(self: Code) ThreadedFn.Fn {
        return self.threadedFn;
    }
    pub inline fn asThreadedFn(self: Code) ThreadedFn {
        return .{ .f = self.threadedFn };
    }
    pub fn end(_: PC, sp: SP, _: *Process, _: *Context, _: Extra) callconv(stdCall) SP { // not embedded
        return sp;
    }
    pub fn panic(_: PC, _: SP, _: *Process, _: *Context, _: Extra) callconv(stdCall) SP { // not embedded
        @panic("not implemented");
    }
    fn noOp(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) callconv(stdCall) SP {
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, signature });
    }
    const endCode = [CompiledMethod.codeSize]Code{Code.primOf(end)};
    pub const endThread = PC.init(@ptrCast(&endCode));
    pub fn format(
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
pub fn intOf(comptime str: []const u8) u12 {
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
pub fn countNonLabels(comptime tup: anytype) usize {
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
    try expectEqual(10, countNonLabels(.{
        ":abc",
        &Code.noOp,
        "def",
        True,
        comptime Object.from(42),
        ":def",
        "abc",
        "^",
        3,
        "1mref",
        null,
        ClassIndex.True,
    }));
}
pub fn combine(size: type, tup: anytype) comptime_int {
    comptime var n: u56 = 0;
    comptime var shift = 0;
    inline for (tup) |field| {
        switch (@TypeOf(field)) {
            comptime_int => n |= @as(u56, @as(size, field)) << shift,
            else => n |= @as(u56, @as(size, @intFromEnum(field))) << shift,
        }
        shift += @typeInfo(size).int.bits;
    }
    return n;
}
pub fn combine14(tup: anytype) comptime_int {
    return combine(u14, tup);
}
pub fn combine24(tup: anytype) comptime_int {
    return combine(u24, tup);
}
test "combiners" {
    std.debug.print("Test: combiners\n", .{});
    const expectEqual = std.testing.expectEqual;
    try expectEqual(16384 + 2, combine14(.{ 2, 1 }));
    try expectEqual(229391, combine14([_]ClassIndex{ .SmallInteger, .Symbol }));
    try expectEqual(16777216 + 2, combine24(.{ 2, 1 }));
}
pub fn CompileTimeMethod(comptime counts: usize) type {
    const codes = counts + (if (config.is_test) 1 else 0);
    return struct { // structure must exactly match CompiledMethod
        header: HeapHeader,
        stackStructure: Object, // number of local values beyond the parameters
        signature: Signature,
        executeFn: ThreadedFn.Fn,
        jitted: ThreadedFn.Fn,
        code: [codes]Code,
        offsets: [codes]bool,
        const codeOffsetInUnits = CompiledMethod.codeOffsetInObjects;
        const Self = @This();
        // comptime {
        //     std.debug.assert(@offsetOf(Self, "header") == 0);
        //     if (checkEqual(@offsetOf(CompiledMethod, "code"), @offsetOf(Self, "code"))) |s|
        //         @compileError("CompiledMethod prefix not the same as CompileTimeMethod == " ++ s);
        // }
        const cacheSize = 0; //@sizeOf(SendCacheStruct) / @sizeOf(Code);
        pub fn init(comptime name: Object, comptime locals: u16, comptime maxStack: u16, function: ?ThreadedFn.Fn, class: ClassIndex, tup: anytype) Self {
            const header = HeapHeader.calc(.CompiledMethod, codeOffsetInUnits + codes, name.hash24(), Age.static, null, Object, false) catch @compileError("method too big");
            //  @compileLog(codes,refs,footer,heap.Format.allocationInfo(5,null,0,false));
            const f = function orelse &Code.noOp;
            var method = Self{
                .header = header,
                .signature = Signature.from(name, class),
                .stackStructure = Object.packedInt(locals, maxStack, locals + name.numArgs()),
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
                    Object, bool, @TypeOf(null), comptime_int => {
                        code[n] = Code.objectOf(field);
                        n = n + 1;
                    },
                    ClassIndex => {
                        code[n] = Code.objectOf(@intFromEnum(field));
                        n = n + 1;
                    },
                    else => {
                        if (field.len == 1 and field[0] == '^') {
                            code[n] = Code.offset(0);
                            method.offsets[n] = true;
                            n = n + 1;
                        } else if (field[0] != ':') {
                            code[n] = Code.offset(lookupLabel(tup, field) - n - 1);
                            method.offsets[n] = true;
                            n = n + 1;
                        }
                    },
                }
            }
            if (config.is_test) code[n] = Code.primOf(&Code.end);
            return method;
        }
        pub fn withCodeX(name: Object, locals: u16, maxStack: u16, code: [codes]Code) Self {
            const footer = HeapHeader.calcHeapHeader(ClassIndex.CompiledMethod, codeOffsetInUnits + codes, name.hash24(), Age.static, null, Object, false) catch @compileError("method too big");
            return .{
                .header = footer.asHeader(),
                .selector = name,
                .stackStructure = Object.packedInt(locals, maxStack, locals + name.numArgs()),
                .code = code,
            };
        }
        fn cacheOffset(_: *Self, codeOffs: usize, cacheOffs: usize) u32 {
            return @truncate((codes - codeOffs) + (cacheOffs * cacheSize));
        }
        pub inline fn asCompiledMethodPtr(self: *const Self) *CompiledMethod {
            return @as(*CompiledMethod, @ptrCast(@constCast(self)));
        }
        pub fn setLiterals(self: *Self, replacements: []const Object, refReplacements: []const Object) void {
            _ = .{ self, replacements, refReplacements, unreachable };
            // //trace("\nsetLiterals: 0x{x:0>16} {any}", .{ self.selector.u(), replacements });
            // for (&self.code) |*c| {
            //     if (c.asObject().isIndexSymbol0()) {
            //         const index = c.asObject().indexNumber();
            //         c.* = Code.objectOf(replacements[index]);
            //     }
            // }
            // if (self.signature.isIndexSymbol()) {
            //     const index = self.signature.indexNumber();
            //     const replacement = if (index < 0x10000) replacements[index] else refReplacements[index & 0xffff];
            //     _ = replacement;
            //     unreachable;
            //     // self.stackStructure.classIndex = @enumFromInt(@intFromEnum(self.stackStructure.classIndex) - (indexSymbol0(0).numArgs() - replacement.numArgs()));
            //     // self.signature.selectorHash = replacement.hash32();
            // }
            // for (refReplacements, &self.references) |obj, *srefs|
            //     srefs.* = obj;
            // if (self.references.len > 0) {
            //     for (&self.code) |*c| {
            //         if (c.asObject().isIndexSymbol1()) {
            //             const newValue = (@intFromPtr(&self.references[c.asObject().indexNumber() & (Code.refFlag - 1)]) - @intFromPtr(c)) / @sizeOf(Object) - 1;
            //             c.* = Code.uintOf(newValue);
            //         }
            //     }
            // }
        }
        pub fn resolve(self: *Self) void {
            for (&self.code, &self.offsets) |*c, isOffset| {
                if (isOffset) {
                    const offset = c.object.thunkImmediateValue().?.to(i64);
                    if (offset == 0) {
                        c.* = Code.methodOf(@ptrCast(self));
                    } else {
                        c.* = Code.codePtrOf(c, offset);
                    }
                }
            }
            // if (self.signature.isIndexSymbol()) {
            //     const index = self.signature.indexNumber();
            //     const replacement = if (index < 0x10000) replacements[index] else refReplacements[index & 0xffff];
            //     _ = replacement;
            //     unreachable;
            //     // self.stackStructure.classIndex = @enumFromInt(@intFromEnum(self.stackStructure.classIndex) - (indexSymbol0(0).numArgs() - replacement.numArgs()));
            //     // self.signature.selectorHash = replacement.hash32();
            // }
        }
        pub fn getCodeSize(_: *Self) usize {
            return codes;
        }
        pub fn findObject(self: *Self, search: Object) usize {
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
        True,
        comptime Object.from(42),
        ":def",
        "abc",
        "*",
        "^",
        3,
        "1mref",
        null,
    }));
    var r1 = c1.init(Sym.value, 2, 3, null, .none, .{});
    //TODO    r1.setLiterals(Object.empty, &[_]Object{Nil, True});
    try expectEqual(r1.getCodeSize(), 9);
}
pub fn compiledMethodType(comptime codeSize: comptime_int) type {
    return CompileTimeMethod(.{ .codes = codeSize });
}
pub fn compileMethod(comptime name: Object, comptime locals: comptime_int, comptime maxStack: comptime_int, comptime class: ClassIndex, comptime tup: anytype) CompileTimeMethod(countNonLabels(tup)) {
    return compileMethodWith(name, locals, maxStack, class, null, tup);
}
pub fn compileMethodWith(comptime name: Object, comptime locals: comptime_int, comptime maxStack: comptime_int, comptime class: ClassIndex, comptime verifier: ?ThreadedFn.Fn, comptime tup: anytype) CompileTimeMethod(countNonLabels(tup)) {
    @setEvalBranchQuota(20000);
    const methodType = CompileTimeMethod(countNonLabels(tup));
    return methodType.init(name, locals, maxStack, verifier, class, tup);
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
        //        &p.setupSend,
        "def",
        True,
        comptime Object.from(42),
        ":def",
        "abc",
        //"*",
        "^",
        3,
        //"1mref",
        null,
    }, def);
    try expectEqual(3, c1);
}
// const stdout = std.io.getStdOut().writer(); // outside of a functions, stdout causes error on Windows
const print = std.io.getStdOut().writer().print;
const p = @import("controlPrimitives.zig");
test "compiling method" {
    std.debug.print("Test: compiling method\n", .{});
    const expectEqual = std.testing.expectEqual;
    //@compileLog(&p.send);
    var m = compileMethod(Sym.yourself, 0, 0, .none, .{
        ":abc", &p.send,
        "def",  True,
        42,     ":def",
        "abc",  "^",
        3,      null,
    });
    //TODO    m.setLiterals(&[_]Object{Sym.value}, &[_]Object{Object.from(42)});
    const t = m.code[0..];
    try expectEqual(8, t.len);
    const debugging = false;
    if (debugging) {
        @setRuntimeSafety(false);
        trace("\nm: 0x{x:0>16}", .{@intFromPtr(&m)});
        for (t, 0..) |tv, idx|
            trace("\nt[{}]: 0x{x:0>16}", .{ idx, tv.object.rawU() });
    }
    m.resolve();
    if (debugging) {
        @setRuntimeSafety(false);
        for (t, 0..) |*tv, idx|
            trace("\nt[{}]=0x{x:0>8}: 0x{x:0>16}", .{ idx, @intFromPtr(tv), tv.object.rawU() });
    }
    //    try expectEqual(t.prim,controlPrimitives.noop);
    try expectEqual(t[0].prim(), &p.send);
    try expectEqual(t[1].codePtr, &m.code[4]);
    try expectEqual(t[2].object, True);
    try expectEqual(t[3].object, Object.from(42));
    try expectEqual(t[4].codePtr, &m.code[0]);
    try expectEqual(t[5].method, m.asCompiledMethodPtr());
    try expectEqual(t[6].object, Object.from(3));
    try expectEqual(t[7].object, Nil);
}

pub fn CompileTimeObject(comptime counts: usize) type {
    const codes = counts;
    return struct {
        objects: [codes]Object,
        const Self = @This();
        pub fn init(comptime tup: anytype) Self {
            var obj = Self{
                .objects = undefined,
            };
            const objects = obj.objects[0..];
            comptime var last = -1;
            comptime var n = 0;
            inline for (tup) |field| {
                switch (@TypeOf(field)) {
                    Object, bool, @TypeOf(null), comptime_int => {
                        objects[n] = Object.from(field);
                        n += 1;
                    },
                    ClassIndex => {
                        if (last >= 0)
                            objects[last] = @as(HeapHeader, @bitCast(objects[last])).withLength(n - last - 1).o();
                        const header = HeapHeader.calc(field, 0, 0xffffff, Age.static, null, Object, false) catch unreachable;
                        objects[n] = header.o();
                        last = n;
                        n += 1;
                    },
                    else => {
                        if (field.len >= 1 and field[0] >= '0' and field[0] <= '9') {
                            objects[n] = Object.thunkImmediate(symbol.fromHash32(comptime intOf(field[0..]))).?;
                            n += 1;
                        } else if (field[0] != ':') {
                            objects[n] = Object.thunkImmediate(Object.from(lookupLabel(tup, field))).?;
                            n += 1;
                        }
                    },
                }
            }
            if (last >= 0)
                objects[last] = @as(HeapHeader, @bitCast(objects[last])).withLength(n - last - 1).o();
            return obj;
        }
        pub fn setLiterals(self: *Self, replacements: []const Object, classes: []const ClassIndex) void {
            var includesPointer = false;
            for (&self.objects) |*o| {
                if (o.thunkImmediateValue()) |ob| {
                    if (ob.isInt()) {
                        o.* = Object.from(&self.objects[ob.hash]);
                        includesPointer = true;
                    } else {
                        const obj = replacements[ob.toNatNoCheck()];
                        if (obj.isMemoryAllocated()) includesPointer = true;
                        o.* = obj;
                    }
                } else { // there is a miniscule chance of false-positive
                    var header: HeapHeader = @bitCast(o.*);
                    if (header.length < 1024 and
                        header.hash == 0xffffff and
                        header.format == .notIndexable and
                        header.age == .static)
                    {
                        if (@intFromEnum(header.classIndex) > @intFromEnum(ClassIndex.max))
                            header.classIndex = classes[@intFromEnum(ClassIndex.replace0) - @intFromEnum(header.classIndex)];
                        if (includesPointer)
                            header.format = .notIndexableWithPointers;
                        header.hash ^= @truncate(@intFromPtr(o) *% phi32);
                        o.* = @bitCast(header);
                        includesPointer = false;
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
            return Object.from(self.asHeapObjectPtr());
        }
    };
}
pub fn compileObject(comptime tup: anytype) CompileTimeObject(countNonLabels(tup)) {
    @setEvalBranchQuota(20000);
    const objType = CompileTimeObject(countNonLabels(tup));
    return objType.init(tup);
}
test "compileObject" {
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
        ":second",
        c.replace0, // second HeapObject - runtime ClassIndex #0
        ":third",
        c.Dispatch, // third HeapObject
        True,
        "def",
    });
    const debugging = false;
    if (debugging) {
        for (&o.objects, 0..) |*ob, idx|
            trace("\no[{}]: 0x{x:0>16}", .{ idx, ob.rawU() });
    }
    o.setLiterals(&[_]Object{ Nil, True }, &[_]ClassIndex{@enumFromInt(0xdead)});
    if (debugging) {
        for (&o.objects, 0..) |*ob, idx|
            trace("\no[{}]=0x{x:0>8}: 0x{x:0>16}", .{ idx, @intFromPtr(ob), ob.rawU() });
    }

    try expect(o.asObject().isHeapObject());
    try expect(o.objects[8].equals(o.asObject()));
    //    try expectEqual(@as(u48, @truncate(o.asObject().rawU())), @as(u48, @truncate(@intFromPtr(&o.objects[8]))));
    try expect(o.objects[2].equals(Object.from(42)));
    try expect(o.objects[3].equals(True));
    const h1: HeapObjectConstPtr = @ptrCast(&o.objects[0]);
    try expectEqual(h1.header.length, 4);
    try expect(!h1.header.isIndexable());
    try expect(h1.header.isStatic());
    try expect(h1.header.isUnmoving());
    const h2: HeapObjectConstPtr = @ptrCast(&o.objects[5]);
    try expectEqual(@intFromEnum(h2.header.classIndex), 0xdead);
    try expectEqual(h2.header.length, 0);
    const h3: HeapObjectConstPtr = @ptrCast(&o.objects[6]);
    try expectEqual(h3.header.classIndex, c.Dispatch);
    try expectEqual(h3.header.length, 2);
    try expectEqual(h3.header.age, .static);
    try expectEqual(h3.header.format, .notIndexable);
}
// test "method object" {
// std.debug.print("Test: method object\n",.{});
//     // + aNumber
//     //     "Primitive. Add the receiver to the argument and answer with the result
//     //     if it is a SmallInteger. Fail if the argument or the result is not a
//     //     SmallInteger  Essential  No Lookup. See Object documentation whatIsAPrimitive."

//     //     <primitive: 1>
//     //     ^ super + aNumber
//     const c = ClassIndex;
//     var o = compileObject(.{
//         ":super1",
//         c.ASSuper,

//         0,
//         ":aNumber",
//         c.ASArg,

//         "super1",
//         Sym.@"+",
//         "aNumber",
//         ":send1",
//         c.ASSend,

//         "send1",
//         ":return1",
//         c.ASReturn,

//         "return1",
//         // another statement
//         ":body",
//         c.ASSequence,

//         "body",
//         1,
//         Sym.@"+",
//         ":first",
//         c.Method, // first HeapObject
//     });
//     o.setLiterals(&[_]Object{}, &[_]ClassIndex{});
//     const method = o.asObject();
//     _ = method;
// }
pub const embedded = @import("controlPrimitives.zig");
fn callMethod(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
    _ = .{ pc, sp, process, context, unreachable };
}
pub const controlPrimitivesX = struct {
    const ContextPtr = CodeContextPtr;

    pub fn verifyMethod(pc: PC, sp: SP, process: *Process, context: *Context, signature: Extra) callconv(stdCall) SP {
        const method = pc.method();
        trace("\nverifyMethod: {*} {} {}", .{ method, signature, method.signature });
        if (!method.signature.equals(signature)) {
            trace(" failed match", .{});
            return @call(tailCall, pc.prim2(), .{ pc.next2(), sp, process, context, signature });
        }
        const newPc = PC.init(@ptrCast(&method.code));
        trace(" newPc={} {}", .{ newPc, newPc.prim() });
        return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, undefined });
    }
    // pub fn ifTrue(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
    //     const process = tfAsProcess(_process);
    //     const context = tfAsContext(_context);
    //     trace("\nifTrue: {any}", .{context.stack(sp, process)});
    //     const v = sp.top;
    //     if (True.equals(v)) return @call(tailCall, branch, .{ pc, sp.drop(), process, context, undefined });
    //     if (False.equals(v)) return @call(tailCall, pc.prim2(), .{ pc.next2(), sp.drop(), process, context, undefined });
    //     @panic("non boolean");
    // }
    // pub fn ifFalse(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
    //     const process = tfAsProcess(_process);
    //     const context = tfAsContext(_context);
    //     trace("\nifFalse: {any}", .{context.stack(sp, process)});
    //     const v = sp.top;
    //     if (False.equals(v)) return @call(tailCall, branch, .{ pc, sp.drop(), process, context, undefined });
    //     if (True.equals(v)) return @call(tailCall, pc.next().prim(), .{ pc.skip(2), sp.drop(), process, context, undefined });
    //     @panic("non boolean");
    // }
    // pub fn ifNil(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
    //     const v = sp.top;
    //     if (Nil.equals(v)) return @call(tailCall, branch, .{ pc, sp.drop(), process, context, undefined });
    //     return @call(tailCall, pc.next().prim(), .{ pc.skip(2), sp.drop(), process, context, undefined });
    // }
    // pub fn ifNotNil(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
    //     const v = sp.top;
    //     if (Nil.equals(v)) return @call(tailCall, pc.next().prim(), .{ pc.skip(2), sp.drop(), process, context, undefined });
    //     return @call(tailCall, branch, .{ pc, sp.drop(), process, context, undefined });
    // }
    pub fn primFailure(_: PC, _: SP, _: *Process, _: *Context, _: Extra) callconv(stdCall) SP {
        @panic("primFailure");
    }
    pub fn replaceLiteral(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
        sp.top = pc.object();
        trace("\nreplaceLiteral: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.prim2(), .{ pc.next2(), sp, process, context, undefined });
    }
    pub fn replaceLiteral0(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
        sp.top = Object.from(0);
        trace("\nreplaceLiteral0: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined });
    }
    pub fn replaceLiteral1(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
        sp.top = Object.from(1);
        trace("\nreplaceLiteral0: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined });
    }
    pub fn pushLiteral0(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
        const newSp = sp.push(Object.from(0));
        trace("\npushLiteral0: {any}", .{context.stack(newSp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteral1(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
        const newSp = sp.push(Object.from(1));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteral2(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
        const newSp = sp.push(Object.from(2));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteral_1(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
        const newSp = sp.push(Object.from(-1));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteralIndirect(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
        const newSp = sp.push(pc.literalIndirect());
        return @call(tailCall, pc.prim2(), .{ pc.next2(), newSp, process, context, undefined });
    }
    pub fn pushLiteralNil(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
        const newSp = sp.push(Nil);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteralTrue(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
        const newSp = sp.push(True);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteralFalse(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
        const newSp = sp.push(False);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn printStack(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
        trace("\nstack: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined });
    }
    pub fn returnNoContextSwitchToThreaded(_: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
        trace("\nreturnNoContext: {any} N={} T={}", .{ context.stack(sp, process), context.getNPc(), context.getTPc() });
        const tPc = context.getTPc();
        const nPc = tPc.prev().prim();
        return @call(tailCall, nPc, .{ tPc, sp, process, context, undefined });
    }
    pub fn isCallerInThreadedMode(pc: PC, sp: SP, process: *Process, context: *Context, _: Extra) callconv(stdCall) SP {
        trace("\nreturnNoContext: {any} N={} T={}", .{ context.stack(sp, process), context.getNPc(), context.getTPc() });
        const tPc = context.getTPc();
        const nPc = tPc.prev().prim();
        const newSp = sp.push(if (nPc == context.getNPc()) True else False);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
};
pub const Execution = struct {
    process: Process align(1024),
    ctxt: Context,
    sp: SP,
    const Self = @This();
    pub fn new() Self {
        return Self{
            .process = Process.new(),
            .ctxt = Context.init(),
            .sp = undefined,
        };
    }
    var yourself = CompiledMethod.init(Sym.noFallback, Code.end);
    pub fn init(self: *Self, stackObjects: ?[]const Object) void {
        self.ctxt.method = &yourself;
        self.process.init();
        if (stackObjects) |source| {
            self.initStack(source);
        } else self.sp = self.process.endOfStack();
    }
    fn initStack(self: *Self, source: []const Object) void {
        self.sp = self.process.endOfStack().reserve(source.len);
        self.process.setSp(self.sp);
        for (source, self.sp.slice(source.len)) |src, *stck|
            stck.* = src;
        trace("\ninitial-stack: {any}", .{self.process.getStack(self.sp)});
    }
    pub fn stack(self: *Self, sp: SP) []Object {
        self.sp = sp;
        trace("\nfinal-stack: {any}", .{self.process.getStack(sp)});
        return self.ctxt.stack(self.sp, &self.process);
    }
    pub fn run(self: *Self, source: []const Object, ptr: anytype) []Object {
        const method: CompiledMethodPtr = @constCast(@ptrCast(ptr));
        self.initStack(source);
        self.ctxt.setReturn(Code.endThread);
        trace("\nrun: {x} {x}", .{ &self.process, &self.ctxt });
        return self.stack(method.execute(self.sp, &self.process, &self.ctxt));
    }
    fn mainSendTo(selector: Object, target: Object) !void {
        std.debug.print("Sending: {} to {}\n", .{ selector, target });
        var exec = Self.new();
        const args = [_]Object{target};
        exec.initStack(&args);
        exec.ctxt.setReturn(Code.endThread);
        return error.NotImplemented;
    }
};

pub const mainSendTo = Execution.mainSendTo;

extern fn llvmPL(_: *anyopaque, _: *anyopaque, _: *anyopaque, _: *anyopaque, c_int) *anyopaque;
const llvmPLCM = CompiledMethod.init(Sym.yourself, @ptrCast(&llvmPL));
test "simple llvm" {
    std.debug.print("Test: simple llvm\n", .{});
    const expectEqual = std.testing.expectEqual;
    Process.resetForTest();
    const pl = if (true) &p.pushLiteral else llvmPLCM;
    var method = compileMethod(Sym.yourself, 0, 0, .none, .{
        pl,                    0,
        &p.returnTopNoContext,
    });
    var te = Execution.new();
    te.init(null);
    var objs = [_]Object{ Nil, True };
    const result = te.run(objs[0..], &method);
    try expectEqual(result.len, 3);
    try expectEqual(result[0], Object.from(0));
    try expectEqual(result[1], Nil);
    try expectEqual(result[2], True);
}
test "llvm external" {
    std.debug.print("Test: llvm external\n", .{});
    const expectEqual = std.testing.expectEqual;
    Process.resetForTest();
    const pl = if (true) &p.pushLiteral else llvmPLCM;
    var method = compileMethod(Sym.yourself, 1, 0, .none, .{
        &p.pushContext,   "^",
        ":label1",        &p.pushLiteral,
        42,               &p.popLocal,
        0,                &p.pushLocal,
        0,                pl,
        0,                &p.pushLiteral,
        true,             &p.classCase,
        ClassIndex.False, "label3",
        &p.branch,        "label2",
        ":label3",        &p.pushLocal,
        0,                ":label4",
        &p.returnTop,     ":label2",
        pl,               0,
        &p.branch,        "label4",
    });
    method.resolve();
    const debugging = false;
    if (debugging) {
        @setRuntimeSafety(false);
        for (&method.code, 0..) |*tv, idx|
            trace("\nt[{}]=0x{x:0>8}: 0x{x:0>16}", .{ idx, @intFromPtr(tv), tv.object.rawU() });
    }
    var objs = [_]Object{ Nil, Nil, Nil };
    var te = Execution.new();
    te.init(null);
    const result = te.run(objs[0..], &method);
    try expectEqual(result.len, 1);
    try expectEqual(result[0], Object.from(0));
}
