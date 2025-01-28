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
        std.debug.assert(@offsetOf(Stack,"top")==0);
        std.debug.assert(@offsetOf(Stack,"next")==@sizeOf(Object));
        std.debug.assert(@offsetOf(Stack,"third")==@sizeOf(Object)*2);
    }
    pub inline fn lessThan(self: SP, other: SP) bool {
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
    pub inline fn at(self: SP, n: usize) Object {
        return self.array()[n];
    }
    pub inline fn atPut(self: SP, n: usize, o: Object) void {
        self.array()[n] = o;
    }
};
test "Stack" {
    std.debug.print("Test: Stack\n",.{});
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
pub fn check(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP { // not embedded
    if (process.debugger()) |debugger|
        return @call(tailCall, debugger, .{ pc, sp, process, context, undefined });
    return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined });
}
pub const ThreadedFn = packed struct {
    f: Fn,
    pub const Fn = *const fn (programCounter: PC, stackPointer: SP, process: *Process, context: *Context, signature: MethodSignature) callconv(stdCall) SP;
};
pub const MethodSignature = union {
    int: u64,
    const Internal = packed struct {
        selector: u40,
        class: ClassIndex,
    };
    const nil: MethodSignature = .{.int = @bitCast(Nil)};
    pub fn isNil(self: MethodSignature) bool {
        return self.int==Nil.rawU();
    }
    pub fn hash(self: MethodSignature) u32 {
        return @truncate(self.int);
    }
    pub fn from(selector: Object, class: ClassIndex) MethodSignature {
        return .{ .int = selector.rawU() + (@as(u64,@intFromEnum(class))<<40)};
    }
    fn equals(self: MethodSignature, other: MethodSignature) bool {
        return self.int == other.int;
    }
    fn numArgs(self: MethodSignature) u8 {
        return @truncate(self.int >> 32);
    }
    fn isIndexSymbol(self: MethodSignature) bool {
        return self.numArgs() == 0xff;
    }
    fn indexNumber(self: MethodSignature) u24 {
        return @truncate(self.int >> 8);
    }
    fn asSymbol(self: MethodSignature) Object {
        return @bitCast(self.int & 0xffffffffff);
    }
    fn getClassIndex(self: MethodSignature) u64 {
        return self.int>>40;
    }
    fn getClass(self: MethodSignature) ClassIndex {
        return @enumFromInt(self.getClassIndex());
    }
    fn setClass(self: *MethodSignature, class: ClassIndex) void {
        self.int = (self.int & 0xffffffffff) + (@as(u64,@intFromEnum(class))<<40);
    }
     pub fn format(
         self: MethodSignature,
         comptime fmt: []const u8,
         options: std.fmt.FormatOptions,
         writer: anytype,
     ) !void {
         _ = .{ fmt, options };
         try writer.print("MethodSignature({},{})", .{ self.asSymbol(), self.getClass() });
     }
 };
pub const CodeContextPtr = *Context;
pub const CompiledMethodPtr = *CompiledMethod;
pub const CompiledMethod = struct {
    header: HeapHeader,
    stackStructure: Object, // number of local values beyond the parameters
    signature: MethodSignature,
    executeFn: ThreadedFn,
    jitted: ThreadedFn,
    code: [codeSize]Code, // will typically be a lot more then 1, as it will be the threaded version of the method
    const Self = @This();
    const codeSize = 1;
    pub const codeOffset = @offsetOf(CompiledMethod, "code");
    comptime {
        std.debug.assert(@offsetOf(Self,"header")==0);
    }
    const codeOffsetInObjects = codeOffset / 8;
    pub fn init(name: Object, methodFn: ThreadedFn) Self {
        return Self{
            .header = HeapHeader.calc(ClassIndex.CompiledMethod, codeOffsetInObjects + codeSize, name.hash24(), Age.static, null, Object, false) catch unreachable,
            .stackStructure = Object.from(0),
            .signature = MethodSignature.from(name,.none),
            .executeFn = methodFn,
            .jitted = methodFn,
            .code = .{.{.prim = .{.f = Code.panic}}},
        };
    }
    pub fn execute(self: *Self, sp: SP, process: *Process, context: *Context) callconv(stdCall) SP {
        const pc = PC.init(&self.code[0]);
        trace("\nexecute: {} {} {}", .{ pc, sp, self.signature });
        return pc.prim()(pc.next(), sp, process, context, self.signature);
    }
    pub fn forDispatch(self: *Self, class: ClassIndex) void {
        self.signature.setClass(class);
        addMethod(self);
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
    pub inline fn method(self: PC) CompiledMethodPtr {
        return @constCast(self.code.method);
    }
    pub inline fn codeAddress(self: PC) *const Code {
        return self.code.codeAddress;
    }
    pub inline fn targetPC(self: PC) PC {
        return .{ .code = self.code.codeAddress};
    }
    pub inline fn prim(self: PC) ThreadedFn.Fn {
        return self.code.prim.f;
    }
    pub inline fn justPrim(self: PC) ThreadedFn {
        return self.code.prim;
    }
    pub inline fn uint(self: PC) u64 {
        return self.code.uint;
    }
    pub inline fn int(self: PC) i64 {
        return self.code.int;
    }
    pub inline fn object(self: PC) Object {
        return self.code.object;
    }
    pub inline fn asCode(self: PC) Code {
        return self.code.*;
    }
    pub inline fn asCodePtr(self: PC) *const Code {
        return self.code;
    }
    pub inline fn next(self: PC) PC {
        return asPC(self.array() + 1);
    }
    pub inline fn prev(self: PC) PC {
        return asPC(self.array() - 1);
    }
    pub inline fn prim2(self: PC) ThreadedFn.Fn {
        return self.array()[1].prim.f;
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
    pub inline fn literalIndirect(self: PC) Object {
        const offset = self.code.uint;
        return @as(*const Object, @ptrCast(@as([*]const Code, @ptrCast(self.code)) + 1 + offset)).*;
    }
    inline fn asPC(ptr: [*]const Code) PC {
        return .{ .code = @ptrCast(ptr) };
    }
    inline fn array(self: PC) [*]const Code {
        return @ptrCast(self.code);
    }
};
pub const Code = union {
    prim: ThreadedFn,
    int: i64,
    uint: u64,
    object: Object,
    header: heap.HeapObject,
    method: *const CompiledMethod,
    codeAddress: *const Code,
    const refFlag = 1024;
    pub inline fn primOf(pp: ThreadedFn) Code {
        return Code{ .prim = pp };
    }
    inline fn intOf(i: i64) Code {
        return Code{ .int = i };
    }
    pub inline fn uintOf(u: u64) Code {
        return Code{ .uint = u };
    }
    pub inline fn asObject(self: Code) Object {
        @setRuntimeSafety(false);
        return self.object;
    }
    pub inline fn objectOf(o: Object) Code {
        return Code{ .object = o };
    }
    inline fn ref1Of(comptime u: u12) Code {
        return Code{ .object = indexSymbol1(u) };
    }
    inline fn headerOf(h: heap.HeapObject) Code {
        return Code{ .header = h };
    }
    pub inline fn codeRefOf(c: [*]const Code) Code {
        return Code{ .uint = @intFromPtr(@constCast(c)) };
    }
    pub fn end(_: PC, sp: SP, _: *Process, _: *Context, _: MethodSignature) callconv(stdCall) SP { // not embedded
        return sp;
    }
    pub fn panic(_: PC, _: SP, _: *Process, _: *Context, _: MethodSignature) callconv(stdCall) SP { // not embedded
        @panic("not implemented");
    }
    var endCode = [CompiledMethod.codeSize]Code{.{ .prim = .{.f = end }}};
    pub const endThread = PC.init(@ptrCast(&endCode));
    pub fn format(
        self: *const Code,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        const obj = self.asObject();
        if (!obj.isDouble()) {
            try writer.print("{}", .{obj});
        } else if (self.int >= -100 and self.int < 100) {
            try writer.print("({})", .{self.int});
        } else try writer.print("0x{x}", .{self.uint});
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
    std.debug.print("Test: intOf\n",.{});
    const expectEqual = std.testing.expectEqual;
    try expectEqual(comptime intOf("012Abc"), 12);
    try expectEqual(comptime intOf("1230Abc"), 1230);
}
pub const CountSizes = struct { codes: usize, refs: usize = 0, objects: usize = 0, caches: usize = 0 };
pub fn countNonLabels(comptime tup: anytype) CountSizes {
    comptime var c = 0;
    comptime var r = 0;
    comptime var o = 0;
    inline for (tup) |field| {
        switch (@TypeOf(field)) {
            Object => {
                c += 1;
                if (!comptime field.isLiteral()) @compileError("use reference for non-literal object");
                o += 1;
            },
            @TypeOf(null) => {
                c += 1;
                o += 1;
            },
            comptime_int, comptime_float, ClassIndex => {
                c += 1;
            },
            else => switch (@typeInfo(@TypeOf(field))) {
                .pointer => |pointer| {
                    switch (@typeInfo(pointer.child)) {
                        .array => switch (field[0]) {
                            ':' => {},
                            '0'...'9' => {
                                r = comptime @max(r, intOf(field[0..]) + 1);
                                c += 1;
                            },
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
            },
        }
    }
    return .{ .codes = c, .refs = r, .objects = o };
}
test "countNonLabels" {
    std.debug.print("Test: countNonLabels\n",.{});
    const expectEqual = std.testing.expectEqual;
    const r1 = countNonLabels(.{
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
        ClassIndex.True,
    });
    try expectEqual(r1.codes, 11);
    try expectEqual(r1.refs, 2);
    try expectEqual(r1.objects, 3);
}
fn noOp(pc: PC, sp: SP, process: *Process, context: *Context, signature: MethodSignature) callconv(stdCall) SP {
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, signature });
    }

pub fn CompileTimeMethod(comptime counts: CountSizes) type {
    const codes = counts.codes;
    const refs = counts.refs;
    return struct { // structure must exactly match CompiledMethod
        header: HeapHeader,
        stackStructure: Object, // number of local values beyond the parameters
        signature: MethodSignature,
        executeFn: ThreadedFn,
        jitted: ThreadedFn,
        code: [codes]Code,
        references: [refs]Object,
        const codeOffsetInUnits = CompiledMethod.codeOffsetInObjects;
        const Self = @This();
        comptime {
            std.debug.assert(@offsetOf(Self,"header")==0);
                 if (checkEqual(CompiledMethod.codeOffset,@offsetOf(Self,"code"))) |s|
                @compileError("CompiledMethod prefix not the same as CompileTimeMethod == " ++ s);
        }
        const cacheSize = 0; //@sizeOf(SendCacheStruct) / @sizeOf(Code);
        pub fn init(comptime name: Object, comptime locals: u16, comptime maxStack: u16, function: ?ThreadedFn, class: ClassIndex) Self {
            const header = HeapHeader.calc(.CompiledMethod, codeOffsetInUnits + codes + refs, name.hash24(), Age.static, null, Object, false) catch @compileError("too many refs");
            //  @compileLog(codes,refs,footer,heap.Format.allocationInfo(5,null,0,false));
            const f = function orelse &noOp;  
            return .{
                .header = header,
                .signature = MethodSignature.from(name,class),
                .stackStructure = Object.packedInt(locals, maxStack, locals + name.numArgs()),
                .executeFn = .{ .f = f},
                .jitted = .{ .f = f},
                .code = undefined,
                .references = [_]Object{object.NotAnObject} ** refs,
            };
        }
        pub fn withCode(name: Object, locals: u16, maxStack: u16, code: [codes]Code) Self {
            const footer = HeapHeader.calcHeapHeader(ClassIndex.CompiledMethod, codeOffsetInUnits + codes + refs, name.hash24(), Age.static, null, Object, false) catch @compileError("too many refs");
            return .{
                .header = footer.asHeader(),
                .selector = name,
                .stackStructure = Object.packedInt(locals, maxStack, locals + name.numArgs()),
                .code = code,
                .references = [_]Object{object.NotAnObject} ** refs,
                .footer = footer,
            };
        }
        fn cacheOffset(_: *Self, codeOffs: usize, cacheOffs: usize) u32 {
            return @truncate((codes - codeOffs) + refs + (cacheOffs * cacheSize));
        }
        pub inline fn asCompiledMethodPtr(self: *const Self) *CompiledMethod {
            return @as(*CompiledMethod, @ptrCast(@constCast(self)));
        }
        pub fn setLiterals(self: *Self, replacements: []const Object, refReplacements: []const Object) void {
            //trace("\nsetLiterals: 0x{x:0>16} {any}", .{ self.selector.u(), replacements });
            for (&self.code) |*c| {
                if (c.asObject().isIndexSymbol0()) {
                    const index = c.asObject().indexNumber();
                    c.* = Code.objectOf(replacements[index]);
                }
            }
            if (self.signature.isIndexSymbol()) {
                const index = self.signature.indexNumber();
                const replacement = if (index < 0x10000) replacements[index] else refReplacements[index & 0xffff];
                _=replacement;unreachable;
                // self.stackStructure.classIndex = @enumFromInt(@intFromEnum(self.stackStructure.classIndex) - (indexSymbol0(0).numArgs() - replacement.numArgs()));
                // self.signature.selectorHash = replacement.hash32();
            }
            for (refReplacements, &self.references) |obj, *srefs|
                srefs.* = obj;
            if (self.references.len > 0) {
                for (&self.code) |*c| {
                    if (c.asObject().isIndexSymbol1()) {
                        const newValue = (@intFromPtr(&self.references[c.asObject().indexNumber() & (Code.refFlag - 1)]) - @intFromPtr(c)) / @sizeOf(Object) - 1;
                        c.* = Code.uintOf(newValue);
                    }
                }
            }
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
    std.debug.print("Test: CompileTimeMethod\n",.{});
    const expectEqual = std.testing.expectEqual;
    const empty = Object.empty;
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
    var r1 = c1.init(Sym.value, 2, 3, null, .none);
    var r1r = [_]Object{ Nil, True };
    r1.setLiterals(empty, &r1r);
    try expectEqual(r1.getCodeSize(), 10);
}
pub fn compiledMethodType(comptime codeSize: comptime_int) type {
    return CompileTimeMethod(.{ .codes = codeSize });
}
pub fn compileMethod(comptime name: Object, comptime locals: comptime_int, comptime maxStack: comptime_int, comptime class: ClassIndex, comptime tup: anytype) CompileTimeMethod(countNonLabels(tup)) {
    return compileMethodWith(name, locals, maxStack, class, null, tup);
}
pub fn compileMethodWith(comptime name: Object, comptime locals: comptime_int, comptime maxStack: comptime_int, comptime class: ClassIndex, comptime verifier: ?ThreadedFn, comptime tup: anytype) CompileTimeMethod(countNonLabels(tup)) {
    @setEvalBranchQuota(20000);
    const methodType = CompileTimeMethod(countNonLabels(tup));
    var method = methodType.init(name, locals, maxStack, verifier, class);
    const code = method.code[0..];
    comptime var n = 0;
    inline for (tup) |field| {
        switch (@TypeOf(field)) {
            ThreadedFn => {
                code[n] = .{.prim =field};
                n = n + 1;
            },
            Object,bool,@TypeOf(null),comptime_int => {
                code[n] = .{.object = Object.from(field)};
                n = n + 1;
            },
            ClassIndex => {
                code[n] = .{.object = Object.from(@intFromEnum(field))};
                n = n + 1;
            },
            else => {
                comptime var found = false;
                switch (@typeInfo(@TypeOf(field))) {
                    .pointer => |fPointer| {
                        switch (@typeInfo(fPointer.child)) {
                            .array => {
                                if (field[0] == ':') {
                                    found = true;
                                } else if (field.len == 1 and field[0] == '^') {
                                    code[n] = Code.uintOf(n);
                                    n = n + 1;
                                    found = true;
                                } else if (field.len == 1 and field[0] == '*') {
                                    code[n] = Code.intOf(-1);
                                    n = n + 1;
                                    found = true;
                                } else if (field.len >= 1 and field[0] >= '0' and field[0] <= '9') {
                                    code[n] = Code.ref1Of(intOf(field[0..]));
                                    n += 1;
                                    found = true;
                                } else {
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
                                                                code[n] = Code.intOf(lp - n - 1);
                                                                n = n + 1;
                                                                found = true;
                                                                break;
                                                            }
                                                        } else lp = lp + 1;
                                                    },
                                                    // .Fn => |fun| @compileLog(fun),
                                                    else => lp += 1,
                                                }
                                            },
                                            else => {
                                                lp = lp + 1;
                                            },
                                        }
                                    }
                                    if (!found) @compileError("missing label: \"" ++ field ++ "\"");
                                }
                            },
                            .@"fn" => {
                                //@compileLog(field);
                                code[n] = Code.primOf(.{.f = field});
                                n = n + 1;
                                found = true;
                            },
                            else => {},
                        }
                    },
                    else => {},
                }
                if (!found) @compileError("don't know how to handle \"" ++ @typeName(@TypeOf(field)) ++ "\"");
            },
        }
    }
    return method;
}
// const stdout = std.io.getStdOut().writer(); // outside of a functions, stdout causes error on Windows
const print = std.io.getStdOut().writer().print;
const p = @import("controlPrimitives.zig");
test "compiling method" {
    std.debug.print("Test: compiling method\n",.{});
    const expectEqual = std.testing.expectEqual;
    var m = compileMethod(Sym.yourself, 0, 0, .none, .{ ":abc", 0,//&p.setupSend,
                                                       "def", True, comptime Object.from(42), ":def", "abc", "*", "^", 3, "0mref", Sym.i_0, null });
    m.setLiterals(&[_]Object{Sym.value}, &[_]Object{Object.from(42)});
    const t = m.code[0..];
    //    for (t,0..) |tv,idx|
    //        trace("\nt[{}]: 0x{x:0>16}",.{idx,tv.uint});
    //    try expectEqual(t.prim,controlPrimitives.noop);
    try expectEqual(t[0].prim.f, p.send);
    //try expectEqual(t[1].int, 2);
    try expectEqual(t[2].object, True);
    try expectEqual(t[3].object, Object.from(42));
    try expectEqual(t[4].int, -5);
    try expectEqual(t[5].int, -1);
    try expectEqual(t[6].uint, 6);
    try expectEqual(t[7].int, 3);
    //   try expectEqual(t[8].int,1+Code.refFlag);
    try expectEqual(t[9].object, Sym.value);
    try expectEqual(t[10].object, Nil);
    try expectEqual(t.len, 11);
}

pub fn CompileTimeObject(comptime counts: CountSizes) type {
    const codes = counts.codes;
    return struct {
        objects: [codes]Object,
        const Self = @This();
        pub fn init() Self {
            return .{
                .objects = undefined,
            };
        }
        pub fn setLiterals(self: *Self, replacements: []const Object, classes: []const ClassIndex) void {
            var includesPointer = false;
            for (&self.objects) |*o| {
                if (o.isIndexSymbol0()) {
                    const obj = replacements[o.indexNumber()];
                    if (obj.isMemoryAllocated()) includesPointer = true;
                    o.* = obj;
                } else if (o.isIndexSymbol1()) {
                    o.* = Object.from(&self.objects[@as(u16, @truncate(o.indexNumber()))]);
                    includesPointer = true;
                } else { // there is a miniscule chance of false-positive for some floating number here
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
    var obj = objType.init();
    const objects = obj.objects[0..];
    comptime var n = 0;
    comptime var last = -1;
    inline for (tup) |field| {
        switch (@TypeOf(field)) {
            Object, comptime_int => {
                objects[n] = Object.from(field);
                n = n + 1;
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
                comptime var found = false;
                switch (@typeInfo(@TypeOf(field))) {
                    .pointer => |fPointer| {
                        switch (@typeInfo(fPointer.child)) {
                            .array => {
                                if (field[0] == ':') {
                                    found = true;
                                } else if (field.len >= 1 and field[0] >= '0' and field[0] <= '9') {
                                    objects[n] = object.Object.indexSymbol0(comptime intOf(field[0..]));
                                    n += 1;
                                    found = true;
                                } else {
                                    comptime var lp = 0;
                                    inline for (tup) |t| {
                                        switch (@typeInfo(@TypeOf(t))) {
                                            .pointer => |tPointer| {
                                                switch (@typeInfo(tPointer.child)) {
                                                    .array => {
                                                        if (t[0] == ':') {
                                                            if (comptime std.mem.endsWith(u8, t, field)) {
                                                                objects[n] = object.Object.indexSymbol1(lp);
                                                                n = n + 1;
                                                                found = true;
                                                                break;
                                                            }
                                                        } else lp = lp + 1;
                                                    },
                                                    //                                                .Fn => |fun| @compileLog(fun),
                                                    else => lp += 1,
                                                }
                                            },
                                            else => {
                                                lp = lp + 1;
                                            },
                                        }
                                    }
                                    if (!found) @compileError("missing label: \"" ++ field ++ "\"");
                                }
                            },
                            else => {},
                        }
                    },
                    else => {},
                }
                if (!found) @compileError("don't know how to handle \"" ++ @typeName(@TypeOf(field)) ++ "\"");
            },
        }
    }
    if (last >= 0)
        objects[last] = @as(HeapHeader, @bitCast(objects[last])).withLength(n - last - 1).o();
    return obj;
}
test "compileObject" {
    std.debug.print("Test: compileObject\n",.{});
    const expectEqual = std.testing.expectEqual;
    const expect = std.testing.expect;
    const c = ClassIndex;
    var o = compileObject(.{
        ":def",
        c.Class, // first HeapObject
        "second", // pointer to second object
        Sym.i_1, // alternate reference to replacement Object #1
        "1mref", // reference to replacement Object #1
        "third", // pointer to third object
        ":second",
        c.replace0, // second HeapObject - runtime ClassIndex #0
        ":third",
        c.Dispatch, // third HeapObject
        True,
        "def",
    });
    o.setLiterals(&[_]Object{ Nil, True }, &[_]ClassIndex{@enumFromInt(0xdead)});
    try expect(o.asObject().isHeapObject());
    try expect(o.objects[8].equals(o.asObject()));
    //    try expectEqual(@as(u48, @truncate(o.asObject().rawU())), @as(u48, @truncate(@intFromPtr(&o.objects[8]))));
    try expect(o.objects[2].equals(True));
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
pub const controlPrimitivesX = struct {
    const ContextPtr = CodeContextPtr;
    inline fn getMethodSignature(pc: PC, sp: SP, comptime offset: anytype) MethodSignature {
        const selector = pc.object();
        const receiver = sp.at(if (@TypeOf(offset) == @TypeOf(null)) selector.numArgs() else offset);
        const class = receiver.get_class();
        return MethodSignature.from(selector,class);
    }
 fn setupSend(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
        const ms = getMethodSignature(pc, sp, null);
        const returnPc = pc.next().returnOffset();
        context.setReturn(returnPc);
        return @call(tailCall, pc.prim2(), .{ pc.next2(), sp, process, context, ms });
    }
 fn setupTailSend(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, getMethodSignature(pc, sp, null) });
    }
    pub fn setupTailSend0(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, getMethodSignature(pc, sp, 0) });
    }
    pub fn dynamicDispatch(_: PC, sp: SP, process: *Process, context: *Context, signature: MethodSignature) callconv(stdCall) SP {
        const cM = lookupMethod(signature);
        trace("\ndynamicDispatch: {any} {}", .{ cM, signature });
        const pc = cM.codePc();
        return @call(tailCall, cM.executeFn, .{ pc, sp, process, context, signature });
    }

    pub fn verifyMethod(pc: PC, sp: SP, process: *Process, context: *Context, signature: MethodSignature) callconv(stdCall) SP {
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
    // pub fn ifTrue(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
    //     const process = tfAsProcess(_process);
    //     const context = tfAsContext(_context);
    //     trace("\nifTrue: {any}", .{context.stack(sp, process)});
    //     const v = sp.top;
    //     if (True.equals(v)) return @call(tailCall, branch, .{ pc, sp.drop(), process, context, undefined });
    //     if (False.equals(v)) return @call(tailCall, pc.prim2(), .{ pc.next2(), sp.drop(), process, context, undefined });
    //     @panic("non boolean");
    // }
    // pub fn ifFalse(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
    //     const process = tfAsProcess(_process);
    //     const context = tfAsContext(_context);
    //     trace("\nifFalse: {any}", .{context.stack(sp, process)});
    //     const v = sp.top;
    //     if (False.equals(v)) return @call(tailCall, branch, .{ pc, sp.drop(), process, context, undefined });
    //     if (True.equals(v)) return @call(tailCall, pc.next().prim(), .{ pc.skip(2), sp.drop(), process, context, undefined });
    //     @panic("non boolean");
    // }
    // pub fn ifNil(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
    //     const v = sp.top;
    //     if (Nil.equals(v)) return @call(tailCall, branch, .{ pc, sp.drop(), process, context, undefined });
    //     return @call(tailCall, pc.next().prim(), .{ pc.skip(2), sp.drop(), process, context, undefined });
    // }
    // pub fn ifNotNil(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
    //     const v = sp.top;
    //     if (Nil.equals(v)) return @call(tailCall, pc.next().prim(), .{ pc.skip(2), sp.drop(), process, context, undefined });
    //     return @call(tailCall, branch, .{ pc, sp.drop(), process, context, undefined });
    // }
    pub fn primFailure(_: PC, _: SP, _: *Process, _: *Context, _: MethodSignature) callconv(stdCall) SP {
        @panic("primFailure");
    }
    pub fn replaceLiteral(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
        sp.top = pc.object();
        trace("\nreplaceLiteral: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.prim2(), .{ pc.next2(), sp, process, context, undefined });
    }
    pub fn replaceLiteral0(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
        sp.top = Object.from(0);
        trace("\nreplaceLiteral0: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined });
    }
    pub fn replaceLiteral1(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
        sp.top = Object.from(1);
        trace("\nreplaceLiteral0: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined });
    }
    pub fn pushLiteral0(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
        const newSp = sp.push(Object.from(0));
        trace("\npushLiteral0: {any}", .{context.stack(newSp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteral1(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
        const newSp = sp.push(Object.from(1));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteral2(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
        const newSp = sp.push(Object.from(2));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteral_1(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
        const newSp = sp.push(Object.from(-1));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteralIndirect(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
        const newSp = sp.push(pc.literalIndirect());
        return @call(tailCall, pc.prim2(), .{ pc.next2(), newSp, process, context, undefined });
    }
    pub fn pushLiteralNil(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
        const newSp = sp.push(Nil);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteralTrue(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
        const newSp = sp.push(True);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteralFalse(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
        const newSp = sp.push(False);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn printStack(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
        trace("\nstack: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined });
    }
    pub fn fallback(pc: PC, sp: SP, process: *Process, context: *Context, signature: MethodSignature) callconv(stdCall) SP {
        const self = sp.at(signature.numArgs());
        context.setReturn(pc);
        const class = self.get_class();
        const cM = lookupMethod(signature);
        trace("\nfallback: {} {} {} {}", .{ signature, class, pc, cM });
        return @call(tailCall, cM.executeFn, .{ cM.codePc(), sp, process, context, undefined });
    }
    pub fn callRecursive(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
        context.setReturn(pc.next());
        const offset = pc.int();
        const newPc = pc.next().back(@intCast(-offset));
        trace("\ncallRecursive: {any}", .{context.stack(sp, process)});
        return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, undefined });
    }
    // pub fn send1(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature, prevCache: SendCache) callconv(stdCall) SP {
    //     const self = sp.next;
    //     context.setReturn(pc.next().skip(sendCacheSize));
    //     const class = self.get_class();
    //     const selector = pc.object().withClass(class);
    //     trace("\nsend1: {} {}", .{ selector, class });
    //     const cache = if (dispatchCache) @as(SendCache, @constCast(@ptrCast(pc + 1))) else prevCache;
    //     const newPc = if (dispatchCache) cache.current() else lookupAddress(selector, class);
    //     trace(" {} {any}", .{ newPc, process.getStack(sp) });
    //     return @call(tailCall, newPc.*.prim(), .{ newPc.*.next(), sp, process, context, signature });
    // }
    //    pub fn tailSend1(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
    //    }
    // pub fn perform(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
    //     const context = tfAsContext(_context);
    //     const selector = sp.top;
    //     const numArgs = selector.numArgs();
    //     if (numArgs != 0) @panic("wrong number of args");
    //     const newPc = lookupAddress(selector);//, sp.next.get_class());
    //     context.setReturn(pc);
    //     return @call(tailCall, newPc.prim(), .{ newPc.next(), sp + 1, process, context, undefined });
    // }
    // pub fn performWith(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
    //     const context = tfAsContext(_context);
    //     const selector = sp.next;
    //     sp.next = sp.top;
    //     if (selector.numArgs() != 1) @panic("wrong number of args");
    //     const newPc = lookupAddress(selector);//, sp.third.get_class());
    //     context.setTPc(pc + 1);
    //     return @call(tailCall, newPc.prim(), .{ newPc.next(), sp + 1, process, context, undefined });
    // }
    pub fn returnNoContextSwitchToThreaded(_: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
        trace("\nreturnNoContext: {any} N={} T={}", .{ context.stack(sp, process), context.getNPc(), context.getTPc() });
        const tPc = context.getTPc();
        const nPc = tPc.prev().prim();
        return @call(tailCall, nPc, .{ tPc, sp, process, context, undefined });
    }
    pub fn isCallerInThreadedMode(pc: PC, sp: SP, process: *Process, context: *Context, _: MethodSignature) callconv(stdCall) SP {
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
    pub fn init(self: *Self) void {
        self.process.init();
        self.ctxt.method = &yourself;
        self.sp = self.process.endOfStack();
    }
    var yourself = CompiledMethod.init(Sym.noFallback, .{.f = Code.end});
    pub fn initStack(self: *Self, source: []const Object) void {
        self.sp = self.process.endOfStack().reserve(source.len);
        self.process.setSp(self.sp);
        for (source, self.sp.slice(source.len)) |src, *dst|
            dst.* = src;
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
    fn mainSendTo(selector: Object, target: Object) !Object {
        std.debug.print("Sending: {} to {}\n",.{selector,target});
        // var exec = Self.new();
        // exec.initStack(.{target});
        // exec.ctxt.setReturn(Code.endThread);
        return error.NotImplemented;
    }
};

pub fn mainSendTo(selector: Object, target: Object) !Object {
    std.debug.print("Sending: {} to {}\n",.{selector,target});
//    _ = Execution.new();
     return error.NotImplemented;
}


const max_classes = config.max_classes;
const symbols = symbol.symbols;
const smallestPrimeAtLeast = @import("utilities.zig").smallestPrimeAtLeast;
// // note that self and other could become invalid after any method call if they are heap objects, so will need to be re-loaded from context.fields if needed thereafter

pub const forTest = Dispatch.forTest;
const noArgs = ([0]Object{})[0..];
pub const lookupMethod = Dispatch.lookupMethodForClass;
pub const dump = Dispatch.dump;
pub fn init() void {
    //    _ = Dispatch.new();
}
pub fn loadIntrinsicsDispatch() void {
}
pub const addMethod = Dispatch.addMethod;
const DispatchElement = struct {
    signature: MethodSignature,
    methodPointer: ?*const CompiledMethod,
    const Self = @This();
    const IntSelf = u128;
    comptime {
        std.debug.assert(@sizeOf(Self)==@sizeOf(IntSelf));
    }
    fn initUpdateable(self: *Self) void {
        self.signature = MethodSignature.nil;
        self.methodPointer = null;
    }
    fn new(compiledMethod: *const CompiledMethod) Self {
        return .{ .signature = compiledMethod.signature, .methodPointer = compiledMethod };
    }
    fn empty() Self {
        return .{ .signature = MethodSignature.nil, .methodPointer = null };
    }
    inline fn cas(self: *Self, replacement: *const CompiledMethod) ?Self {
        const current = self.asInt();
        const replace = new(replacement).asInt();
        if (@cmpxchgWeak(IntSelf, self.asIntPtr(), current, replace, .seq_cst, .seq_cst)) |notClean|
            return @bitCast(notClean);
        return null;
    }
    inline fn match(self: Self, signature: MethodSignature) ?*const CompiledMethod {
        return if(self.signature.equals(signature)) self.methodPointer else null;
    }
    inline fn isNil(self: Self) bool {
        return self.signature.isNil();
    }
    inline fn asInt(self: Self) IntSelf {
        return @bitCast(self);
    }
    inline fn asIntPtr(self: *Self) *IntSelf {
        return @alignCast(@ptrCast(self));
    }
    inline fn store(self: *Self, replacement: *const CompiledMethod) void {
        self.signature = replacement.signature;
        self.methodPointer = replacement;
    }
    inline fn pc(self: *const Self) PC {
        return PC.init(self.methodPointer.?.codePtr());
    }
    inline fn next(self: *Self) *Self {
        return @ptrCast(@as([*]Self, @ptrCast(self)) + 1);
    }
    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = .{ fmt, options };
        try writer.print("DispatchElement(ThreadedFn@{x},CompiledMethod@{x})", .{ @intFromPtr(self.primitive), @intFromPtr(self.methodPointer) });
    }
};
const Dispatch = struct {
    header: HeapHeader,
    nMethods: u64,
    state: DispatchState,
    methodStart: DispatchElement, // this is just the empty size... normally a larger array
    comptime {
        // @compileLog(@sizeOf(Self));
        // std.debug.assert(@as(usize, 1) << @ctz(@as(u62, @sizeOf(Self))) == @sizeOf(Self));
        std.debug.assert(@offsetOf(Self,"header")==0);
        std.debug.assert(numberOfFixed == 0);
//        std.debug.assert(@offsetOf(Self, "methods") & 0xf == 0);
    }
    const Self = @This();
    const Fixed = enum {
        // value,
        // valueColon,
        // cullColon,
        // equal,
        // hash,
    };
    const fixedSelectors = [_]Object{ symbols.value, symbols.@"value:", symbols.@"cull:", symbols.@"=", symbols.hash, };
    const numberOfFixed: usize = @typeInfo(Fixed).@"enum".fields.len;
    const loadFactor = 70; // hashing load factor
    const DispatchState = enum(u64) { clean, beingUpdated, dead };
    var empty = Self{
        .header = HeapHeader.staticHeaderWithClassLengthHash(ClassIndex.Dispatch, @sizeOf(Self) / 8 - 1, 0), // don't count header, but do count one element of methods
        .nMethods = 0,
        .state = .clean,
        .methodStart = DispatchElement.empty(),
    };
    var dispatches = [_]*Self{&empty} ** max_classes;
    const dnu = if (@import("builtin").is_test) &testDnu else &forceDnu;
    const grow = if (@import("builtin").is_test) &testGrow else &growDispatch;
    fn methods(self: *const Self) [*]const DispatchElement {
        return @as([*]const DispatchElement,@ptrCast(@alignCast(&self.methodStart)));
    }
    fn isUpdateable(self: *const Self, methodPtr: *const DispatchElement) bool {
        return @intFromPtr(methodPtr)<@intFromPtr(self.methods()+self.nMethods-1) and methodPtr.isNil();
    }
    fn methodSlice(self: *const Self) []const DispatchElement {
        return self.methods()[0..self.nMethods];
    }
    pub fn forceDnu(pc: PC, sp: SP, process: *Process, context: *Context, signature: MethodSignature) callconv(stdCall) SP { // not embedded
        std.debug.print("\nforceDnu: 0x{x} {} {}", .{ signature.selectorHash, signature.classIndex, signature.asSymbol() });
        _ = .{ pc, sp, process, context, signature, @panic("forceDnu unimplemented") };
    }
    pub fn growDispatch(pc: PC, sp: SP, process: *Process, context: *Context, signature: MethodSignature) callconv(stdCall) SP { // not embedded
        std.debug.print("\ngrowDispatch: 0x{x} {} {}", .{ signature.selectorHash, signature.class, signature.asSymbol() });
        _ = .{ pc, sp, process, context, signature, @panic("growDispatch unimplemented") };
    }
    fn dump(index: ClassIndex) void {
        trace("\ndump: {} {}", .{ index, dispatches[@intFromEnum(index)] });
    }
    pub fn addMethod(method: *CompiledMethod) void {
        const index = method.signature.getClassIndex();
//        trace("\naddMethod: {} {} {}", .{ index, method.selector(), method.codePtr() });
        if (dispatches[index].add(method)) return;
        var numMethods: usize = 3;
        while (true) {
            const dispatch = dispatches[index];
            if (dispatch.lock()) |_| {
                numMethods = @max(numMethods, dispatch.nMethods + 1) * 100 / loadFactor;
                const newDispatch = alloc(numMethods);
                if (dispatch.addMethodsTo(newDispatch, method)) {
                    dispatches[index] = newDispatch;
                    dispatch.state = .dead;
                    return;
                }
            } else |_| {}
        }
    }
    fn addMethodsTo(self: *Self, newDispatch: *Self, method: *CompiledMethod) bool {
        for (self.methodSlice()) |de| {
            if (de.methodPointer) |ptr|
                if (!newDispatch.add(ptr)) return false;
        }
        return newDispatch.add(method);
    }
    inline fn alloc(words: usize) *Self {
        const hash = smallestPrimeAtLeast(words);
        const nMethods = hash;
        const nInstVars = (nMethods * @sizeOf(DispatchElement) + @offsetOf(Self, "methodStart")) / @sizeOf(Object) - 1;
        trace("\ninstVars: {}", .{nInstVars});
        const aR = globalArena.aHeapAllocator().alloc(.CompiledMethod, @intCast(nInstVars), null, Object, false);
        const self: *Self = @alignCast(@ptrCast(aR.allocated));
        self.nMethods = hash;
        for (@constCast(self.methodSlice())) |*ptr|
            ptr.initUpdateable();
        self.state = .clean;
        return self;
    }
    inline fn lookupAddress(self: *const Self, selector: MethodSignature) *DispatchElement {
        const index = getIndex(selector, self.nMethods);
        //@setRuntimeSafety(false);
        return @constCast(&self.methods()[index]);
    }
    inline fn lookupMethod(self: *const Self, selector: MethodSignature) *const CompiledMethod {
        const index = getIndex(selector, self.nMethods);
        var des: [*]const DispatchElement = self.methods()+index;
        while (true) {
            if (des[0].match(selector)) |cM|
                return cM;
            if (des[0].isNil()) unreachable;
        }
    }
    inline fn getIndex(selector: MethodSignature, size: u64) u64 {
        return selector.hash() * size >> 32;
    }
    pub inline fn lookupMethodForClass(signature: MethodSignature) *const CompiledMethod {
        trace(" (lookupMethodForClass) {}", .{signature.getClass()});
        const cM = dispatches[@intFromEnum(signature.getClass())].lookupMethod(signature);
        return cM;
    }
    fn lock(self: *Self) !void {
        while (true) {
            if (@cmpxchgWeak(DispatchState, &self.state, .clean, .beingUpdated, .seq_cst, .seq_cst)) |notClean| {
                if (notClean == .dead) return error.DeadDispatch;
            } else break;
            trace("\nlock: looping", .{});
        }
    }
    fn add(self: *Self, cmp: *const CompiledMethod) bool {
        trace("\nadd: {}", .{cmp.signature});
        self.lock() catch {
            return false;
        };
        defer {
            self.state = .clean;
        }
        var address = self.lookupAddress(cmp.signature);
        while (address.methodPointer) |existing| : (address = address.next()) {
            if (existing.signature.equals(cmp.signature)) {
                address.store(cmp); // replace this
                trace(" - replaced existing", .{});
                return true;
            }
        } else {
            if (self.isUpdateable(address)) {
                address.store(cmp);
                trace(" - installed", .{});
                return true;
            }
        }
        trace(" - no free space", .{});
        return false;
    }
    fn fail(programCounter: PC, sp: SP, process: *Process, context: *Context, signature: MethodSignature) callconv(stdCall) SP {
        _ = .{ programCounter, sp, process, context, signature };
        if (programCounter.uint() == 0)
            @panic("called fail function");
        @panic("fail with non-zero next");
    }
    fn testDnu(programCounter: PC, sp: SP, process: *Process, context: *Context, signature: MethodSignature) callconv(stdCall) SP {
        _ = .{ programCounter, sp, process, context, signature, @panic("testDnu") };
        //        return sp.push(object.NotAnObject);
    }
    fn testGrow(programCounter: PC, sp: SP, process: *Process, context: *Context, signature: MethodSignature) callconv(stdCall) SP {
        _ = .{ programCounter, sp, process, context, signature, @panic("testGrow") };
        //        return sp.push(object.NotAnObject);
    }
    fn testIncrement(programCounter: PC, sp: SP, process: *Process, context: *Context, signature: MethodSignature) callconv(stdCall) SP {
        _ = .{ process, context, signature };
        @as(*usize, @ptrFromInt(programCounter.uint())).* += 1;
        return sp;
    }
};
pub fn PolymorphicInlineCache(comptime picSize: usize) type {
    return struct {
        de0: if (picSize>0) DispatchElement else void,
        de1: if (picSize>1) DispatchElement else void,
        de2: if (picSize>2) DispatchElement else void,
        de3: if (picSize>3) DispatchElement else void,
        fn ev0(self: *Self) ?*const CompiledMethod {
            if (@TypeOf(self.de0)==void) return null;
            

        }
        const Self = @This();
    };
}
test "PIC" {
    try std.testing.expectEqual(16,@sizeOf(PolymorphicInlineCache(1)));
    try std.testing.expectEqual(0,@sizeOf(PolymorphicInlineCache(0)));
}

//fn initTest(self: *Self, target: *usize) void {
//    self.initPrivate(.{Code.prim(&testIncrement),Code.uint(@intFromPtr(target))});
//}
//pub fn forTest() void {
//    var foo = Self.new();
//    foo.initDNU();
//u}
fn tOffset(hash: u32, ptr: *const DispatchElement, comptime bit: comptime_int) *const DispatchElement {
    return Dispatch.offset(hash, PC.init(@ptrCast(ptr)), bit);
}
fn eo(lDe: *const DispatchElement, rDe: *const DispatchElement) !void {
    try std.testing.expectEqual(@intFromPtr(lDe), @intFromPtr(rDe));
}
// test "disambiguate" {
//     const ee = std.testing.expectEqual;
//     Process.resetForTest();
//     const empty = Object.empty;
//     const fns = struct {
//         fn push1(_: PC, sp: SP, _: *Process, _: *Context, _: MethodSignature) callconv(stdCall) SP {
//             return sp.push(Object.from(1));
//         }
//         fn push2(_: PC, sp: SP, _: *Process, _: *Context, _: MethodSignature) callconv(stdCall) SP {
//             return sp.push(Object.from(2));
//         }
//         fn push3(_: PC, sp: SP, _: *Process, _: *Context, _: MethodSignature) callconv(stdCall) SP {
//             return sp.push(Object.from(3));
//         }
//     };
//     // value=01101 yourself=00001 @"<="=11101
//     var method1 = compileMethod(symbols.value, 0, 0, .{ &fns.push1, &Code.end });
//     method1.setLiterals(empty, empty);
//     var method2 = compileMethod(symbols.yourself, 0, 0, .{ &fns.push2, &Code.end });
//     method2.setLiterals(empty, empty);
//     var method3 = compileMethod(symbols.@"<=", 0, 0, .{ &fns.push3, &Code.end });
//     method3.setLiterals(empty, empty);
//     var space2 = [_]DispatchElement{undefined}**2;
//     var dispatcher = Dispatch.disambiguate2(&space2, @ptrCast(&method1), @ptrCast(&method2));
//     const push1Code = DispatchElement.init(&method1.code[0]);
//     const push2Code = DispatchElement.init(&method2.code[0]);
//     try ee(space2[0], push1Code);
//     try ee(space2[1], push2Code);
//     dispatcher = Dispatch.disambiguate2(&space2, @ptrCast(&method2), @ptrCast(&method1));
//     try ee(space2[0], push1Code);
//     try ee(space2[1], push2Code);
//     var process = Process.new();
//     process.init();
//     defer process.deinit();
//     var context = Context.init();
//     const sp = process.endOfStack();
//     if (config.dispatchCache) {
//         try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.value).top.to(i64), 1);
//         try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.yourself).top.to(i64), 2);
//         try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.@"<=").top.to(i64), 3);
//         try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.value).top.to(i64), 1);
//     }
//     try ee(dispatcher.prim(), &Dispatch.bitTest2);
//         dispatcher = Dispatch.disambiguate2(&space2, @ptrCast(&method3), @ptrCast(&method1));
//     try ee(dispatcher.prim(), &Dispatch.bitTest4);
// }
fn doDispatch(tE: *Execution, dispatch: *Dispatch, signature: MethodSignature) []Object {
    tE.initStack(&[_]Object{Object.from(0)});
    return tE.stack(dispatch.dispatch(tE.sp, &tE.process, &tE.ctxt, signature));
}
// test "add methods" {
//     const empty = Object.empty;
//     Process.resetForTest();
//     const ee = std.testing.expectEqual;
//     var temp0: usize = 0;
//     var temp: usize = 0;
//     const methodType = compiledMethodType(2);
//     const fns = struct {
//         fn testYourself(_: PC, sp: SP, _: *Process, _: CodeContextPtr, signature: MethodSignature) callconv(stdCall) SP {
//             if (!selector.equals(symbols.yourself)) @panic("hash doesn't match");
//             sp.top = Object.cast(sp.top.u() + 2);
//             return sp;
//         }
//         fn testAt(_: PC, sp: SP, _: *Process, _: CodeContextPtr, signature: MethodSignature) callconv(stdCall) SP {
//             if (!selector.equals(symbols.@"at:")) @panic("hash doesn't match");
//             sp.top = Object.cast(sp.top.u() + 4);
//             return sp;
//         }
//     };
//     var code0 = methodType.withCode(symbols.yourself, 0, 0, .{ Code.prim(&fns.testYourself), Code.uint(@intFromPtr(&temp0)) });
//     code0.setLiterals(empty, empty, null);
//     var code1 = methodType.withCode(symbols.yourself, 0, 0, .{ Code.prim(&fns.testYourself), Code.uint(@intFromPtr(&temp)) });
//     code1.setLiterals(empty, empty, null);
//     var code2 = methodType.withCode(symbols.@"at:", 0, 0, .{ Code.prim(&fns.testAt), Code.uint(@intFromPtr(&temp)) });
//     code2.setLiterals(empty, empty, null);
//     var tE = Execution.new();
//     tE.init();
//     var dispatch = Dispatch.new();
//     dispatch.init();
//     try dispatch.add(@ptrCast(&code0));
//     try dispatch.add(@ptrCast(&code1));
//     try ee(doDispatch(&tE, &dispatch, symbols.yourself)[0], Object.from(2));
//     try ee(doDispatch(&tE, &dispatch, symbols.self)[0], object.NotAnObject);
//     try dispatch.add(@ptrCast(&code2));
//     try ee(doDispatch(&tE, &dispatch, symbols.yourself)[0], Object.from(2));
//     try ee(doDispatch(&tE, &dispatch, symbols.@"at:")[0], Object.from(4));
//     try std.testing.expectEqual(dispatch.add(@ptrCast(&code2)), error.Conflict);
// }
inline fn bumpSize(size: u16) u16 {
    return size * 2;
}
inline fn initialSize(size: usize) u16 {
    return @import("utilities.zig").largerPowerOf2(@max(@as(u16, @intCast(size)), 4));
}
extern fn llvmPL(_: *anyopaque, _: *anyopaque, _: *anyopaque, _: *anyopaque, c_int) *anyopaque;
const llvmPLCM = CompiledMethod.init(Sym.yourself, @ptrCast(&llvmPL));
test "llvm external" {
    const expectEqual = std.testing.expectEqual;
    Process.resetForTest();
    const pl = if (true) &p.pushLiteral else llvmPLCM;
    var method = compileMethod(Sym.yourself, 1, 0, .none, .{
        &p.pushContext,           "^",
        ":label1",                &p.pushLiteral,
        comptime Object.from(42), &p.popLocal,
        0,                        &p.pushLocal, 0,
        pl, 0,                     &p.pushLiteral, true,
        &p.classCase, ClassIndex.False, "label3",
        &p.branch,                "label2",
        ":label3",                &p.pushLocal,
        0,                        ":label4",
        &p.returnTop,             ":label2",
        pl, 0,                     &p.branch,
        "label4",
    });
    var objs = [_]Object{Nil};
    var te = Execution.new();
    te.init();
    const result = te.run(objs[0..], &method);
    try expectEqual(result.len, 1);
    try expectEqual(result[0], Object.from(0));
}
test "simple llvm" {
    const expectEqual = std.testing.expectEqual;
    Process.resetForTest();
    const pl = if (true) &p.pushLiteral else llvmPLCM;
    var method = compileMethod(Sym.yourself, 0, 0, .none, .{
        pl,0,
        &p.returnNoContext,
    });
    var te = Execution.new();
    te.init();
    var objs = [_]Object{ Nil, True };
    const result = te.run(objs[0..], &method);
    try expectEqual(result.len, 3);
    try expectEqual(result[0], Object.from(0));
    try expectEqual(result[1], Nil);
    try expectEqual(result[2], True);
}
