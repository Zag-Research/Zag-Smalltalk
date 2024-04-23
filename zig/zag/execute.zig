const std = @import("std");
const config = @import("config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const checkEqual = @import("utilities.zig").checkEqual;
const Process = @import("process.zig").Process;
const ProcessPtr = @import("process.zig").ProcessPtr;
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
pub const Context = @import("context.zig").Context;
const heap = @import("heap.zig");
const HeapHeader = heap.HeapHeader;
const HeapObject = heap.HeapObject;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
const Format = heap.Format;
const Age = heap.Age;
//const class = @import("class.zig");
const symbol = @import("symbol.zig");
const Sym = symbol.symbols;
const phi32 = @import("utilities.zig").inversePhi(u32);

pub const SP = *Stack;
const Stack = extern struct {
    top: Object,
    next: Object,
    third: Object,
    fourth: Object,
    pub inline fn lessThan(self: SP, other: SP) bool {
        return @intFromPtr(self) < @intFromPtr(other);
    }
    pub inline fn push(self: SP, v: Object) SP {
        const newSp = self.reserve(1);
        newSp.top = v;
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
pub fn check(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
    if (process.debugger()) |debugger|
        return @call(tailCall, debugger, .{ pc, sp, process, context, undefined });
    return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined });
}

pub const TFProcess = if (true) *anyopaque else *Process;
pub const TFContext = if (true) *anyopaque else *Process;
pub const ThreadedFn = *const fn (
    programCounter: PC,
    stackPointer: SP,
    process: TFProcess,
    context: TFContext,
    signature: MethodSignature
) callconv(stdCall) SP;
pub const fallback = controlPrimitives.fallback;
const fallbackCode = Code.prim(&fallback);
const fallbackPc = PC.init(&fallbackCode);
pub const MethodSignature = extern struct {
    selectorHash: u32,
    class: ClassIndex,
    fn isIndexSymbol(self: MethodSignature) bool {
        return self.selectorHash & 0xff == 0xff;
    }
    fn indexNumber(self: MethodSignature) usize {
        return self.selectorHash >> 8;
    }
};
pub const CodeContextPtr = *Context;
pub const CompiledMethodPtr = *CompiledMethod;
pub const CompiledMethod = extern struct {
    header: HeapHeader,
    stackStructure: Object, // number of local values beyond the parameters
    signature: MethodSignature,
    verifier: ThreadedFn,
    code: [codeSize]Code, // will typically be a lot more then 3, as it will be the processed version of the method
    //references: [n]Object,
    const Self = @This();
    const codeSize = 1;
    pub const codeOffset = @offsetOf(CompiledMethod, "code");
    const codeOffsetInUnits = codeOffset / 8;
    pub fn init(name: Object, methodFn: ThreadedFn) Self {
        return Self{
            .header = HeapHeader.calc(ClassIndex.CompiledMethod, codeOffsetInUnits + codeSize, name.hash24(), Age.static, null, Object, false) catch unreachable,
            .stackStructure = Object.from(0),
            .signature = .{ .selectorHash = name.hash32(), .class = .none},
            .verifier = methodFn,
            .code = Code.endCode,
        };
    }
    pub fn execute(self: *Self, sp: SP, process: TFProcess, context: TFContext) callconv(stdCall) SP {
        const pc = PC.init(self.codePtr());
        trace("\nexecuteX: {} {*} {*}", .{ pc, self, self.codePtr() });
        trace("\nexecuteY: {} {x} {} {}", .{ self.stackStructure, self.code[2], self.code[0], self.code[1] });
        trace("\nexecute: [{*}]: {*} {} {}", .{ pc.asCodePtr(), pc.prim(), sp.top, self.selector() });
        //        return @call(tailCall,pc.prim(),.{pc+1,sp,process,context,self.selector});
        return pc.prim()(pc.next(), sp, process, context, self.selector());
    }
    pub fn forDispatch(self: *Self, class: ClassIndex) void {
        trace("\nforDispatch: {x} {}",.{self.code[1],class});
        addMethod(class, self) catch @panic("addMethod failed");
    }
    inline fn asHeapObjectPtr(self: *const Self) HeapObjectConstPtr {
        return @as(HeapObjectConstPtr, @ptrCast(self));
    }
    pub fn checkFooter(self: *Self) void {
        trace("\ncheckFooter*: {}\n    header={}\n    footer={}\n  *footer={}\n   a1={x}\n   a2={x}\n   a3={x}", .{ self.selector, self.header, self.footer, self.header.realHeapObject(), @intFromPtr(self), @intFromPtr(self.header.realHeapObject()), @intFromPtr(&self.footer) });
    }
    pub inline fn matchedSelector(self: *Self, selectorO: Object) bool {
        return selectorO.hashEquals(self.selector());
    }
    pub inline fn codePtr(self: *const Self) * const Code {
        return &self.code[0];
    }
    pub inline fn selectorHash32(self: *const Self) u32 {
        return self.code[2].classSelector.selectorHash32;
    }
    pub inline fn selector(self: *const Self) Object {
        return symbol.fromHash32(self.selectorHash32());
    }
    pub fn format(
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
        try writer.print("\nCMethod: {} locals:{} maxStack:{} selfOffset:{} realHO:{} {any} ({any})", .{ self.selector, locals, maxStackNeeded, selfOffset, realHO, all[codeOffsetInUnits .. all.len - refs.len], refs });
    }
    pub fn asFakeObject(self: *const Self) Object {
        return @as(Object, @bitCast(@intFromPtr(self)));
    }
};
pub const PC = extern struct {
    code: * const Code,
    pub const baseType = Code;
    const Self = @This();
    pub inline fn init(code: *const Code) PC {
        return .{.code=code};
    }
    pub inline fn prim(self: PC) ThreadedFn {
        return self.code.prim;
    }
    pub inline fn next(self: PC) PC {
        return @bitCast(@intFromPtr(@as([*]const Code, @ptrCast(self.code)) + 1));
    }
    pub inline fn next2(self: PC) PC {
        return @bitCast(@intFromPtr(@as([*]const Code, @ptrCast(self.code)) + 2));
    }
    pub inline fn skip(self: PC, n: usize) PC {
        return @bitCast(@intFromPtr(@as([*]const Code, @ptrCast(self.code)) + n));
    }
    pub inline fn prim2(self: PC) ThreadedFn {
        return @as([*]const Code, @ptrCast(self.code))[1].prim;
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
    pub inline fn compiledMethodPtr(self: PC, comptime index: comptime_int) *const CompiledMethod {
        return @fieldParentPtr("code", @as(*const [3]Code, @ptrCast(@as([*]const Code, @ptrFromInt(@as(usize,@bitCast(self)))) - index)));
    }
    pub fn XXXchoose(self: PC, v: u32) PC {
        if (v == 0) return self.codeRef;
        return @as([*]const Code, @ptrCast(self))[1].codeRef;
    }
};
pub const Code = extern union {
    prim: ThreadedFn,
    int: i64,
    uint: u64,
    object: Object,
    header: heap.HeapObject,
    classSelector: MethodSignature,
    const refFlag = 1024;
    pub inline fn classSelector(class: ClassIndex, selector: Object) Code {
        return Code{ .classSelector = .{.selectorHash = selector.hash32(), .class = class}};
    }
    pub inline fn prim(pp: ThreadedFn) Code {
        return Code{ .prim = pp };
    }
    inline fn int(i: i64) Code {
        return Code{ .int = i };
    }
    pub inline fn uint(u: u64) Code {
        return Code{ .uint = u };
    }
    pub inline fn object(o: Object) Code {
        return Code{ .object = o };
    }
    inline fn ref1(comptime u: u12) Code {
        return Code{ .object = indexSymbol1(u) };
    }
    inline fn header(h: heap.HeapObject) Code {
        return Code{ .header = h };
    }
    pub inline fn codeRef(c: [*]const Code) Code {
        return Code{ .uint = @intFromPtr(@constCast(c)) };
    }
    pub fn end(_: PC, sp: SP, _: TFProcess, _: TFContext, _: MethodSignature) callconv(stdCall) SP {
        return sp;
    }
    var endCode = [CompiledMethod.codeSize]Code{.{ .prim = &end }};
    pub const endThread = PC.init(@ptrCast(&endCode));
    inline fn compiledMethodX(self: PC) *const CompiledMethod {
        return @as(*const CompiledMethod, @ptrCast(self));
    }
    pub fn format(
        self: *const Code,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        if (!self.object.isDouble()) {
            try writer.print("{}", .{self.object});
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
                .Pointer => |pointer| {
                    switch (@typeInfo(pointer.child)) {
                        .Array => switch (field[0]) {
                            ':' => {},
                            '0'...'9' => {
                                r = comptime @max(r, intOf(field[0..]) + 1);
                                c += 1;
                            },
                            else => c += 1,
                        },
                        .Fn => {
                            c += 1;
                        },
                        else => {
                            @compileLog(field,ThreadedFn);
                            unreachable;
                        }
                }},
                else => {
                    c += 1;
                },
            },
        }
    }
    return .{ .codes = c, .refs = r, .objects = o };
}
test "countNonLabels" {
    const expectEqual = std.testing.expectEqual;
    const r1 = countNonLabels(.{
        ":abc",
        &p.send,
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

pub fn CompileTimeMethod(comptime counts: CountSizes) type {
    const codes = counts.codes;
    const refs = counts.refs;
    return extern struct { // structure must exactly match CompiledMethod
        header: HeapHeader,
        stackStructure: Object,
        signature: MethodSignature,
        verifier: ThreadedFn,
        code: [codes]Code,
        references: [refs]Object,
        const codeOffsetInUnits = CompiledMethod.codeOffsetInUnits;
        const Self = @This();
        // comptime {
        //     if (checkEqual(CompiledMethod.codeOffset,@offsetOf(Self,"code"))) |s|
        //         @compileError("CompiledMethod prefix not the same as CompileTimeMethod == " ++ s);
        // }
        const cacheSize = 0; //@sizeOf(SendCacheStruct) / @sizeOf(Code);
        pub fn init(comptime name: Object, comptime locals: u16, comptime maxStack: u16, verifier: ThreadedFn) Self {
            const header = HeapHeader.calc(.CompiledMethod, codeOffsetInUnits + codes + refs, name.hash24(), Age.static, null, Object, false) catch @compileError("too many refs");
            //  @compileLog(codes,refs,footer,heap.Format.allocationInfo(5,null,0,false));
            return .{
                .header = header,
                .signature = .{ .selectorHash=name.hash32(), .class=.none},
                .verifier = verifier,
                .stackStructure = Object.packedInt(locals, maxStack, locals + name.numArgs()),
                .code = undefined,
                .references = [_]Object{object.NotAnObject} ** refs,
            };
        }
        pub fn checkFooter(self: *Self) void {
            trace("\ncheckFooter: {}\n    header={}\n    footer={}\n     allocInfo={}\n   a1={x}\n   a2={x}", .{ self.selector, self.header, self.footer, Format.allocationInfo(codeOffsetInUnits + codes, refs, @sizeOf(Object), false), @intFromPtr(self), @intFromPtr(self.header.realHeapObject()) });
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
        pub fn asCompiledMethodPtr(self: *const Self) *CompiledMethod {
            return @as(*CompiledMethod, @ptrCast(@constCast(self)));
        }
        pub fn setLiterals(self: *Self, replacements: []const Object, refReplacements: []const Object) void {
            //trace("\nsetLiterals: 0x{x:0>16} {any}", .{ self.selector.u(), replacements });
            for (&self.code) |*c| {
                if (c.object.isIndexSymbol0()) {
                    const index = c.object.indexNumber();
                    c.* = Code.object(replacements[index]);
                }
                // if (dispatchCache and cachedSend) {
                //     @as(SendCache, @ptrCast(@as([*]Code, @ptrCast(c)) + 1)).* = if (cache) |aSendCache| aSendCache.* else SendCacheStruct.init();
                //     cachedSend = false;
                // }
            }
            if (self.signature.isIndexSymbol()) {
                const replacement = replacements[self.signature.indexNumber()];
                self.stackStructure.classIndex = @enumFromInt(@intFromEnum(self.stackStructure.classIndex) - (indexSymbol0(0).numArgs() - replacement.numArgs()));
                self.signature.selectorHash = replacement.hash32();
            }
            for (refReplacements, &self.references) |obj, *srefs|
                srefs.* = obj;
            if (self.references.len > 0) {
                for (&self.code) |*c| {
                    if (c.object.isIndexSymbol1()) {
                        const newValue = (@intFromPtr(&self.references[c.object.indexNumber() & (Code.refFlag - 1)]) - @intFromPtr(c)) / @sizeOf(Object) - 1;
                        c.* = Code.uint(newValue);
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
    const expectEqual = std.testing.expectEqual;
    const empty = Object.empty;
    const c1 = CompileTimeMethod(countNonLabels(.{
        ":abc",
        &p.hardDnu,
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
        comptime Code.classSelector(.SmallInteger,Nil),
    }));
    var r1 = c1.init(Nil, 2, 3, &controlPrimitives.verifyMethod);
    var r1r = [_]Object{ Nil, True };
    r1.setLiterals(empty, &r1r);
    try expectEqual(r1.getCodeSize(), 11);
}
pub fn compiledMethodType(comptime codeSize: comptime_int) type {
    return CompileTimeMethod(.{ .codes = codeSize });
}
pub fn compileMethod(comptime name: Object, comptime locals: comptime_int, comptime maxStack: comptime_int, comptime tup: anytype) CompileTimeMethod(countNonLabels(tup)) {
    return compileMethodWith(name, locals, maxStack, &controlPrimitives.verifyMethod, tup);
}
pub fn compileMethodWith(comptime name: Object, comptime locals: comptime_int, comptime maxStack: comptime_int, comptime verifier: ThreadedFn, comptime tup: anytype) CompileTimeMethod(countNonLabels(tup)) {
    @setEvalBranchQuota(20000);
    const methodType = CompileTimeMethod(countNonLabels(tup));
    var method = methodType.init(name, locals, maxStack, verifier);
    const code = method.code[0..];
    comptime var n = 0;
    inline for (tup) |field| {
        switch (@TypeOf(field)) {
            Code => {
                code[n] = field;
                n = n + 1;
            },
            Object => {
                code[n] = Code.object(field);
                n = n + 1;
            },
            @TypeOf(null) => {
                code[n] = Code.object(Nil);
                n = n + 1;
            },
            comptime_int => {
                code[n] = Code.int(field);
                n = n + 1;
            },
            ThreadedFn => {
                code[n] = Code.prim(field);
                n = n + 1;
            },
            else => {
                comptime var found = false;
                switch (@typeInfo(@TypeOf(field))) {
                    .Pointer => |fPointer| {
                        switch (@typeInfo(fPointer.child)) {
                            .Array => {
                                if (field[0] == ':') {
                                    found = true;
                                } else if (field.len == 1 and field[0] == '^') {
                                    code[n] = Code.uint(n);
                                    n = n + 1;
                                    found = true;
                                } else if (field.len == 1 and field[0] == '*') {
                                    code[n] = Code.int(-1);
                                    n = n + 1;
                                    found = true;
                                } else if (field.len >= 1 and field[0] >= '0' and field[0] <= '9') {
                                    code[n] = Code.ref1(intOf(field[0..]));
                                    n += 1;
                                    found = true;
                                } else {
                                    comptime var lp = 0;
                                    inline for (tup) |t| {
                                        if (@TypeOf(t) == ThreadedFn) {
                                            lp += 1;
                                            if (t == &controlPrimitives.send or
                                                t == &controlPrimitives.sendTail) lp += 4;
                                        } else switch (@typeInfo(@TypeOf(t))) {
                                            .Pointer => |tPointer| {
                                                switch (@typeInfo(tPointer.child)) {
                                                    .Array => {
                                                        if (t[0] == ':') {
                                                            if (comptime std.mem.endsWith(u8, t, field)) {
                                                                code[n] = Code.int(lp - n - 1);
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
                            .Fn => {
                                @compileLog(field);
                                code[n] = Code.prim(field);
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
test "compiling method" {
    const expectEqual = std.testing.expectEqual;
    var m = compileMethod(Sym.yourself, 0, 0, .{ ":abc", &p.hardDnu, "def", True, comptime Object.from(42), ":def", "abc", "*", "^", 3, "0mref", Sym.i_0, null });
    m.setLiterals(&[_]Object{Sym.value}, &[_]Object{Object.from(42)});
    const t = m.code[0..];
    //    for (t,0..) |tv,idx|
    //        trace("\nt[{}]: 0x{x:0>16}",.{idx,tv.uint});
    //    try expectEqual(t.prim,controlPrimitives.noop);
    try expectEqual(t[0].prim, p.hardDnu);
    try expectEqual(t[1].int, 2);
    try expectEqual(t[2].object, True);
    try expectEqual(t[3].object, Object.from(42));
    try expectEqual(t[4].int, -5);
    try expectEqual(t[5].int, -1);
    try expectEqual(t[6].int, 6);
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
                if (last>=0)
                    objects[last] = @as(HeapHeader,@bitCast(objects[last])).withLength(n - last - 1).o();
                const header = HeapHeader.calc(field, 0, 0xffffff, Age.static, null, Object, false) catch unreachable;
                objects[n] = header.o();
                last = n;
                n += 1;
            },
            else => {
                comptime var found = false;
                switch (@typeInfo(@TypeOf(field))) {
                    .Pointer => |fPointer| {
                        switch (@typeInfo(fPointer.child)) {
                            .Array => {
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
                                            .Pointer => |tPointer| {
                                                switch (@typeInfo(tPointer.child)) {
                                                    .Array => {
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
    if (last>=0)
        objects[last] = @as(HeapHeader,@bitCast(objects[last])).withLength(n - last - 1).o();
    return obj;
}
test "compileObject" {
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
        c.Method, // third HeapObject
        True,
        "def",

    });
    std.debug.print("\nhere",.{});
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
    try expectEqual(h3.header.classIndex, c.Method);
    try expectEqual(h3.header.length, 2);
    try expectEqual(h3.header.age, .static);
    try expectEqual(h3.header.format, .notIndexableWithPointers);
}
// test "method object" {
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

pub const controlPrimitives = struct {
    const ContextPtr = CodeContextPtr;
    pub inline fn checkSpace(pc: PC, sp: SP, process: TFProcess, context: TFContext, needed: usize) void {
        _ = .{pc, sp, process, context, needed};
    }
    pub fn send(pc: PC, sp: SP, process: TFProcess, context: TFContext, signature: MethodSignature) callconv(stdCall) SP {
        _ = .{pc, sp, process, context, signature, @panic("send")};
    }
    pub fn sendTail(pc: PC, sp: SP, process: TFProcess, context: TFContext, signature: MethodSignature) callconv(stdCall) SP {
        _ = .{pc, sp, process, context, signature, @panic("send")};
    }
    pub fn verifyMethod(pc: PC, sp: SP, process: TFProcess, context: TFContext, signature: MethodSignature) callconv(stdCall) SP {
        _ = signature;
        //const method = pc.compiledMethodPtr(1); // must be first word in method, pc already bumped
        //trace("\nverifySelector: 0x{x} 0x{x} {}", .{ method.selector.rawU(), selector.rawU(), pc });
        // if (!method.selector.selectorEquals(selector)) {
        //     return @call(tailCall, hardDnu, .{ pc, sp, process, context, selector});
        // }
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined });
    }
    pub fn verifyDirectSelector(pc: PC, sp: SP, process: TFProcess, context: TFContext, signature: MethodSignature) callconv(stdCall) SP {
        const method = (&pc[0]).compiledMethodPtr(1); // must be first word in method, pc already bumped
        trace("\nverifyDirectSelector: {} {} {}", .{ method.selector, signature, pc });
        if (!method.selector.selectorEquals(signature)) {
            return @call(tailCall, hardDnu, .{ pc, sp, process, context, signature });
        }
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined });
    }
    pub fn branch(pc: PC, sp: SP, _process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const process: *Process = @alignCast(@ptrCast(_process));
        const offset = pc.int();
        trace("\nbranch offset: {}", .{offset});
        if (offset >= 0) {
            const target = pc.skip(@as(u64, @intCast(offset)) + 1);
            if (process.needsCheck()) return @call(tailCall, check, .{ target, sp, process, context, undefined });
            return @call(tailCall, target.prim(), .{ target.next(), sp, process, context, undefined });
        }
        const target = pc.next().back(@as(u64, @intCast(-offset)));
        if (process.needsCheck()) return @call(tailCall, check, .{ target, sp, process, context, undefined });
        return @call(tailCall, target.prim(), .{ target.next(), sp, process.decCheck(), context, undefined });
    }
    pub fn ifTrue(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        trace("\nifTrue: {any}", .{context.stack(sp, process)});
        const v = sp.top;
        if (True.equals(v)) return @call(tailCall, branch, .{ pc, sp.drop(), process, context, undefined });
        if (False.equals(v)) return @call(tailCall, pc.prim2(), .{ pc.next2(), sp.drop(), process, context, undefined });
        @panic("non boolean");
    }
    pub fn ifFalse(pc: PC, sp: SP, _process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const process: *Process = @alignCast(@ptrCast(_process));
        const context: *Context = @alignCast(@ptrCast(_context));
        trace("\nifFalse: {any}", .{context.stack(sp, process)});
        const v = sp.top;
        if (False.equals(v)) return @call(tailCall, branch, .{ pc, sp.drop(), process, context, undefined });
        if (True.equals(v)) return @call(tailCall, pc.next().prim(), .{ pc.skip(2), sp.drop(), process, context, undefined });
        @panic("non boolean");
    }
    pub fn ifNil(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const v = sp[0];
        if (Nil.equals(v)) return @call(tailCall, branch, .{ pc, sp.pop(), process, context, undefined });
        return @call(tailCall, pc.next().prim, .{ pc.skip(2), sp.pop(), process, context, undefined });
    }
    pub fn ifNotNil(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const v = sp[0];
        if (Nil.equals(v)) return @call(tailCall, pc.next().prim, .{ pc.skip(2), sp.pop(), process, context, undefined });
        return @call(tailCall, branch, .{ pc, sp.pop(), process, context, undefined });
    }
    pub fn primFailure(_: PC, _: SP, _: *Process, _: ContextPtr, _: MethodSignature) callconv(stdCall) SP {
        @panic("primFailure");
    }
    pub fn dup(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const newSp = sp.push(sp.top);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn over(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const newSp = sp.push(sp.next);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn drop(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        return @call(tailCall, pc.prim(), .{ pc.next(), sp.drop(), process, context, undefined });
    }
    pub fn dropNext(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        trace("\ndropNext: {}", .{sp.top});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp.dropPut(sp.top), process, context, undefined });
    }
    pub fn swap(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const saved = sp.top;
        sp.top = sp.next;
        sp.next = saved;
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined });
    }
    pub fn replaceLiteral(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        sp.top = pc[0].object;
        trace("\nreplaceLiteral: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.next().prim, .{ pc.skip(2), sp, process, context, undefined });
    }
    pub fn replaceLiteral0(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        sp.top = Object.from(0);
        trace("\nreplaceLiteral0: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined });
    }
    pub fn replaceLiteral1(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        sp.top = Object.from(1);
        trace("\nreplaceLiteral0: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined });
    }
    pub fn pushLiteral(pc: PC, sp: SP, _process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const process: *Process = @alignCast(@ptrCast(_process));
        const context: *Context = @alignCast(@ptrCast(_context));
        const newSp = sp.push(pc.object());
        trace("\npushLiteral: {any}", .{context.stack(newSp, process)});
        return @call(tailCall, pc.next().prim(), .{ pc.skip(2), newSp, process, context, undefined });
    }
    pub fn pushLiteral0(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const newSp = sp.push(Object.from(0));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteral1(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const newSp = sp.push(Object.from(1));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteral2(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const newSp = sp.push(Object.from(2));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteral_1(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const newSp = sp.push(Object.from(-1));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteralIndirect(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const newSp = sp.push(pc.literalIndirect());
        return @call(tailCall, pc.prim2(), .{ pc.next2(), newSp, process, context, undefined });
    }
    pub fn pushLiteralNil(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const newSp = sp.push(Nil);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteralTrue(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const newSp = sp.push(True);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLiteralFalse(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const newSp = sp.push(False);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushThisContext(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const newSp = sp.push(Object.from(context));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn pushLocal(pc: PC, sp: SP, _process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const process: *Process = @alignCast(@ptrCast(_process));
        const context: *Context = @alignCast(@ptrCast(_context));
        const newSp = sp.push(context.getLocal(pc.uint()));
        trace("\npushLocal: {any} {any}", .{ context.stack(newSp, process), context.allLocals(process) });
        return @call(tailCall, pc.next().prim(), .{ pc.skip(2), newSp, process, context, undefined });
    }
    pub fn pushLocal0(pc: PC, sp: SP, _process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const process: *Process = @alignCast(@ptrCast(_process));
        const context: *Context = @alignCast(@ptrCast(_context));
        const newSp = sp.push(context.getLocal(0));
        trace("\npushLocal0: {any}", .{context.stack(newSp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, undefined });
    }
    pub fn popLocal0(pc: PC, sp: SP, process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const context: *Context = @alignCast(@ptrCast(_context));
        context.setLocal(0, sp.top);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp + 1, process, context, undefined });
    }
    pub fn storeLocal(pc: PC, sp: SP, process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const context: *Context = @alignCast(@ptrCast(_context));
        trace("\nstoreIntoLocal: {} {}", .{ pc.uint, sp.top });
        context.setLocal(pc.uint, sp.top);
        return @call(tailCall, pc.next().prim, .{ pc.skip(2), sp, process, context, undefined });
    }
    pub fn pushLocalField(pc: PC, sp: SP, process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const context: *Context = @alignCast(@ptrCast(_context));
        const ref = pc.uint;
        const local = context.getLocal(ref & 0xff);
        const newSp = sp.push(local.getField(ref >> 12));
        trace("\npushLocalField: {} {} {any} {any}", .{ ref, local, context.stack(newSp, process), context.allLocals(process) });
        return @call(tailCall, pc.next().prim, .{ pc.skip(2), newSp, process, context, undefined });
    }
    pub fn popLocalField(pc: PC, sp: SP, process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const context: *Context = @alignCast(@ptrCast(_context));
        const ref = pc.uint;
        const local = context.getLocal(ref & 0xfff);
        trace("\npopLocalField: {} {}", .{ ref, sp.top });
        local.setField(ref >> 12, sp.top);
        return @call(tailCall, pc.next().prim, .{ pc.skip(2), sp + 1, process, context, undefined });
    }
    pub fn pushLocalData(pc: PC, sp: SP, process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const context: *Context = @alignCast(@ptrCast(_context));
        const ref = pc.uint;
        const local = context.getLocal(ref & 0xfff);
        const newSp = sp.push(local - (ref >> 12));
        trace("\npushLocalData: {} {} {any} {any}", .{ ref, local, context.stack(newSp, process), context.allLocals(process) });
        return @call(tailCall, pc.next().prim, .{ pc.skip(2), newSp, process, context, undefined });
    }
    pub fn popLocalData(pc: PC, sp: SP, process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const context: *Context = @alignCast(@ptrCast(_context));
        const ref = pc.uint;
        const local = context.getLocal(ref & 0xff);
        trace("\npopLocalData: {} {}", .{ ref, sp.top });
        local.setData(ref >> 12, sp.top);
        return @call(tailCall, pc.next().prim, .{ pc.skip(2), sp + 1, process, context, undefined });
    }
    pub fn popLocal(pc: PC, sp: SP, process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const context: *Context = @alignCast(@ptrCast(_context));
        trace("\npopIntoLocal: {} {}", .{ pc.uint(), sp.top });
        context.setLocal(pc.uint(), sp.top);
        return @call(tailCall, pc.next().prim(), .{ pc.skip(2), sp.drop(), process, context, undefined });
    }
    pub fn printStack(pc: PC, sp: SP, process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const context: *Context = @alignCast(@ptrCast(_context));
        trace("\nstack: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, undefined });
    }
    pub fn primitiveFailed(pc: PC, sp: SP, process: TFProcess, context: TFContext, signature: MethodSignature) callconv(stdCall) SP {
        _ = .{ pc, sp, process, context, signature, @panic("primitiveFailed") };
    }
    pub fn fallback(pc: PC, sp: SP, process: TFProcess, _context: TFContext, signature: MethodSignature) callconv(stdCall) SP {
        const context: *Context = @alignCast(@ptrCast(_context));
        const self = sp.at(signature.numArgs());
        context.setReturn(pc);
        const class = self.get_class();
        const newPc = lookupAddress(signature, class);
        trace("\nfallback: {} {} {} {} {}", .{ signature, class, pc, newPc, newPc.prim() });
        return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, undefined });
    }
    pub fn call(pc: PC, sp: SP, process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const context: *Context = @alignCast(@ptrCast(_context));
        context.setReturn(pc.next());
        const offset = pc.uint();
        const method = pc.skip(offset + 1).object().to(CompiledMethodPtr);
        const newPc = PC.init(method.codePtr());
        return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, undefined });
    }
    pub fn callRecursive(pc: PC, sp: SP, process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const context: *Context = @alignCast(@ptrCast(_context));
        context.setReturn(pc.next());
        const offset = pc.int();
        const newPc = pc.next().back(@intCast(-offset));
        trace("\ncallRecursive: {any}", .{context.stack(sp, process)});
        return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, undefined });
    }
    pub fn send0(pc: PC, sp: SP, process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const context: *Context = @alignCast(@ptrCast(_context));
        const self = sp.top;
        context.setReturn(pc.next2());
        const class = self.get_class();
        const signature = pc.object();
        trace("\nsend0: {} {}", .{ signature, class });
        const newPc = lookupAddress(signature, class);
        trace(" {} {any}", .{ newPc, process.getStack(sp) });
        return @call(tailCall, newPc.*.prim(), .{ newPc.*.next(), sp, process, context, signature });
    }
    //    pub fn tailSend0(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
    //    }
    // pub fn send1(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature, prevCache: SendCache) callconv(stdCall) SP {
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
    //    pub fn tailSend1(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
    //    }
    pub fn perform(pc: PC, sp: SP, process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const context: *Context = @alignCast(@ptrCast(_context));
        const selector = sp.top;
        const numArgs = selector.numArgs();
        if (numArgs != 0) @panic("wrong number of args");
        const newPc = lookupAddress(selector, sp[numArgs + 1].get_class());
        context.setReturn(pc);
        return @call(tailCall, newPc.prim(), .{ newPc.next(), sp + 1, process, context, undefined });
    }
    pub fn performWith(pc: PC, sp: SP, process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const context: *Context = @alignCast(@ptrCast(_context));
        const selector = sp.next;
        sp.next = sp.top;
        if (selector.numArgs() != 1) @panic("wrong number of args");
        const newPc = lookupAddress(selector, sp[2].get_class());
        context.setTPc(pc + 1);
        return @call(tailCall, newPc.prim(), .{ newPc.next(), sp + 1, process, context, undefined });
    }
    pub fn pushContext(pc: PC, sp: SP, process: TFProcess, context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const method = @as(CompiledMethodPtr, @ptrFromInt(@intFromPtr(pc.back(pc.uint()).asCodePtr()) - CompiledMethod.codeOffset));
        const stackStructure = method.stackStructure;
        const locals = stackStructure.low16() & 255;
        const maxStackNeeded = stackStructure.mid16();
        const selfOffset = stackStructure.high16();
        trace("\npushContext: locals={} maxStack={} selfOffset={} selector={x}", .{ locals, maxStackNeeded, selfOffset, method.code[1] });
        const ctxt = context.push(sp, process, method, locals, maxStackNeeded, selfOffset);
        const newSp = ctxt.asNewSp();
        trace("\npushContext: {any} {} {} {} 0x{x} 0x{x}", .{ process.getStack(sp), locals, method.selector(), selfOffset, @intFromPtr(ctxt), @intFromPtr(sp) });
        return @call(tailCall, pc.prim2(), .{ pc.next2(), newSp, process, ctxt, undefined });
    }
    pub fn returnWithContext(_: PC, sp: SP, _process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const process: *Process = @alignCast(@ptrCast(_process));
        const context: *Context = @alignCast(@ptrCast(_context));
        trace("\nreturnWithContext: {any} -> ", .{context.stack(sp, process)});
        const result = context.pop(process);
        const newSp = result.sp;
        var callerContext = result.ctxt;
        const stack = callerContext.stack(newSp, process);
        if (stack.len < 20) {
            trace("{any}", .{stack});
        } else trace("{}", .{stack.len});
        trace("\nrWC: sp={*} newSp={*}\n", .{ sp, newSp });
        return @call(tailCall, callerContext.getNPc(), .{ callerContext.getTPc(), newSp, process, @constCast(callerContext), undefined });
    }
    pub fn returnTop(_: PC, sp: SP, _process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const process: *Process = @alignCast(@ptrCast(_process));
        const context: *Context = @alignCast(@ptrCast(_context));
        trace("\nreturnTop: {any} ", .{context.stack(sp, process)});
        const top = sp.top;
        const result = context.pop(process);
        const newSp = result.sp;
        newSp.top = top;
        const callerContext = result.ctxt;
        trace("-> {x}", .{@intFromPtr(newSp)});
        trace("-> {any}", .{callerContext.stack(newSp, process)});
        return @call(tailCall, callerContext.getNPc(), .{ callerContext.getTPc(), newSp, process, @constCast(callerContext), undefined });
    }
    pub fn returnNoContext(_: PC, sp: SP, process: TFProcess, _context: TFContext, _: MethodSignature) callconv(stdCall) SP {
        const context: *Context = @alignCast(@ptrCast(_context));
        trace("\nreturnNoContext: {x} {any} N={} T={}", .{ context.method.code[2], context.stack(sp, process), context.getNPc(), context.getTPc() });
        return @call(tailCall, context.getNPc(), .{ context.getTPc(), sp, process, context, undefined });
    }
    pub fn forceDnu(pc: PC, sp: SP, process: TFProcess, context: TFContext, signature: MethodSignature) callconv(stdCall) SP {
        std.debug.print("\nforceDnu: 0x{x} {} {}", .{ signature.selectorHash, signature.classIndex, signature.asSymbol() });
        _ = .{ pc, sp, process, context, signature, @panic("forceDnu unimplemented") };
    }
    fn hardDnu(_: PC, sp: SP, process: TFProcess, context: TFContext, signature: MethodSignature) callconv(stdCall) SP {
        trace("\nhardDnu: {}", .{ signature });
        const newPc = lookupAddress(signature);
        return @call(tailCall, newPc.prim(), .{ newPc.*.next(), sp, process, context, undefined });
    }
    // fn cacheDnu(_: PC, sp: SP, process: TFProcess, context: TFContext, signature: MethodSignature) callconv(stdCall) SP {
    //     trace("\ncacheDnu: 0x{x} {} {}", .{ selector.u(), signature.classIndex, signature.asSymbol() });
    //     const newPc = lookupAddress(selector, signature.classIndex);
    //     const newCache = cache.previous();
    //     newCache.cache1 = newPc;
    //     trace("\ncacheDnu: {} {*} {*}", .{ newCache, newCache, cache });
    //     return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, undefined });
    //     //        const pc = cache.current();
    //     //        return @call(tailCall, pc.prim(), .{ pc+1, sp, process, context, signature, cache.next() });
    // }
};
pub const Execution = struct {
    process: Process,
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
    var yourself = CompiledMethod.init(Sym.noFallback, Code.end);
    pub fn initStack(self: *Self, source: []const Object) void {
        self.sp = self.process.endOfStack().reserve(source.len);
        for (source, self.sp.slice(source.len)) |src, *dst|
            dst.* = src;
        trace("\ninitial-stack: {x} {x}", .{ @intFromPtr(self.sp), @intFromPtr(self.process.endOfStack()) });
    }
    pub fn stack(self: *Self, sp: SP) []Object {
        self.sp = sp;
        trace("\nfinal-stack: {x} {x}", .{ @intFromPtr(sp), @intFromPtr(self.process.endOfStack()) });
        return self.ctxt.stack(self.sp, &self.process);
    }
    pub fn run(self: *Self, source: []const Object, ptr: anytype) []Object {
        const method: CompiledMethodPtr = @constCast(@ptrCast(ptr));
        const stdout = std.io.getStdOut().writer();
        self.initStack(source);
        self.ctxt.setReturn(Code.endThread);
        if (@TypeOf(trace) == @TypeOf(std.debug.print) and trace == std.debug.print) method.write(stdout) catch unreachable;
        //        trace("\nrun: {} {*}",.{cache.dontCache(),cache.dontCache().current()});
        return self.stack(method.execute(self.sp, &self.process, &self.ctxt));
    }
};
const p = struct {
    usingnamespace controlPrimitives;
};
fn push42(_: PC, sp: SP, _: TFProcess, _: TFContext, _: MethodSignature) callconv(stdCall) SP {
    const newSp = sp.push(Object.from(42));
    return newSp;
}
test "send with dispatch direct" {
    const expectEqual = std.testing.expectEqual;
    const method = compileMethod(Sym.yourself, 0, 0, .{
        &p.send,           Sym.value,
        &p.primitiveFailed,
    });
    const methodV = compileMethod(Sym.value, 0, 0, .{
        &push42,
        &p.primitiveFailed,
    });
    init();
    methodV.asCompiledMethodPtr().forDispatch(.UndefinedObject);
    var te = Execution.new();
    te.init();
    var objs = [_]Object{ Nil, True };
    const result = te.run(objs[0..], &method);
    try expectEqual(result.len, 3);
    try expectEqual(result[0], Object.from(42));
}
test "simple return via Execution" {
    const expectEqual = std.testing.expectEqual;
    var method = compileMethod(Sym.yourself, 0, 0, .{
        &p.pushLiteral,     comptime Object.from(42),
        &p.returnNoContext,
    });
    var te = Execution.new();
    te.init();
    var objs = [_]Object{ Nil, True };
    const result = te.run(objs[0..], &method);
    try expectEqual(result.len, 3);
    try expectEqual(result[0], Object.from(42));
    try expectEqual(result[1], Nil);
    try expectEqual(result[2], True);
}
test "context return via Execution" {
    const expectEqual = std.testing.expectEqual;
    var method = compileMethod(Sym.@"at:", 0, 0, .{
        &p.pushContext,       "^",
        &p.pushLiteral,       comptime Object.from(42),
        &p.returnWithContext,
    });
    var te = Execution.new();
    te.init();
    var objs = [_]Object{ Nil, True };
    const result = te.run(objs[0..], &method);
    try expectEqual(result.len, 1);
    try expectEqual(result[0], True);
}
test "context returnTop via Execution" {
    const expectEqual = std.testing.expectEqual;
    var method = compileMethod(Sym.yourself, 3, 0, .{
        &p.pushContext, "^",
        &p.pushLiteral, comptime Object.from(42),
        &p.returnTop,
    });
    var te = Execution.new();
    te.init();
    var objs = [_]Object{ Nil, True };
    const result = te.run(objs[0..], &method);
    try expectEqual(result.len, 2);
    try expectEqual(result[0], Object.from(42));
}
test "context returnTop twice via Execution" {
    const expectEqual = std.testing.expectEqual;
    const empty = Object.empty;
    var method1 = compileMethod(Sym.yourself, 3, 0, .{
        &p.pushContext, "^",
        &p.pushLiteral, comptime Object.from(1),
        &p.call,        "0Obj",
        &p.returnTop,
    });
    var method2 = compileMethod(Sym.name, 3, 0, .{
        &p.pushContext, "^",
        &p.pushLiteral, comptime Object.from(42),
        &p.returnTop,
    });
    method1.setLiterals(empty, &[_]Object{Object.from(&method2)});
    var te = Execution.new();
    te.init();
    var objs = [_]Object{ Nil, True };
    const result = te.run(objs[0..], &method1);
    try expectEqual(result.len, 2);
    try expectEqual(result[0], Object.from(42));
}
test "context returnTop with indirect via Execution" {
    const expectEqual = std.testing.expectEqual;
    const empty = Object.empty;
    var method = compileMethod(Sym.yourself, 3, 0, .{
        //        &p.noop,
        &p.pushContext,
        "^",
        &p.pushLiteralIndirect,
        "0Obj",
        &p.returnTop,
    });
    method.setLiterals(empty, &[_]Object{Object.from(42)});
    var te = Execution.new();
    te.init();
    var objs = [_]Object{ Nil, True };
    const result = te.run(objs[0..], &method);
    try expectEqual(result.len, 2);
    try expectEqual(result[0], Object.from(42));
}
test "simple executable" {
    const expectEqual = std.testing.expectEqual;
    var method = compileMethod(Sym.yourself, 1, 0, .{
        &p.pushContext,           "^",
        ":label1",                &p.pushLiteral,
        comptime Object.from(42), &p.popLocal,
        0,                        &p.pushLocal0,
        &p.pushLiteral0,          &p.pushLiteralTrue,
        &p.ifFalse,               "label3",
        &p.branch,                "label2",
        ":label3",                &p.pushLocal,
        0,                        ":label4",
        &p.returnTop,             ":label2",
        &p.pushLiteral0,          &p.branch,
        "label4",
    });
    var objs = [_]Object{Nil};
    var te = Execution.new();
    te.init();
    const result = te.run(objs[0..], &method);
    try expectEqual(result.len, 1);
    try expectEqual(result[0], Object.from(0));
}

// const std = @import("std");
// const config = @import("config.zig");
// const tailCall = config.tailCall;
// const trace = config.trace;
// const stdCall = config.stdCall;
// const object = @import("zobject.zig");
// const Object = object.Object;
// const Nil = object.Nil;
// //const class = @import("class.zig");
// const ClassIndex = object.ClassIndex;
const max_classes = 100; //class.ReservedNumberOfClasses;
// const Process = @import("process.zig").Process;
// const heap = @import("heap.zig");
// const HeapPtr = heap.HeapPtr;
// const HeapObject = heap.HeapObject;
// const HeapHeader = heap.HeapHeader;
// const builtin = @import("builtin");
const symbols = symbol.symbols;
// const execute = @import("execute.zig");
// const SendCache = execute.SendCache;
// const Context = execute.Context;
// const TestExecution = execute.TestExecution;
// const ThreadedFn = execute.ThreadedFn;
// const TFProcess = execute.TFProcess;
// const TFContext = execute.TFContext;
// const CompiledMethod = execute.CompiledMethod;
// const compileMethod = execute.compileMethod;
// const compiledMethodType = execute.compiledMethodType;
// const Code = execute.Code;
// const PC = execute.PC;
// const SP = execute.SP;
// const CodeContextPtr = execute.CodeContextPtr;
// const MethodSignature = execute.MethodSignature;
const smallestPrimeAtLeast = @import("utilities.zig").smallestPrimeAtLeast;
// // note that self and other could become invalid after any method call if they are heap objects, so will need to be re-loaded from context.fields if needed thereafter

pub const forTest = Dispatch.forTest;
const noArgs = ([0]Object{})[0..];
pub const lookupAddress = Dispatch.lookupAddressForClass;
pub const dump = Dispatch.dump;
pub const initClass = Dispatch.initClass;
pub fn init() void {
    _ = Dispatch.new();
}
pub const addMethod = Dispatch.addMethod;
const DispatchElement = extern struct {
    primitive: ThreadedFn,
    methodPointer: *const CompiledMethod,
    const baseType = Self;
    const Self = @This();
    pub inline fn init(_method: *const CompiledMethod) Self {
        return .{.primitive = _method.code[0].prim,.methodPointer = _method};
    }
    inline fn set(self: *Self, _method: *const CompiledMethod) void {
        self.primitive = _method.code.prim;
        self.methodPointer = _method;
    }
    inline fn asInt(self: Self) u128 {
        return @bitCast(self);
    }
    inline fn asIntPtr(self: *Self) *u128 {
        return @alignCast(@ptrCast(self));
    }
    inline fn prim(self: *const Self) ThreadedFn {
        return self.primitive;
    }
    inline fn pc(self: *const Self) PC {
        return @ptrCast(self.methodPointer);
    }
    inline fn method(self: *const Self) *const CompiledMethod {
        return self.methodPointer;
    }
    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = .{fmt,options};
        try writer.print("DispatchElement(ThreadedFn@{x},CompiledMethod@{x})", .{@intFromPtr(self.primitive),@intFromPtr(self.methodPointer)});
    }
};
const Dispatch = extern struct {
    header: HeapHeader,
    hash: u64,
    free: u16,
    length: u16,
    state: DispatchState,
    fixed: [numberOfFixed]DispatchElement align(@sizeOf(DispatchElement)),
    methods: [hashedMethods+1]DispatchElement, // this is just the default... normally a larger array
    const Self = @This();
    const Fixed = enum {
        equal,
        hash,
        value,
        valueColon,
        cullColon,
        // insert new names here
        maxIndex,
    };
    const numberOfFixed: usize = @intFromEnum(Fixed.maxIndex);
    const hashedMethods = (if (config.indirectDispatch) 59 else 29) - numberOfFixed; // FIX was 12 else 6
    const classIndex = ClassIndex.Dispatch;
    const DispatchState = enum(u8) { clean, beingUpdated, dead };
    var empty = Self{
        .header = HeapHeader.staticHeaderWithClassLengthHash(classIndex, @offsetOf(Self, "methods") / 8 - 1 + 1, 0), // don't count header, but do count one element of methods
        .hash = 1,
        .free = 0,
        .length = 1,
        .state = .clean,
        .fixed = undefined,
        .methods = undefined,
    };
    comptime {
        // @compileLog(@sizeOf(Self));
        std.debug.assert(@as(usize, 1) << @ctz(@as(u62, @sizeOf(Self) + 16)) == @sizeOf(Self) + 16);
    }
    const dnu = if (@import("builtin").is_test) &testDnu else &controlPrimitives.forceDnu;
    const dnuThread = [_]Code{Code.prim(dnu),Code.uint(0)};
    const dnuInit = DispatchElement.init(&dnuThread[0]);
    var dispatchData: [max_classes]Self = undefined;
    var dispatches = [_]*Self{&empty}**max_classes;
    fn dump(index: ClassIndex) void {
        trace("\ndump: {} {}",.{index,dispatches[@intFromEnum(index)]});
    }
    fn initClass(index: ClassIndex) void {
        dispatches[@intFromEnum(index)].init();
    }
    pub fn addMethod(index: ClassIndex, method: *CompiledMethod) !void {
        trace("\naddMethod: {} {} {}", .{ index, method.selector(), method.codePtr() });
        //method.checkFooter();
        const idx = @intFromEnum(index);
        var dispatchP = dispatches[idx];
        if (dispatchP == &empty) {
            dispatchP = &dispatchData[idx];
            dispatches[idx] = dispatchP;
            dispatchP.init();
        }
        return try dispatchP.add(method);
    }
    inline fn init(self: *Self) align(@sizeOf(DispatchElement)) void {
        self.initOfSize(@sizeOf(Self) / @sizeOf(usize));
    }
    inline fn initOfSize(self: *Self, words: usize) align(@sizeOf(DispatchElement)) void {
        self.header = HeapHeader.staticHeaderWithClassLengthHash(classIndex, words - 1, 0);
        const nMethods: u16 = (words * @sizeOf(usize) - @offsetOf(Self, "methods")) / @sizeOf(DispatchElement);
        const hash = smallestPrimeAtLeast(nMethods * 10 / 6);
        self.hash = hash;
        for (self.fixed[0..]) |*ptr|
            ptr.* = dnuInit;
        for (self.methods[0..nMethods]) |*ptr|
            ptr.* = dnuInit;
        self.free = hash;
        self.length = nMethods;
        self.state = .clean;
    }
    fn isAvailable(self: *Self, ptr: PC) bool {
        _ = self;
        if (ptr == dnuInit) return true;
        return false;
    }
    inline fn lookupAddress(self: *align(@sizeOf(DispatchElement)) const Self, selector: Object) align(@sizeOf(DispatchElement)) *DispatchElement {
        const hash = doHash(selector,self.hash);
        trace("\nlookupAddress: {} {} {*} {*}",.{selector,hash,&self.methods[0],&self.methods[hash]});
        return @constCast(&self.methods[hash]);
    }
    inline fn doHash(selector: Object, size: u64) u64 {
        return @as(u64,@intCast(selector.hash32())) * size >> 32;
    }
    pub inline fn lookupAddressForClass(selector: Object, index: ClassIndex) *DispatchElement {
        trace(" (lookupAddressForClass) {}", .{index});
        const code = dispatches[@intFromEnum(index)].lookupAddress(selector);
        return code;
    }
    fn add(self: *Self, cmp: *CompiledMethod) !void {
        trace("\nadd: {}", .{ cmp.selector()});
        while (true) {
            trace("\nadd: {*}",.{self});
            if (@cmpxchgWeak(DispatchState, &self.state, .clean, .beingUpdated, .seq_cst, .seq_cst)) |notClean| {
                if (notClean == .dead) return error.DeadDispatch;
            } else break;
        }
        trace("\nadd: after loop", .{});
        defer {
            self.state = .clean;
        }
        const address = self.lookupAddress(cmp.selector());
        trace("\nadd: {} {} {}", .{ cmp.selector(), address, address.* });
        const newElement = DispatchElement.init(cmp.codePtr()).asInt();
        if (@cmpxchgWeak(DispatchElement.equivalentInt(), address.asIntPtr(), dnuInit.asInt(), newElement, .seq_cst, .seq_cst) == null) {
            //trace("\nexchange: {*} {*} {*} {}", .{ address.*, dnuInit, cmp.codePtr(), cmp.codePtr()[0] });
            return; // we replaced DNU with method
        }
        const existing = address.compiledMethodPtr(0);
        if (existing.selector().equals(cmp.selector())) {
            address.set(cmp.codePtr());
            return;
        } else trace("\nexisting: {}", .{existing.selector()});
        if (self.isExternalCompiledMethod(@constCast(existing).codePtr().prim)) { // an actual cmp - not internal
            trace("\nfree:{} hash:{} len:{} length:{}",.{self.free,self.hash,self.header.length,self.length});
            if (self.free < self.length) {
                self.free += 3;
                unreachable;//return;
            }
        }
        return error.Conflict;
    }
    fn fail(programCounter: PC, sp: SP, process: TFProcess, context: TFContext, signature: MethodSignature) callconv(stdCall) SP {
        _ = .{ programCounter, sp, process, context, signature };
        if (programCounter.uint() == 0)
            @panic("called fail function");
        @panic("fail with non-zero next");
    }
    fn testDnu(programCounter: PC, sp: SP, process: TFProcess, context: TFContext, signature: MethodSignature) callconv(stdCall) SP {
        _ = .{ programCounter, sp, process, context, signature };
        return sp.push(object.NotAnObject);
    }
    fn testIncrement(programCounter: PC, sp: SP, process: TFProcess, context: TFContext, signature: MethodSignature) callconv(stdCall) SP {
        _ = .{ process, context, signature };
        @as(*usize, @ptrFromInt(programCounter.uint())).* += 1;
        return sp;
    }
};
//fn initTest(self: *Self, target: *usize) void {
//    self.initPrivate(.{Code.prim(&testIncrement),Code.uint(@intFromPtr(target))});
//}
//pub fn forTest() void {
//    var foo = Self.new();
//    foo.initDNU();
//u}
fn tOffset(hash: u32, ptr:*const DispatchElement, comptime bit: comptime_int) *const DispatchElement {
    return Dispatch.offset(hash,PC.init(@ptrCast(ptr)),bit);
}
fn eo(lDe: *const DispatchElement, rDe: *const DispatchElement) !void {
    try std.testing.expectEqual(@intFromPtr(lDe),@intFromPtr(rDe));
}
test "offset for bitTests" {
    const de = [_]DispatchElement{undefined} ** 5;
    try eo(tOffset(0xffffff55, &de[0], 0), &de[1]);
    try eo(tOffset(0xffffff55, &de[0], 1), &de[0]);
    try eo(tOffset(0xffffff55, &de[0], 2), &de[1]);
    try eo(tOffset(0xffffff55, &de[0], 3), &de[0]);
    try eo(tOffset(0xffffff55, &de[0], 4), &de[1]);
    try eo(tOffset(0xffffff55, &de[0], 5), &de[0]);
    try eo(tOffset(0xffffff55, &de[0], 6), &de[1]);
    try eo(tOffset(0xffffff55, &de[0], 7), &de[0]);
    try eo(tOffset(0xffffff55, &de[0], 8), &de[1]);
}
test "disambiguate" {
    const ee = std.testing.expectEqual;
    const empty = Object.empty;
    const fns = struct {
        fn push1(_: PC, sp: SP, _: TFProcess, _: TFContext, _: MethodSignature) callconv(stdCall) SP {
            return sp.push(Object.from(1));
        }
        fn push2(_: PC, sp: SP, _: TFProcess, _: TFContext, _: MethodSignature) callconv(stdCall) SP {
            return sp.push(Object.from(2));
        }
        fn push3(_: PC, sp: SP, _: TFProcess, _: TFContext, _: MethodSignature) callconv(stdCall) SP {
            return sp.push(Object.from(3));
        }
    };
    // value=01101 yourself=00001 @"<="=11101
    var method1 = compileMethod(symbols.value, 0, 0, .{ &fns.push1, &Code.end });
    method1.setLiterals(empty, empty);
    var method2 = compileMethod(symbols.yourself, 0, 0, .{ &fns.push2, &Code.end });
    method2.setLiterals(empty, empty);
    var method3 = compileMethod(symbols.@"<=", 0, 0, .{ &fns.push3, &Code.end });
    method3.setLiterals(empty, empty);
    if (config.indirectDispatch) {
        var space3 = [_]DispatchElement{undefined}**3;
        var dispatcher = Dispatch.disambiguate2(&space3, @ptrCast(&method1), @ptrCast(&method2));
        const push1Code = DispatchElement.init(&method1.code[0]);
        const push2Code = DispatchElement.init(&method2.code[0]);
        try ee(space3[1], push1Code);
        try ee(space3[2], push2Code);
        dispatcher = Dispatch.disambiguate2(&space3, @ptrCast(&method2), @ptrCast(&method1));
        try ee(space3[1], push1Code);
        try ee(space3[2], push2Code);
        var process = Process.new();
        process.init();
        defer process.deinit();
        try ee(dispatcher.prim(), &Dispatch.bitTest8);
        dispatcher = Dispatch.disambiguate2(&space3, @ptrCast(&method3), @ptrCast(&method1));
        try ee(dispatcher.prim(), &Dispatch.bitTest0);
    } else {
        var space2 = [_]DispatchElement{undefined}**2;
        var dispatcher = Dispatch.disambiguate2(&space2, @ptrCast(&method1), @ptrCast(&method2));
        const push1Code = DispatchElement.init(&method1.code[0]);
        const push2Code = DispatchElement.init(&method2.code[0]);
        try ee(space2[0], push1Code);
        try ee(space2[1], push2Code);
        dispatcher = Dispatch.disambiguate2(&space2, @ptrCast(&method2), @ptrCast(&method1));
        try ee(space2[0], push1Code);
        try ee(space2[1], push2Code);
        var process = Process.new();
        process.init();
        defer process.deinit();
        var context = Context.init();
        const sp = process.endOfStack();
        if (config.dispatchCache) {
            try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.value).top.to(i64), 1);
            try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.yourself).top.to(i64), 2);
            try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.@"<=").top.to(i64), 3);
            try ee(dispatcher.prim(dispatcher.next(), sp, &process, &context, symbols.value).top.to(i64), 1);
        }
        try ee(dispatcher.prim(), &Dispatch.bitTest2);
        dispatcher = Dispatch.disambiguate2(&space2, @ptrCast(&method3), @ptrCast(&method1));
        try ee(dispatcher.prim(), &Dispatch.bitTest4);
    }
}
test "isExternalCompiledMethod" {
    const e = std.testing.expect;
    var d: Dispatch = undefined;
    try e(d.isExternalCompiledMethod(&Dispatch.bitTest0));
    d = Dispatch.new();
    try e(!d.isExternalCompiledMethod(&Dispatch.bitTest0));
    try e(!d.isExternalCompiledMethod(&Dispatch.bitTest31));
    try e(!d.isExternalCompiledMethod(&Dispatch.super));
    try e(d.isExternalCompiledMethod(&Dispatch.testIncrement));
}
test "empty dispatch" {
    const ee = std.testing.expectEqual;
    _ = Dispatch.new();
    const empty = Dispatch.empty;
    try ee(empty.lookupAddress(symbols.value).*, Dispatch.dnuInit);
}
fn doDispatch(tE: *Execution, dispatch: *Dispatch, signature: MethodSignature) []Object {
    tE.initStack(&[_]Object{Object.from(0)});
    return tE.stack(dispatch.dispatch(tE.sp, &tE.process, &tE.ctxt, signature));
}
// test "add methods" {
//     const empty = Object.empty;
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
//     var tE = TestExecution.new();
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
