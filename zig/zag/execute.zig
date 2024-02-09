const std = @import("std");
const config = @import("config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const dispatchCache = config.dispatchCache;
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
const dispatch = @import("dispatch.zig");
const lookupAddress = dispatch.lookupAddress;
pub const Context = @import("context.zig").Context;
const heap = @import("heap.zig");
const HeapHeader = heap.HeapHeader;
const HeapObject = heap.HeapObject;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
const Format = heap.Format;
const Age = heap.Age;
//const class = @import("class.zig");
const Sym = @import("symbol.zig").symbols;
const phi32 = @import("utilities.zig").inversePhi(u32);

pub const SendCache = *SendCacheStruct;
pub const sendCacheSize = if (dispatchCache) @sizeOf(SendCacheStruct) / @sizeOf(Code) else 0;
pub const SendCacheStruct = if (dispatchCache) extern struct {
    cache1: PC,
    cache2: PC,
    noCache: PC,
    dnu: PC,
    const Self = @This();
    pub fn init() Self {
        return initWith(&dnus[0]);
    }
    pub fn initWith(fn1: PC) Self {
        return .{
            .cache1 = @ptrCast(fn1),
            .cache2 = @ptrCast(fn1),
            .noCache = @ptrCast(&dnus[1]),
            .dnu = @ptrCast(&dnus[2]),
        };
    }
    pub fn current(self: *Self) PC { // INLINE
        return self.cache1;
    }
    pub inline fn next(self: *Self) SendCache {
        trace("\nnext: {}", .{self});
        return @ptrCast(&self.cache2);
    }
    pub inline fn fromDnu(self: *Self) SendCache {
        return @fieldParentPtr(SendCacheStruct, "dnu", @as(*PC, @ptrCast(self))).previous();
    }
    pub inline fn dontCache(self: *Self) SendCache { // don't use on a SendCache that is the result of `next`
        return @ptrCast(&self.dnu);
    }
    pub inline fn previous(self: *Self) SendCache {
        return @ptrCast(@as([*][*]const Code, @ptrCast(self)) - 1);
    }
} else struct {
    const Self = @This();
    pub fn init() Self {
        return .{};
    }
    pub inline fn current(_: *Self) PC {
        return fallbackPc;
    }
    pub inline fn next(self: *Self) SendCache {
        return self;
    }
    pub inline fn dontCache(self: *Self) SendCache {
        return self;
    }
    inline fn fromDnu(self: *Self) SendCache {
        return self;
    }
};
const dnus = [_]Code{
    Code.prim(&controlPrimitives.cacheDnu),
    Code.prim(&controlPrimitives.hardDnu),
    Code.prim(&controlPrimitives.forceDnu),
};
test "SendCache" {
    if (dispatchCache) {
        const expectEqual = std.testing.expectEqual;
        var cache = SendCacheStruct.init();
        try expectEqual(cache.current().prim, &controlPrimitives.cacheDnu);
        try expectEqual(cache.dontCache().current().prim, &controlPrimitives.forceDnu);
        try expectEqual(cache.next().next().current().prim, &controlPrimitives.hardDnu);
        try expectEqual(cache.next().next().next().current().prim, &controlPrimitives.forceDnu);
    }
}
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
pub fn check(pc: PC, sp: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
    if (process.debugger()) |debugger|
        return @call(tailCall, debugger, .{ pc, sp, process, context, selector, cache });
    return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector, cache });
}

pub const ThreadedFn = *const fn (programCounter: PC, stackPointer: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP;
//const TFn = fn (programCounter: PC, stackPointer: SP, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object;
pub const fallback = controlPrimitives.fallback;
const fallbackCode = Code.prim(&fallback);
const fallbackPc = PC.init(&fallbackCode);
pub const CodeContextPtr = *Context;
pub const CompiledMethodPtr = *CompiledMethod;
pub const CompiledMethod = extern struct {
    header: HeapHeader,
    stackStructure: Object, // number of local values beyond the parameters
    selector: Object, // must be the word before code
    code: [codeSize]Code, // will typically be a lot more then 2, as it will be the processed version of the method
    //references: [n]Object,
    const Self = @This();
    const codeSize = 2;
    pub const codeOffset = @offsetOf(CompiledMethod, "code");
    const codeOffsetInUnits = codeOffset / 8;
    pub fn init(name: Object, methodFn: ThreadedFn) Self {
        return init2(name, methodFn, Code.end);
    }
    pub fn init2(name: Object, methodFn: ThreadedFn, methodFn2: ThreadedFn) Self {
        return Self{
            .header = HeapHeader.calc(ClassIndex.CompiledMethod, codeOffsetInUnits + codeSize, name.hash24(), Age.static, null, Object, false) catch unreachable,
            .selector = name,
            .stackStructure = Object.from(0),
            .code = [2]Code{ Code.prim(methodFn), Code.prim(methodFn2) },
        };
    }
    pub fn execute(self: *Self, sp: SP, process: *Process, context: CodeContextPtr, cache: SendCache) callconv(stdCall) SP {
        const pc = PC.init(self.codePtr());
        trace("\nexecuteX: {} {*} {*}", .{ pc, self, self.codePtr() });
        trace("\nexecuteY: {} {} {} {}", .{ self.stackStructure, self.selector, self.code[0], self.code[1] });
        trace("\nexecute: [{*}]: {*} {} {}", .{ pc.asCodePtr(), pc.prim(), sp.top, self.selector });
        //        return @call(tailCall,pc.prim(),.{pc+1,sp,process,context,self.selector});
        return pc.prim()(pc.next(), sp, process, context, self.selector, cache);
    }
    pub fn forDispatch(self: *Self, class: ClassIndex) void {
        if (dispatchCache) self.selector = self.selector.withClass(class);
        dispatch.addMethod(class, self) catch @panic("addMethod failed");
    }
    inline fn asHeapObjectPtr(self: *const Self) HeapObjectConstPtr {
        return @as(HeapObjectConstPtr, @ptrCast(self));
    }
    pub fn checkFooter(self: *Self) void {
        trace("\ncheckFooter*: {}\n    header={}\n    footer={}\n  *footer={}\n   a1={x}\n   a2={x}\n   a3={x}", .{ self.selector, self.header, self.footer, self.header.realHeapObject(), @intFromPtr(self), @intFromPtr(self.header.realHeapObject()), @intFromPtr(&self.footer) });
    }
    pub inline fn matchedSelector(self: *Self, selector: Object) bool {
        return selector.hashEquals(self.selector);
    }
    pub inline fn codePtr(self: *Self) * const Code {
        return &self.code[0];
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
        try writer.print("\nCMethod: {} locals:{} maxStack:{} selfOffset:{} realHO:{} {any} ({any})", .{ self.selector, locals, maxStackNeeded, selfOffset, realHO, all[codeOffset / 8 .. all.len - refs.len], refs });
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
    pub inline fn initDispatchElement(f: ThreadedFn, code: *Code) Self {
        code.* = Code.prim(f);
        return .{.code=code};
    }
    pub fn equivalentInt() type {
        return usize;
    }
    pub inline fn set(self: *Self, code: *const Code) void {
        self.code = code;
    }
    pub inline fn asInt(self: Self) equivalentInt() {
        return @bitCast(self);
    }
    pub inline fn asIntPtr(self: *Self) *equivalentInt() {
        return @alignCast(@ptrCast(self));
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
        return @fieldParentPtr(CompiledMethod, "code", @as(*const [2]Code, @ptrCast(@as([*]const Code, @ptrFromInt(@as(usize,@bitCast(self)))) - index)));
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
    const refFlag = 1024;
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
    pub fn end(_: PC, sp: SP, _: *Process, _: *Context, _: Object, _: SendCache) callconv(stdCall) SP {
        return sp;
    }
    var endCode = [_]Code{.{ .prim = &end }};
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
    comptime var d = 0;
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
            ThreadedFn => {
                if (field == &controlPrimitives.send0 or
                    field == &controlPrimitives.send1) d += 1;
                c += 1;
            },
            comptime_int, comptime_float, ClassIndex => {
                c += 1;
            },
            else => switch (@typeInfo(@TypeOf(field))) {
                .Pointer => |pointer| {
                    if (@hasField(pointer.child, "len"))
                        switch (field[0]) {
                            ':' => {},
                            '0'...'9' => {
                                r = comptime @max(r, intOf(field[0..]) + 1);
                                c += 1;
                            },
                            else => c += 1,
                        }
                    else
                        c += 1;
                },
                else => {
                    c += 1;
                },
            },
        }
    }
    return .{ .codes = c, .refs = r, .objects = o, .caches = d };
}
test "countNonLabels" {
    const expectEqual = std.testing.expectEqual;
    const r1 = countNonLabels(.{
        ":abc",
        &p.send1,
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
    try expectEqual(r1.caches, 1);
}

pub fn CompileTimeMethod(comptime counts: CountSizes) type {
    const codes = counts.codes;
    const refs = counts.refs;
    const caches = counts.caches;
    return extern struct { // structure must exactly match CompiledMethod
        header: HeapHeader,
        stackStructure: Object,
        selector: Object,
        code: [codes + caches * sendCacheSize]Code,
        references: [refs]Object,
        const codeOffsetInUnits = CompiledMethod.codeOffsetInUnits;
        const Self = @This();
        // comptime {
        //     if (checkEqual(CompiledMethod.codeOffset,@offsetOf(Self,"code"))) |s|
        //         @compileError("CompiledMethod prefix not the same as CompileTimeMethod == " ++ s);
        // }
        const cacheSize = @sizeOf(SendCacheStruct) / @sizeOf(Code);
        pub fn init(comptime name: Object, comptime locals: u16, comptime maxStack: u16) Self {
            const header = HeapHeader.calc(.CompiledMethod, codeOffsetInUnits + codes + caches * cacheSize + refs, name.hash24(), Age.static, null, Object, false) catch @compileError("too many refs");
            //  @compileLog(codes,refs,footer,heap.Format.allocationInfo(5,null,0,false));
            return .{
                .header = header,
                .selector = name,
                .stackStructure = Object.packedInt(locals, maxStack, locals + name.numArgs()),
                .code = undefined,
                .references = [_]Object{object.NotAnObject} ** refs,
            };
        }
        pub fn checkFooter(self: *Self) void {
            trace("\ncheckFooter: {}\n    header={}\n    footer={}\n     allocInfo={}\n   a1={x}\n   a2={x}", .{ self.selector, self.header, self.footer, Format.allocationInfo(codeOffsetInUnits + codes, refs, @sizeOf(Object), false), @intFromPtr(self), @intFromPtr(self.header.realHeapObject()) });
        }
        pub fn withCode(name: Object, locals: u16, maxStack: u16, code: [codes]Code) Self {
            const footer = HeapHeader.calcHeapHeader(ClassIndex.CompiledMethod, codeOffsetInUnits + codes + caches * cacheSize + refs, name.hash24(), Age.static, null, Object, false) catch @compileError("too many refs");
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
        pub fn setLiterals(self: *Self, replacements: []const Object, refReplacements: []const Object, cache: ?SendCache) void {
            //trace("\nsetLiterals: 0x{x:0>16} {any}", .{ self.selector.u(), replacements });
            var cachedSend = false;
            for (&self.code) |*c| {
                if (dispatchCache and (c.prim == &controlPrimitives.send0 or c.prim == &controlPrimitives.send1)) {
                    cachedSend = true;
                    unreachable;
                } else {
                    if (c.object.isIndexSymbol0()) {
                        const index = c.object.indexNumber();
                        c.* = Code.object(replacements[index]);
                    }
                    if (dispatchCache and cachedSend) {
                        @as(SendCache, @ptrCast(@as([*]Code, @ptrCast(c)) + 1)).* = if (cache) |aSendCache| aSendCache.* else SendCacheStruct.init();
                        cachedSend = false;
                    }
                }
            }
            if (self.selector.isIndexSymbol0()) {
                const replacement = replacements[self.selector.indexNumber()];
                self.stackStructure.classIndex = @enumFromInt(@intFromEnum(self.stackStructure.classIndex) - (indexSymbol0(0).numArgs() - replacement.numArgs()));
                self.selector = replacement;
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
            trace("-> 0x{x:0>16}", .{self.selector.rawU()});
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
const empty = Object.empty;
test "CompileTimeMethod" {
    const expectEqual = std.testing.expectEqual;
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
    }));
    var r1 = @constCast(c1.init(Nil, 2, 3));
    var r1r = [_]Object{ Nil, True };
    r1.setLiterals(empty, &r1r, null);
    try expectEqual(r1.getCodeSize(), 10);
}
pub fn compiledMethodType(comptime codeSize: comptime_int) type {
    return CompileTimeMethod(.{ .codes = codeSize });
}
pub fn compileMethod(comptime name: Object, comptime locals: comptime_int, comptime maxStack: comptime_int, comptime tup: anytype) *CompileTimeMethod(countNonLabels(tup)) {
    @setEvalBranchQuota(20000);
    const methodType = CompileTimeMethod(countNonLabels(tup));
    var method: *methodType = @constCast(methodType.init(name, locals, maxStack));
    const code = method.code[0..];
    comptime var n = 0;
    comptime var cachedSend = false;
    inline for (tup) |field| {
        switch (@TypeOf(field)) {
            Object => {
                code[n] = Code.object(field);
                n = n + 1;
                if (cachedSend) {
                    code[n] = dnus[0];
                    code[n + 1] = dnus[0];
                    code[n + 2] = dnus[1];
                    code[n + 3] = dnus[2];
                    n += sendCacheSize;
                    cachedSend = false;
                }
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
                if (field == &controlPrimitives.send0 or
                    field == &controlPrimitives.send1) cachedSend = dispatchCache;
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
                                            if (t == &controlPrimitives.send0 or
                                                t == &controlPrimitives.send1) lp += 4;
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
    m.setLiterals(&[_]Object{Sym.value}, &[_]Object{Object.from(42)}, null);
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
    std.debug.assert(last == objects.len);
    return obj;
}
test "compileObject" {
    const expectEqual = std.testing.expectEqual;
    const expect = std.testing.expect;
    const c = ClassIndex;
    var o = compileObject(.{
        "def",
        True,
        ":first",
        c.Method, // first HeapObject

        ":second",
        c.replace0, // second HeapObject - runtime ClassIndex #0
        "first", // pointer to first object
        "1mref", // reference to replacement Object #1
        Sym.i_1, // alternate reference to replacement Object #1
        "second", // pointer to second object
        ":def",
        c.Class, // third HeapObject
    });
    std.debug.print("\nhere",.{});
    o.setLiterals(&[_]Object{ Nil, True }, &[_]ClassIndex{@enumFromInt(0xdead)});
    try expect(o.asObject().isHeapObject());
    try expect(o.objects[0].equals(o.asObject()));
    try expectEqual(@as(u48, @truncate(o.asObject().rawU())), @as(u48, @truncate(@intFromPtr(&o.objects[8]))));
    try expect(o.objects[5].equals(True));
    try expect(o.objects[6].equals(True));
    const h2: HeapObjectConstPtr = @ptrFromInt(@as(u64,@bitCast(o.objects[2])));
    try expectEqual(h2.header.classIndex, c.Method);
    try expectEqual(h2.header.length, 2);
    try expectEqual(h2.header.age, .static);
    try expectEqual(h2.header.format, .notIndexableWithPointers);
    const h3: HeapObjectConstPtr = @ptrFromInt(@as(u64,@bitCast(o.objects[3])));
    try expectEqual(@intFromEnum(h3.header.classIndex), 0xdead);
    try expectEqual(h3.header.length, 0);
    const header: HeapObjectConstPtr = @ptrCast(&o.objects[8]);
    const h8 = header.*;
    try expectEqual(h8.header.length, 4);
    try expect(!header.header.isIndexable());
    try expect(header.header.isStatic());
    try expect(header.header.isUnmoving());
}
test "method object" {
    // + aNumber
    //     "Primitive. Add the receiver to the argument and answer with the result
    //     if it is a SmallInteger. Fail if the argument or the result is not a
    //     SmallInteger  Essential  No Lookup. See Object documentation whatIsAPrimitive."

    //     <primitive: 1>
    //     ^ super + aNumber
    const c = ClassIndex;
    var o = compileObject(.{
        ":super1",
        c.ASSuper,

        0,
        ":aNumber",
        c.ASArg,

        "super1",
        Sym.@"+",
        "aNumber",
        ":send1",
        c.ASSend,

        "send1",
        ":return1",
        c.ASReturn,

        "return1",
        // another statement
        ":body",
        c.ASSequence,

        "body",
        1,
        Sym.@"+",
        ":first",
        c.Method, // first HeapObject
    });
    o.setLiterals(&[_]Object{}, &[_]ClassIndex{});
    const method = o.asObject();
    _ = method;
}

pub const controlPrimitives = struct {
    const ContextPtr = CodeContextPtr;
    pub inline fn checkSpace(pc: PC, sp: SP, process: *Process, context: ContextPtr, needed: usize) void {
        _ = process;
        _ = pc;
        _ = context;
        _ = sp;
        _ = needed;
    }
    pub fn verifySelector(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const method = pc.compiledMethodPtr(1); // must be first word in method, pc already bumped
        trace("\nverifySelector: 0x{x} 0x{x} {}", .{ method.selector.rawU(), selector.rawU(), pc });
        if (!method.selector.selectorEquals(selector)) {
            const dPc = cache.current();
            return @call(tailCall, dPc.prim(), .{ dPc.next(), sp, process, context, selector, cache.next() });
        }
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector, cache });
    }
    pub fn verifyDirectSelector(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const method = (&pc[0]).compiledMethodPtr(1); // must be first word in method, pc already bumped
        trace("\nverifyDirectSelector: {} {} {}", .{ method.selector, selector, pc });
        if (!method.selector.selectorEquals(selector)) {
            const dPc = cache.current();
            return @call(tailCall, dPc.prim, .{ dPc + 1, sp, process, context, selector, cache.next() });
        }
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector, cache });
    }
    pub fn branch(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const offset = pc.int();
        trace("\nbranch offset: {}", .{offset});
        if (offset >= 0) {
            const target = pc.skip(@as(u64, @intCast(offset)) + 1);
            if (process.needsCheck()) return @call(tailCall, check, .{ target, sp, process, context, selector, cache });
            return @call(tailCall, target.prim(), .{ target.next(), sp, process, context, selector, cache });
        }
        const target = pc.next().back(@as(u64, @intCast(-offset)));
        if (process.needsCheck()) return @call(tailCall, check, .{ target, sp, process, context, selector, cache });
        return @call(tailCall, target.prim(), .{ target.next(), sp, process.decCheck(), context, selector, cache });
    }
    pub fn ifTrue(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        trace("\nifTrue: {any}", .{context.stack(sp, process)});
        const v = sp.top;
        if (True.equals(v)) return @call(tailCall, branch, .{ pc, sp.drop(), process, context, selector, cache });
        if (False.equals(v)) return @call(tailCall, pc.prim2(), .{ pc.next2(), sp.drop(), process, context, selector, cache });
        @panic("non boolean");
    }
    pub fn ifFalse(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        trace("\nifFalse: {any}", .{context.stack(sp, process)});
        const v = sp.top;
        if (False.equals(v)) return @call(tailCall, branch, .{ pc, sp.drop(), process, context, selector, cache });
        if (True.equals(v)) return @call(tailCall, pc.next().prim(), .{ pc.skip(2), sp.drop(), process, context, selector, cache });
        @panic("non boolean");
    }
    pub fn ifNil(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const v = sp[0];
        if (Nil.equals(v)) return @call(tailCall, branch, .{ pc, sp.pop(), process, context, selector, cache });
        return @call(tailCall, pc.next().prim, .{ pc.skip(2), sp.pop(), process, context, selector, cache });
    }
    pub fn ifNotNil(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const v = sp[0];
        if (Nil.equals(v)) return @call(tailCall, pc.next().prim, .{ pc.skip(2), sp.pop(), process, context, selector, cache });
        return @call(tailCall, branch, .{ pc, sp.pop(), process, context, selector, cache });
    }
    pub fn primFailure(_: PC, _: SP, _: *Process, _: ContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
        @panic("primFailure");
    }
    pub fn dup(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const newSp = sp.push(sp.top);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, selector, cache });
    }
    pub fn over(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const newSp = sp.push(sp.next);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, selector, cache });
    }
    pub fn drop(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        return @call(tailCall, pc.prim(), .{ pc.next(), sp.drop(), process, context, selector, cache });
    }
    pub fn dropNext(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        trace("\ndropNext: {}", .{sp.top});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp.dropPut(sp.top), process, context, selector, cache });
    }
    pub fn swap(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const saved = sp.top;
        sp.top = sp.next;
        sp.next = saved;
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector, cache });
    }
    pub fn replaceLiteral(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        sp.top = pc[0].object;
        trace("\nreplaceLiteral: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.next().prim, .{ pc.skip(2), sp, process, context, selector, cache });
    }
    pub fn replaceLiteral0(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        sp.top = Object.from(0);
        trace("\nreplaceLiteral0: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector, cache });
    }
    pub fn replaceLiteral1(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        sp.top = Object.from(1);
        trace("\nreplaceLiteral0: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector, cache });
    }
    pub fn pushLiteral(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const newSp = sp.push(pc.object());
        trace("\npushLiteral: {any}", .{context.stack(newSp, process)});
        return @call(tailCall, pc.next().prim(), .{ pc.skip(2), newSp, process, context, selector, cache });
    }
    pub fn pushLiteral0(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const newSp = sp.push(Object.from(0));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, selector, cache });
    }
    pub fn pushLiteral1(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const newSp = sp.push(Object.from(1));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, selector, cache });
    }
    pub fn pushLiteral2(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const newSp = sp.push(Object.from(2));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, selector, cache });
    }
    pub fn pushLiteral_1(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const newSp = sp.push(Object.from(-1));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, selector, cache });
    }
    pub fn pushLiteralIndirect(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const newSp = sp.push(pc.literalIndirect());
        return @call(tailCall, pc.prim2(), .{ pc.next2(), newSp, process, context, selector, cache });
    }
    pub fn pushLiteralNil(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const newSp = sp.push(Nil);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, selector, cache });
    }
    pub fn pushLiteralTrue(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const newSp = sp.push(True);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, selector, cache });
    }
    pub fn pushLiteralFalse(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const newSp = sp.push(False);
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, selector, cache });
    }
    pub fn pushThisContext(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const newSp = sp.push(Object.from(context));
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, selector, cache });
    }
    pub fn pushLocal(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const newSp = sp.push(context.getLocal(pc.uint()));
        trace("\npushLocal: {any} {any}", .{ context.stack(newSp, process), context.allLocals(process) });
        return @call(tailCall, pc.next().prim(), .{ pc.skip(2), newSp, process, context, selector, cache });
    }
    pub fn pushLocal0(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const newSp = sp.push(context.getLocal(0));
        trace("\npushLocal0: {any}", .{context.stack(newSp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), newSp, process, context, selector, cache });
    }
    pub fn popLocal0(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        context.setLocal(0, sp.top);
        return @call(tailCall, pc.prim(), .{ pc.next(), sp + 1, process, context, selector, cache });
    }
    pub fn storeLocal(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        trace("\nstoreIntoLocal: {} {}", .{ pc.uint, sp.top });
        context.setLocal(pc.uint, sp.top);
        return @call(tailCall, pc.next().prim, .{ pc.skip(2), sp, process, context, selector, cache });
    }
    pub fn pushLocalField(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const ref = pc.uint;
        const local = context.getLocal(ref & 0xff);
        const newSp = sp.push(local.getField(ref >> 12));
        trace("\npushLocalField: {} {} {any} {any}", .{ ref, local, context.stack(newSp, process), context.allLocals(process) });
        return @call(tailCall, pc.next().prim, .{ pc.skip(2), newSp, process, context, selector, cache });
    }
    pub fn popLocalField(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const ref = pc.uint;
        const local = context.getLocal(ref & 0xfff);
        trace("\npopLocalField: {} {}", .{ ref, sp.top });
        local.setField(ref >> 12, sp.top);
        return @call(tailCall, pc.next().prim, .{ pc.skip(2), sp + 1, process, context, selector, cache });
    }
    pub fn pushLocalData(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const ref = pc.uint;
        const local = context.getLocal(ref & 0xfff);
        const newSp = sp.push(local - (ref >> 12));
        trace("\npushLocalData: {} {} {any} {any}", .{ ref, local, context.stack(newSp, process), context.allLocals(process) });
        return @call(tailCall, pc.next().prim, .{ pc.skip(2), newSp, process, context, selector, cache });
    }
    pub fn popLocalData(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const ref = pc.uint;
        const local = context.getLocal(ref & 0xff);
        trace("\npopLocalData: {} {}", .{ ref, sp.top });
        local.setData(ref >> 12, sp.top);
        return @call(tailCall, pc.next().prim, .{ pc.skip(2), sp + 1, process, context, selector, cache });
    }
    pub fn popLocal(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        trace("\npopIntoLocal: {} {}", .{ pc.uint(), sp.top });
        context.setLocal(pc.uint(), sp.top);
        return @call(tailCall, pc.next().prim(), .{ pc.skip(2), sp.drop(), process, context, selector, cache });
    }
    pub fn printStack(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        trace("\nstack: {any}", .{context.stack(sp, process)});
        return @call(tailCall, pc.prim(), .{ pc.next(), sp, process, context, selector, cache });
    }
    pub fn primitiveFailed(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        _ = .{ pc, sp, process, context, selector, cache, @panic("primitiveFailed") };
    }
    pub fn fallback(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const self = sp.at(selector.numArgs());
        context.setReturn(pc);
        const class = self.get_class();
        const newPc = lookupAddress(selector, class);
        trace("\nfallback: {} {} {} {} {}", .{ selector, class, pc, newPc, newPc.prim() });
        return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, selector, cache });
    }
    pub fn call(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        context.setReturn(pc.next());
        const offset = pc.uint();
        const method = pc.skip(offset + 1).object().to(CompiledMethodPtr);
        const newPc = PC.init(method.codePtr());
        return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, selector, cache });
    }
    pub fn callRecursive(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        context.setReturn(pc.next());
        const offset = pc.int();
        const newPc = pc.next().back(@intCast(-offset));
        trace("\ncallRecursive: {any}", .{context.stack(sp, process)});
        return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, selector, cache });
    }
    pub fn send0(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, prevCache: SendCache) callconv(stdCall) SP {
        const self = sp.top;
        context.setReturn(pc.next().skip(sendCacheSize));
        const class = self.get_class();
        const selector = pc.object().withClass(class);
        trace("\nsend0: {} {}", .{ selector, class });
        const cache = if (dispatchCache) @as(SendCache, @constCast(@ptrCast(pc + 1))) else prevCache;
        const newPc = if (dispatchCache) cache.current() else lookupAddress(selector, class);
        trace(" {} {any}", .{ newPc, process.getStack(sp) });
        return @call(tailCall, newPc.*.prim(), .{ newPc.*.next(), sp, process, context, selector, if (dispatchCache) cache.next() else prevCache });
    }
    //    pub fn tailSend0(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, cache: SendCache) callconv(stdCall) SP {
    //    }
    pub fn send1(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, prevCache: SendCache) callconv(stdCall) SP {
        const self = sp.next;
        context.setReturn(pc.next().skip(sendCacheSize));
        const class = self.get_class();
        const selector = pc.object().withClass(class);
        trace("\nsend1: {} {}", .{ selector, class });
        const cache = if (dispatchCache) @as(SendCache, @constCast(@ptrCast(pc + 1))) else prevCache;
        const newPc = if (dispatchCache) cache.current() else lookupAddress(selector, class);
        trace(" {} {any}", .{ newPc, process.getStack(sp) });
        return @call(tailCall, newPc.*.prim(), .{ newPc.*.next(), sp, process, context, selector, if (dispatchCache) cache.next() else prevCache });
    }
    //    pub fn tailSend1(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, cache: SendCache) callconv(stdCall) SP {
    //    }
    pub fn perform(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, cache: SendCache) callconv(stdCall) SP {
        const selector = sp.top;
        const numArgs = selector.numArgs();
        if (numArgs != 0) @panic("wrong number of args");
        const newPc = lookupAddress(selector, sp[numArgs + 1].get_class());
        context.setReturn(pc);
        return @call(tailCall, newPc.prim(), .{ newPc.next(), sp + 1, process, context, selector, cache });
    }
    pub fn performWith(pc: PC, sp: SP, process: *Process, context: ContextPtr, _: Object, cache: SendCache) callconv(stdCall) SP {
        const selector = sp.next;
        sp.next = sp.top;
        if (selector.numArgs() != 1) @panic("wrong number of args");
        const newPc = lookupAddress(selector, sp[2].get_class());
        context.setTPc(pc + 1);
        return @call(tailCall, newPc.prim(), .{ newPc.next(), sp + 1, process, context, selector, cache });
    }
    pub fn pushContext(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        const method = @as(CompiledMethodPtr, @ptrFromInt(@intFromPtr(pc.back(pc.uint()).asCodePtr()) - CompiledMethod.codeOffset));
        const stackStructure = method.stackStructure;
        const locals = stackStructure.low16() & 255;
        const maxStackNeeded = stackStructure.mid16();
        const selfOffset = stackStructure.high16();
        trace("\npushContext: locals={} maxStack={} selfOffset={} selector={}", .{ locals, maxStackNeeded, selfOffset, method.selector });
        const ctxt = context.push(sp, process, method, locals, maxStackNeeded, selfOffset);
        const newSp = ctxt.asNewSp();
        trace("\npushContext: {any} {} {} {} 0x{x} 0x{x}", .{ process.getStack(sp), locals, method.selector, selfOffset, @intFromPtr(ctxt), @intFromPtr(sp) });
        return @call(tailCall, pc.prim2(), .{ pc.next2(), newSp, process, ctxt, selector, cache });
    }
    pub fn returnWithContext(_: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        trace("\nreturnWithContext: {any} -> ", .{context.stack(sp, process)});
        const result = context.pop(process);
        const newSp = result.sp;
        var callerContext = result.ctxt;
        const stack = callerContext.stack(newSp, process);
        if (stack.len < 20) {
            trace("{any}", .{stack});
        } else trace("{}", .{stack.len});
        trace("\nrWC: sp={*} newSp={*}\n", .{ sp, newSp });
        return @call(tailCall, callerContext.getNPc(), .{ callerContext.getTPc(), newSp, process, @constCast(callerContext), selector, cache });
    }
    pub fn returnTop(_: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        trace("\nreturnTop: {any} ", .{context.stack(sp, process)});
        const top = sp.top;
        const result = context.pop(process);
        const newSp = result.sp;
        newSp.top = top;
        const callerContext = result.ctxt;
        trace("-> {x}", .{@intFromPtr(newSp)});
        trace("-> {any}", .{callerContext.stack(newSp, process)});
        return @call(tailCall, callerContext.getNPc(), .{ callerContext.getTPc(), newSp, process, @constCast(callerContext), selector, cache });
    }
    pub fn returnNoContext(_: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        trace("\nreturnNoContext: {} {any} N={} T={}", .{ context.method.selector, context.stack(sp, process), context.getNPc(), context.getTPc() });
        return @call(tailCall, context.getNPc(), .{ context.getTPc(), sp, process, context, selector, cache });
    }
    pub fn forceDnu(pc: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        std.debug.print("\nforceDnu: 0x{x} {} {} {}", .{ selector.hash32(), selector.classIndex, selector.asSymbol(), cache.fromDnu() });
        _ = .{ pc, sp, process, context, selector, cache, @panic("forceDnu unimplemented") };
    }
    fn hardDnu(_: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        trace("\nhardDnu: {} {}", .{ selector.classIndex, selector.asSymbol() });
        const newPc = lookupAddress(selector, selector.classIndex);
        return @call(tailCall, newPc.prim(), .{ newPc.*.next(), sp, process, context, selector, cache });
    }
    fn cacheDnu(_: PC, sp: SP, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) callconv(stdCall) SP {
        trace("\ncacheDnu: 0x{x} {} {}", .{ selector.u(), selector.classIndex, selector.asSymbol() });
        const newPc = lookupAddress(selector, selector.classIndex);
        const newCache = cache.previous();
        newCache.cache1 = newPc;
        trace("\ncacheDnu: {} {*} {*}", .{ newCache, newCache, cache });
        return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, selector, cache });
        //        const pc = cache.current();
        //        return @call(tailCall, pc.prim(), .{ pc+1, sp, process, context, selector, cache.next() });
    }
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
        var cache = SendCacheStruct.init();
        self.initStack(source);
        self.ctxt.setReturn(Code.endThread);
        if (@TypeOf(trace) == @TypeOf(std.debug.print) and trace == std.debug.print) method.write(stdout) catch unreachable;
        //        trace("\nrun: {} {*}",.{cache.dontCache(),cache.dontCache().current()});
        return self.stack(method.execute(self.sp, &self.process, &self.ctxt, cache.dontCache()));
    }
};
const p = struct {
    usingnamespace controlPrimitives;
};
fn push42(_: PC, sp: SP, _: *Process, _: CodeContextPtr, _: Object, _: SendCache) callconv(stdCall) SP {
    const newSp = sp.push(Object.from(42));
    return newSp;
}
test "SendCache direct" {
    if (dispatchCache) {
        const expectEqual = std.testing.expectEqual;
        var method = compileMethod(Sym.yourself, 0, 0, .{
            &p.send0,           Sym.value,
            &p.primitiveFailed,
        });
        const myDnu = Code.prim(&push42);
        var cache = SendCacheStruct.initWith(&myDnu);
        //    _ = cache;    method.setLiterals(empty, empty, null);
        trace("\ncache: {}", .{cache});
        trace("\nmethod:< {}", .{method});
        method.setLiterals(empty, empty, &cache);
        trace("\nmethod:> {}", .{method});
        var te = Execution.new();
        te.init();
        var objs = [_]Object{ Nil, True };
        const compiledMethod = method.asCompiledMethodPtr();
        const result = te.run(objs[0..], compiledMethod);
        try expectEqual(result.len, 3);
        try expectEqual(result[0], Object.from(42));
    } else std.debug.print("dispatch cache not enabled - succeeds vacuously ", .{});
}
test "send with dispatch direct" {
    const expectEqual = std.testing.expectEqual;
    const method = compileMethod(Sym.yourself, 0, 0, .{
        &p.send0,           Sym.value,
        &p.primitiveFailed,
    });
    const methodV = compileMethod(Sym.value, 0, 0, .{
        &push42,
        &p.primitiveFailed,
    });
    dispatch.init();
    methodV.asCompiledMethodPtr().forDispatch(ClassIndex.UndefinedObject);
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
    method1.setLiterals(empty, &[_]Object{Object.from(&method2)}, null);
    var te = Execution.new();
    te.init();
    var objs = [_]Object{ Nil, True };
    const result = te.run(objs[0..], &method1);
    try expectEqual(result.len, 2);
    try expectEqual(result[0], Object.from(42));
}
test "context returnTop with indirect via Execution" {
    const expectEqual = std.testing.expectEqual;
    var method = compileMethod(Sym.yourself, 3, 0, .{
        //        &p.noop,
        &p.pushContext,
        "^",
        &p.pushLiteralIndirect,
        "0Obj",
        &p.returnTop,
    });
    method.setLiterals(empty, &[_]Object{Object.from(42)}, null);
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
