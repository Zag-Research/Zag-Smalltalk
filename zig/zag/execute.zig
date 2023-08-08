const std = @import("std");
const config = @import("config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const dispatchCache = config.dispatchCache;
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
const indexSymbol = object.indexSymbol;
const dispatch = @import("dispatch.zig");
const lookup = dispatch.lookup;
pub const Context = @import("context.zig").Context;
//const TestExecution = @import("context.zig").TestExecution;
const heap = @import("heap.zig");
const HeapObject = heap.HeapObject;
const HeapObjectPtr = heap.HeapObjectPtr;
const HeapObjectConstPtr = heap.HeapObjectConstPtr;
const Format = heap.Format;
const Age = heap.Age;
//const class = @import("class.zig");
const Sym = @import("symbol.zig").symbols;

pub const SendCache = *SendCacheStruct;
pub const sendCacheSize = if (dispatchCache) @sizeOf(SendCacheStruct)/@sizeOf(Code) else 0;
pub const SendCacheStruct = extern struct {
    cache1: [*]const Code,
    cache2: [*]const Code,
    noCache: [*]const Code,
    dnu: [*]const Code,
    const Self = @This();
    pub fn init() Self {
        return initWith(&dnus[0]);
    }
    pub fn initWith(fn1: *const Code) Self {
        return .{
            .cache1 = @ptrCast(fn1),
            .cache2 = @ptrCast(fn1),
            .noCache = @ptrCast(&dnus[1]),
            .dnu = @ptrCast(&dnus[2]),
        };
    }
    pub fn current(self: *Self) [*]const Code { // INLINE
        return self.cache1;
    }
    pub inline fn next(self: *Self) SendCache {
        trace("\nnext: {}",.{self});
        return @ptrCast(&self.cache2);
    }
    pub inline fn fromDnu(self: *Self) SendCache {
        return @fieldParentPtr(SendCacheStruct,"dnu",@as(*[*]const Code,@ptrCast(self))).previous();
    }
    pub inline fn dontCache(self: *Self) SendCache { // don't use on a SendCache that is the result of `next`
        return @ptrCast(&self.dnu);
    }
    pub inline fn previous(self: *Self) SendCache {
        return @ptrCast(@as([*][*]const Code,@ptrCast(self)) - 1);
    }
};
const dnus = [_]Code{
    Code.prim(&controlPrimitives.cacheDnu),
    Code.prim(&controlPrimitives.hardDnu),
    Code.prim(&controlPrimitives.forceDnu),
};
test "SendCache" {
    const expectEqual = std.testing.expectEqual;
    var cache = SendCacheStruct.init();
    try expectEqual(cache.current()[0].prim,&controlPrimitives.cacheDnu);
    try expectEqual(cache.dontCache().current()[0].prim,&controlPrimitives.forceDnu);
    try expectEqual(cache.next().next().current()[0].prim,&controlPrimitives.hardDnu);
    try expectEqual(cache.next().next().next().current()[0].prim,&controlPrimitives.forceDnu);
}

pub fn check(pc: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object {
    if (process.debugger()) |debugger|
        return @call(tailCall, debugger, .{ pc, sp, process, context, selector, cache });
    return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
}

pub const ThreadedFn = *const fn (programCounter: [*]const Code, stackPointer: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object;
const TFn = fn (programCounter: [*]const Code, stackPointer: [*]Object, process: *Process, context: CodeContextPtr, selector: Object, cache: SendCache) [*]Object;
pub const fallback = controlPrimitives.fallback;
pub const CodeContextPtr = *Context;
pub const CompiledMethodPtr = *CompiledMethod;
pub const CompiledMethod = extern struct {
    header: HeapObject,
    stackStructure: Object, // number of local values beyond the parameters
    selector: Object, // must be the word before code
    code: [codeSize]Code, // will typically be a lot more then 2, as it will be the processed version of the method
    //references: [n]Object,
    footer: HeapObject,
    const Self = @This();
    const codeSize = 2;
    pub const codeOffset = @offsetOf(CompiledMethod, "code");
    const codeOffsetInUnits = codeOffset / 8;
    pub fn init(name: Object, methodFn: ThreadedFn) Self {
        return init2(name, methodFn, Code.end);
    }
    pub fn init2(name: Object, methodFn: ThreadedFn, methodFn2: ThreadedFn) Self {
        const footer = HeapObject.calcHeapObject(ClassIndex.CompiledMethod, codeOffsetInUnits + codeSize, name.hash24(), Age.static, null, 0, false) catch unreachable;
        return Self{
            .header = footer.asHeader(),
            .selector = name,
            .stackStructure = Object.from(0),
            .code = [2]Code{ Code.prim(methodFn), Code.prim(methodFn2) },
            .footer = footer,
        };
    }
    pub fn execute(self: *Self, sp: [*]Object, process: *Process, context: CodeContextPtr, cache: SendCache) [*]Object {
        const pc = self.codePtr();
        trace("\nexecute: [{*}]: {*} {} {}",.{ pc, pc[0]. prim,sp[0], self.selector });
        //        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,self.selector});
        return pc[0].prim(pc + 1, sp, process, context, self.selector, cache);
    }
    pub fn forDispatch(self: *Self,class: ClassIndex) void {
        if (dispatchCache) self.selector = self.selector.withClass(class);
        dispatch.addMethod(class,self) catch @panic("addMethod failed");
    }
    inline fn asHeapObjectPtr(self: *const Self) HeapObjectConstPtr {
        return @as(HeapObjectConstPtr, @ptrCast(self));
    }
    pub fn checkFooter(self: *Self) void {
        trace("\ncheckFooter*: {}\n    header={}\n    footer={}\n  *footer={}\n   a1={x}\n   a2={x}\n   a3={x}",.{self.selector,self.header,self.footer,self.header.realHeapObject(),@intFromPtr(self),@intFromPtr(self.header.realHeapObject()),@intFromPtr(&self.footer)});
    }
    pub inline fn matchedSelector(self: *Self, selector: Object) bool {
        return selector.hashEquals(self.selector);
    }
    pub inline fn codePtr(self: *Self) [*]Code {
        return @as([*]Code, @ptrCast(&self.code));
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
        const realHO = self.header.realHeapObject();
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
pub const Code = extern union {
    prim: ThreadedFn,
    int: i64,
    uint: u64,
    object: Object,
    header: heap.HeapObject,
    codeRef: [*]Code,
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
    inline fn ref(comptime u: u12) Code {
        return Code{ .object = indexSymbol(u) };
    }
    inline fn header(h: heap.HeapObject) Code {
        return Code{ .header = h };
    }
    pub inline fn codeRef(c: [*]const Code) Code {
        return Code{ .codeRef = @constCast(c) };
    }
    pub fn end(_: [*]const Code, sp: [*]Object, _: *Process, _: *Context, _: Object, _: SendCache) [*]Object {
        return sp;
    }
    pub const endThread = &[_]Code{.{ .prim = &end }};
    inline fn compiledMethodX(self: *const Code) *const CompiledMethod {
        return @as(*const CompiledMethod, @ptrCast(self));
    }
    pub inline fn compiledMethodPtr(self: *const Code, comptime index: comptime_int) *const CompiledMethod {
        return @fieldParentPtr(CompiledMethod, "code", @as(*const [2]Code, @ptrCast(@as([*]const Code, @ptrCast(self)) - index)));
    }
    pub inline fn literalIndirect(self: *const Code) Object {
        const offset = self.uint;
        return @as(*const Object, @ptrCast(@as([*]const Code, @ptrCast(self)) + 1 + offset)).*;
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
pub fn intOf(comptime str: []const u8) u16 {
    var n: u16 = 0;
    for (str) |c| {
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
                        field == &controlPrimitives.send1
                    ) d += 1;
                c += 1;
            },
            comptime_int, comptime_float => {
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
    });
    try expectEqual(r1.codes, 10);
    try expectEqual(r1.refs, 2);
    try expectEqual(r1.objects, 3);
    try expectEqual(r1.caches, 1);
}

pub fn CompileTimeMethod(comptime counts: CountSizes) type {
    const codes = counts.codes;
    const refs = counts.refs;
    const caches = counts.caches;
    return extern struct { // structure must exactly match CompiledMethod
        header: HeapObject,
        stackStructure: Object,
        selector: Object,
        code: [codes + caches*sendCacheSize]Code,
        references: [refs]Object,
        footer: HeapObject,
        const codeOffsetInUnits = CompiledMethod.codeOffsetInUnits;
        const Self = @This();
        // comptime {
        //     if (checkEqual(CompiledMethod.codeOffset,@offsetOf(Self,"code"))) |s|
        //         @compileError("CompiledMethod prefix not the same as CompileTimeMethod == " ++ s);
        // }
        const cacheSize = @sizeOf(SendCacheStruct)/@sizeOf(Code);
        pub fn init(name: Object, locals: u16, maxStack: u16) Self {
            const footer = HeapObject.calcHeapObject(ClassIndex.CompiledMethod, codeOffsetInUnits + codes + caches*cacheSize, name.hash24(), Age.static, refs, @sizeOf(Object), false) catch @compileError("too many refs");
            //  @compileLog(codes,refs,footer,heap.Format.allocationInfo(5,null,0,false));
            //  trace("\nfooter={}",.{footer});
            return .{
                .header = footer.asHeader(),
                .selector = name,
                .stackStructure = Object.packedInt(locals, maxStack, locals + name.numArgs()),
                .code = undefined,
                .references = [_]Object{object.NotAnObject} ** refs,
                .footer = footer,
            };
        }
        pub fn checkFooter(self: *Self) void {
            trace("\ncheckFooter: {}\n    header={}\n    footer={}\n     allocInfo={}\n   a1={x}\n   a2={x}",.{self.selector,self.header,self.footer,Format.allocationInfo(codeOffsetInUnits + codes, refs, @sizeOf(Object), false),@intFromPtr(self),@intFromPtr(self.header.realHeapObject())});
        }
        pub fn withCode(name: Object, locals: u16, maxStack: u16, code: [codes]Code) Self {
            const footer = HeapObject.calcHeapObject(ClassIndex.CompiledMethod, codeOffsetInUnits + codes + caches*cacheSize, name.hash24(), Age.static, refs, @sizeOf(Object), false) catch @compileError("too many refs");
            return .{
                .header = footer.asHeader(),
                .selector = name,
                .stackStructure = Object.packedInt(locals, maxStack, locals + name.numArgs()),
                .code = code,
                .references = [_]Object{object.NotAnObject} ** refs,
                .footer = footer,
            };
        }
        fn cacheOffset(_: *Self,codeOffs: usize, cacheOffs: usize) u32 {
            return @truncate((codes-codeOffs) + refs + (cacheOffs*cacheSize));
        }
        pub fn asCompiledMethodPtr(self: *const Self) *CompiledMethod {
            return @as(*CompiledMethod, @ptrCast(@constCast(self)));
        }
        pub fn asFakeObject(self: *const Self) Object {
            return @as(Object, @bitCast(self));
        }
        pub fn setLiterals(self: *Self, replacements: []const Object, refReplacements: []const Object, cache: ?SendCache) void {
            trace("\nsetLiterals: 0x{x:0>16} {any}",.{self.selector.u(), replacements});
            var cachedSend = false;
            for (&self.code) |*c| {
                if (dispatchCache and (c.prim == &controlPrimitives.send0
                                             or c.prim == &controlPrimitives.send1
                                         )) {
                    cachedSend = true;
                } else {
                    if (c.object.isIndexSymbol()) {
                        for (replacements, 1..) |replacement, index| {
                            const match = indexSymbol(@truncate(index));
                            if (match.indexEquals(c.object)) {
                                c.* = Code.object(replacement);
                                break;
                            }
                        }
                    }
                    if (dispatchCache and cachedSend) {
                        c.* = Code.object(c.object);
                        @as(SendCache,@ptrCast(@as([*]Code,@ptrCast(c))+1)).* = if (cache) |aSendCache| aSendCache.* else SendCacheStruct.init();
                        cachedSend = false;
                    }
                }
            }
            for (replacements, 1..) |replacement, index| {
                const match = indexSymbol(@truncate(index));
                if (match.indexEquals(self.selector)) {
                    self.stackStructure.classIndex = @enumFromInt(@intFromEnum(self.stackStructure.classIndex)-(match.numArgs()-replacement.numArgs()));
                    self.selector = replacement;
                    break;
                }
            }
            for (refReplacements, &self.references) |obj, *srefs|
                srefs.* = obj;
            if (self.references.len > 0) {
                for (&self.code) |*c| {
                    if (c.object.isIndexSymbol()) {
                        const newValue = (@intFromPtr(&self.references[c.object.indexNumber() & (Code.refFlag - 1)]) - @intFromPtr(c)) / @sizeOf(Object) - 1;
                        c.* = Code.uint(newValue);
                    }
                }
            }
            trace("-> 0x{x:0>16}",.{self.selector.u()});
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
    var r1 = c1.init(Nil, 2, 3);
    var r1r = [_]Object{ Nil, True };
    r1.setLiterals(Object.empty, &r1r, null);
    try expectEqual(r1.getCodeSize(), 10);
}
pub fn compiledMethodType(comptime codeSize: comptime_int) type {
    return CompileTimeMethod(.{ .codes = codeSize });
}
pub fn compileMethod(name: Object, comptime locals: comptime_int, comptime maxStack: comptime_int, comptime tup: anytype) CompileTimeMethod(countNonLabels(tup)) {
    @setEvalBranchQuota(20000);
    const methodType = CompileTimeMethod(countNonLabels(tup));
    var method = methodType.init(name, locals, maxStack);
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
                    code[n+1] = dnus[0];
                    code[n+2] = dnus[1];
                    code[n+3] = dnus[2];
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
                        field == &controlPrimitives.send1
                    ) cachedSend = dispatchCache;
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
                                    code[n] = Code.ref(intOf(field[0..]) + Code.refFlag);
                                    n += 1;
                                    found = true;
                                } else {
                                    comptime var lp = 0;
                                    inline for (tup) |t| {
                                        if (@TypeOf(t) == ThreadedFn) {
                                            lp += 1;
                                            if (t == &controlPrimitives.send0 or
                                                    t == &controlPrimitives.send1
                                                ) lp += 4;
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
const stdout = std.io.getStdOut().writer();
const print = std.io.getStdOut().writer().print;
test "compiling method" {
    const expectEqual = std.testing.expectEqual;
    var m = compileMethod(Sym.yourself, 0, 0, .{ ":abc", &p.hardDnu, "def", True, comptime Object.from(42), ":def", "abc", "*", "^", 3, "0mref", null });
    const mcmp = m.asCompiledMethodPtr();
    m.setLiterals(Object.empty, &[_]Object{Object.from(mcmp)}, null);
    var t = m.code[0..];
    //    for (t,0..) |tv,idx|
    //        trace("\nt[{}]: 0x{x:0>16}",.{idx,tv.uint});
    //    try expectEqual(t[0].prim,controlPrimitives.noop);
    try expectEqual(t[0].prim, p.hardDnu);
    try expectEqual(t[1].int, 2);
    try expectEqual(t[2].object, True);
    try expectEqual(t[3].object, Object.from(42));
    try expectEqual(t[4].int, -5);
    try expectEqual(t[5].int, -1);
    try expectEqual(t[6].int, 6);
    try expectEqual(t[7].int, 3);
    //   try expectEqual(t[8].int,1+Code.refFlag);
    try expectEqual(t[9].object, Nil);
    try expectEqual(t.len, 10);
}
pub const controlPrimitives = struct {
    const ContextPtr = CodeContextPtr;
    pub inline fn checkSpace(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, needed: usize) void {
        _ = process;
        _ = pc;
        _ = context;
        _ = sp;
        _ = needed;
    }
    pub fn verifySelector(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const method = (&pc[0]).compiledMethodPtr(1); // must be first word in method, pc already bumped
        trace("\nverifySelector: 0x{x} 0x{x} {*}", .{ method.selector.u(), selector.u(), pc });
        if (!method.selector.selectorEquals(selector)) {
            const dPc = cache.current();
            return @call(tailCall, dPc[0].prim, .{ dPc+1, sp, process, context, selector, cache.next() });
        }
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    pub fn verifyDirectSelector(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const method = (&pc[0]).compiledMethodPtr(1); // must be first word in method, pc already bumped
        trace("\nverifyDirectSelector: {} {} {*}", .{ method.selector, selector, pc });
        if (!method.selector.selectorEquals(selector)) {
            const dPc = cache.current();
            return @call(tailCall, dPc[0].prim, .{ dPc+1, sp, process, context, selector, cache.next() });
        }
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    pub fn branch(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const offset = pc[0].int;
        trace("\nbranch offset: {}\n", .{offset});
        if (offset >= 0) {
            const target = pc + 1 + @as(u64, @intCast(offset));
            if (process.needsCheck()) return @call(tailCall, check, .{ target, sp, process, context, selector, cache });
            trace("\nbranch target: {}", .{target[0].uint});
            return @call(tailCall, target[0].prim, .{ target + 1, sp, process, context, selector, cache });
        }
        const target = pc + 1 - @as(u64, @intCast(-offset));
        if (process.needsCheck()) return @call(tailCall, check, .{ target, sp, process, context, selector, cache });
        return @call(tailCall, target[0].prim, .{ target + 1, sp, process.decCheck(), context, selector, cache });
    }
    pub fn ifTrue(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const v = sp[0];
        if (True.equals(v)) return @call(tailCall, branch, .{ pc, sp + 1, process, context, selector, cache });
        if (False.equals(v)) return @call(tailCall, pc[1].prim, .{ pc + 2, sp + 1, process, context, selector, cache });
        @panic("non boolean");
    }
    pub fn ifFalse(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        trace("\nifFalse: {any}", .{context.stack(sp, process)});
        const v = sp[0];
        if (False.equals(v)) return @call(tailCall, branch, .{ pc, sp + 1, process, context, selector, cache });
        if (True.equals(v)) return @call(tailCall, pc[1].prim, .{ pc + 2, sp + 1, process, context, selector, cache });
        @panic("non boolean");
    }
    pub fn ifNil(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const v = sp[0];
        if (Nil.equals(v)) return @call(tailCall, branch, .{ pc, sp + 1, process, context, selector, cache });
        return @call(tailCall, pc[1].prim, .{ pc + 2, sp + 1, process, context, selector, cache });
    }
    pub fn ifNotNil(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const v = sp[0];
        if (Nil.equals(v)) return @call(tailCall, pc[1].prim, .{ pc + 2, sp + 1, process, context, selector, cache });
        return @call(tailCall, branch, .{ pc, sp + 1, process, context, selector, cache });
    }
    pub fn primFailure(_: [*]const Code, _: [*]Object, _: *Process, _: ContextPtr, _: Object, _: SendCache) [*]Object {
        @panic("primFailure");
    }
    pub fn dup(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const newSp = sp - 1;
        newSp[0] = newSp[1];
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector, cache });
    }
    pub fn over(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const newSp = sp - 1;
        newSp[0] = newSp[2];
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector, cache });
    }
    pub fn drop(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, selector, cache });
    }
    pub fn swap(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const saved = sp[0];
        sp[0] = sp[1];
        sp[1] = saved;
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector, cache });
    }
    pub fn pushLiteral(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const newSp = sp - 1;
        newSp[0] = pc[0].object;
        trace("\npushLiteral: {any}", .{context.stack(newSp, process)});
        return @call(tailCall, pc[1].prim, .{ pc + 2, newSp, process, context, selector, cache });
    }
    pub fn pushLiteral0(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const newSp = sp - 1;
        newSp[0] = Object.from(0);
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector, cache });
    }
    pub fn pushLiteral1(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const newSp = sp - 1;
        newSp[0] = Object.from(1);
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector, cache });
    }
    pub fn pushLiteral2(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const newSp = sp - 1;
        newSp[0] = Object.from(2);
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector, cache });
    }
    pub fn pushLiteral_1(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const newSp = sp - 1;
        newSp[0] = Object.from(-1);
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector, cache });
    }
    pub fn pushLiteralIndirect(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const newSp = sp - 1;
        newSp[0] = @as(*const Code, @ptrCast(pc)).literalIndirect();
        return @call(tailCall, pc[1].prim, .{ pc + 2, newSp, process, context, selector, cache });
    }
    pub fn pushLiteralNil(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const newSp = sp - 1;
        newSp[0] = Nil;
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector, cache });
    }
    pub fn pushLiteralTrue(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const newSp = sp - 1;
        newSp[0] = True;
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector, cache });
    }
    pub fn pushLiteralFalse(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const newSp = sp - 1;
        newSp[0] = False;
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector, cache });
    }
    pub fn pushThisContext(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const newSp = sp - 1;
        newSp[0] = Object.from(context);
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector, cache });
    }
    pub fn pushLocal(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const newSp = sp - 1;
        newSp[0] = context.getLocal(pc[0].uint);
        trace("\npushLocal: {any} {any}", .{ context.stack(newSp, process), context.allLocals(process) });
        return @call(tailCall, pc[1].prim, .{ pc + 2, newSp, process, context, selector, cache });
    }
    pub fn pushLocal0(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const newSp = sp - 1;
        newSp[0] = context.getLocal(0);
        trace("\npushLocal0: {any}", .{ context.stack(newSp, process) });
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector, cache });
    }
    pub fn popLocal0(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        context.setLocal(0, sp[0]);
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, selector, cache });
    }
    pub fn storeLocal(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        trace("\nstoreIntoLocal: {} {}", .{ pc[0].uint, sp[0] });
        context.setLocal(pc[0].uint, sp[0]);
        return @call(tailCall, pc[1].prim, .{ pc + 2, sp, process, context, selector, cache });
    }
    pub fn pushLocalField(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const ref = pc[0].uint;
        const local = context.getLocal(ref & 0xff);
        const newSp = sp - 1;
        newSp[0] = local.getField(ref >> 12);
        trace("\npushLocalField: {} {} {any} {any}", .{ ref, local, context.stack(newSp, process), context.allLocals(process) });
        return @call(tailCall, pc[1].prim, .{ pc + 2, newSp, process, context, selector, cache });
    }
    pub fn popLocalField(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const ref = pc[0].uint;
        const local = context.getLocal(ref & 0xfff);
        trace("\npopLocalField: {} {}", .{ ref, sp[0] });
        local.setField(ref >> 12, sp[0]);
        return @call(tailCall, pc[1].prim, .{ pc + 2, sp + 1, process, context, selector, cache });
    }
    pub fn pushLocalData(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const ref = pc[0].uint;
        const local = context.getLocal(ref & 0xfff);
        const newSp = sp - 1;
        newSp[0] = local - (ref >> 12);
        trace("\npushLocalData: {} {} {any} {any}", .{ ref, local, context.stack(newSp, process), context.allLocals(process) });
        return @call(tailCall, pc[1].prim, .{ pc + 2, newSp, process, context, selector, cache });
    }
    pub fn popLocalData(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const ref = pc[0].uint;
        const local = context.getLocal(ref & 0xff);
        trace("\npopLocalData: {} {}", .{ ref, sp[0] });
        local.setData(ref >> 12, sp[0]);
        return @call(tailCall, pc[1].prim, .{ pc + 2, sp + 1, process, context, selector, cache });
    }
    pub fn popLocal(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        trace("\npopIntoLocal: {} {}", .{ pc[0].uint, sp[0] });
        context.setLocal(pc[0].uint, sp[0]);
        return @call(tailCall, pc[1].prim, .{ pc + 2, sp + 1, process, context, selector, cache });
    }
    pub fn primitiveFailed(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        _ = .{ pc, sp, process, context, selector, cache, @panic("primitiveFailed")};
    }
    pub fn fallback(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const self = sp[selector.numArgs()];
        context.setReturn(pc);
        const class = self.get_class();
        const newPc = lookup(selector, class);
        std.debug.print("\nin fallback {} {} {*} {}\n", .{ selector, class, newPc, newPc[0] });
        return @call(tailCall, newPc[0].prim, .{ newPc + 1, sp, process, context, selector, cache });
    }
    pub fn call(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        context.setReturn(pc + 1);
        const offset = pc[0].uint;
        const method = pc[1 + offset].object.to(CompiledMethodPtr);
        const newPc = method.codePtr();
        return @call(tailCall, newPc[0].prim, .{ newPc + 1, sp, process, context, selector, cache });
    }
    pub fn callRecursive(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        context.setReturn(pc + 1);
        const offset = pc[0].int;
        const newPc = pc + 1 - @as(u64, @intCast(-offset));
        trace("\ncallRecursive: {any}", .{context.stack(sp, process)});
        return @call(tailCall, newPc[0].prim, .{ newPc + 1, sp, process, context, selector, cache });
    }
    pub fn send0(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, _: Object, prevCache: SendCache) [*]Object {
        const self = sp[0];
        context.setReturn(pc + 1 + sendCacheSize);
        const class = self.get_class();
        const selector = pc[0].object.withClass(class);
        trace("\nsend0: 0x{x} {}",.{ selector.u(), class });
        const cache = if (dispatchCache) @as(SendCache,@constCast(@ptrCast(pc+1))) else prevCache;
        const newPc = if (dispatchCache) cache.current() else lookup(selector, class);
        trace(" {*} {any}",.{ newPc, process.getStack(sp) });
        return @call(tailCall, newPc[0].prim, .{ newPc + 1, sp, process, context, selector, if (dispatchCache) cache.next() else prevCache });
      }
//    pub fn tailSend0(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, _: Object, cache: SendCache) [*]Object {
//    }
    pub fn send1(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, _: Object, prevCache: SendCache) [*]Object {
        const self = sp[1];
        context.setReturn(pc + 1 + sendCacheSize);
        const class = self.get_class();
        const selector = pc[0].object.withClass(class);
        trace("\nsend1: 0x{x} {}",.{ selector.u(), class });
        const cache = if (dispatchCache) @as(SendCache,@constCast(@ptrCast(pc+1))) else prevCache;
        const newPc = if (dispatchCache) cache.current() else lookup(selector, class);
        trace(" {*} {any}",.{ newPc, process.getStack(sp) });
        return @call(tailCall, newPc[0].prim, .{ newPc + 1, sp, process, context, selector, if (dispatchCache) cache.next() else prevCache });
    }
//    pub fn tailSend1(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, _: Object, cache: SendCache) [*]Object {
//    }
    pub fn perform(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, _: Object, cache: SendCache) [*]Object {
        const selector = sp[0];
        const numArgs = selector.numArgs();
        if (numArgs != 0) @panic("wrong number of args");
        const newPc = lookup(selector, sp[numArgs+1].get_class());
        context.setReturn(pc);
        return @call(tailCall, newPc[0].prim, .{ newPc + 1, sp + 1, process, context, selector, cache });
    }
    pub fn performWith(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, _: Object, cache: SendCache) [*]Object {
        const selector = sp[1];
        sp[1] = sp[0];
        if (selector.numArgs() != 1) @panic("wrong number of args");
        const newPc = lookup(selector, sp[2].get_class());
        context.setTPc(pc + 1);
        return @call(tailCall, newPc[0].prim, .{ newPc + 1, sp + 1, process, context, selector, cache });
    }
    pub fn pushContext(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        const method = @as(CompiledMethodPtr, @ptrFromInt(@intFromPtr(pc - pc[0].uint) - CompiledMethod.codeOffset));
        const stackStructure = method.stackStructure;
        const locals = stackStructure.low16() & 255;
        const maxStackNeeded = stackStructure.mid16();
        const selfOffset = stackStructure.high16();
        const ctxt = context.push(sp, process, method, locals, maxStackNeeded, selfOffset);
        const newSp = ctxt.asObjectPtr();
        trace("\npushContext: {any} {} {} {} 0x{x}", .{ process.getStack(sp), locals, method.selector, selfOffset, @intFromPtr(ctxt) });
        return @call(tailCall, pc[1].prim, .{ pc + 2, newSp, process, ctxt, selector, cache });
    }
    pub fn returnWithContext(_: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        trace("\nreturnWithContext: {any} -> ", .{context.stack(sp, process)});
        const result = context.pop(process);
        const newSp = result.sp;
        var callerContext = result.ctxt;
        const stack = callerContext.stack(newSp, process);
        if (stack.len<20) {
            trace("{any}", .{stack});
        } else
            trace("{}", .{stack.len});
        trace("\nrWC: sp={*} newSp={*}\n", .{ sp, newSp });
        return @call(tailCall, callerContext.getNPc(), .{ callerContext.getTPc(), newSp, process, @constCast(callerContext), selector, cache });
    }
    pub fn returnTop(_: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        trace("\nreturnTop: {any} ", .{context.stack(sp, process)});
        const top = sp[0];
        var result = context.pop(process);
        const newSp = result.sp;
        newSp[0] = top;
        const callerContext = result.ctxt;
        trace("-> {any}", .{callerContext.stack(newSp, process)});
        return @call(tailCall, callerContext.getNPc(), .{ callerContext.getTPc(), newSp, process, @constCast(callerContext), selector, cache });
    }
    pub fn returnNoContext(_: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        trace("\nreturnNoContext: {} {any} N={*} T={*}", .{ context.method.selector, context.stack(sp, process), context.getNPc(), context.getTPc() });
        return @call(tailCall, context.getNPc(), .{ context.getTPc(), sp, process, context, selector, cache });
    }
    pub fn forceDnu(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        std.debug.print("\nforceDnu: 0x{x} {} {} {}",.{selector.u(),selector.classIndex,selector.asSymbol(), cache.fromDnu()});
        _ = .{ pc, sp, process, context, selector, cache, @panic("forceDnu unimplemented")};
    }
    fn hardDnu(_: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        trace("\nhardDnu: {} {}",.{selector.classIndex,selector.asSymbol()});
        const newPc = lookup(selector, selector.classIndex);
        return @call(tailCall, newPc[0].prim, .{ newPc + 1, sp, process, context, selector, cache });
    }
    fn cacheDnu(_: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object, cache: SendCache) [*]Object {
        trace("\ncacheDnu: 0x{x} {} {}",.{selector.u(),selector.classIndex,selector.asSymbol()});
        const newPc = lookup(selector, selector.classIndex);
        const newCache = cache.previous();
        newCache.cache1 = newPc;
        trace("\ncacheDnu: {} {*} {*}",.{newCache, newCache, cache});
        return @call(tailCall, newPc[0].prim, .{ newPc + 1, sp, process, context, selector, cache });
//        const pc = cache.current();
//        return @call(tailCall, pc[0].prim, .{ pc+1, sp, process, context, selector, cache.next() });
    }
};
pub const TestExecution = struct {
    process: Process,
    ctxt: Context,
    sp: [*]Object,
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
    var yourself = CompiledMethod.init(Sym.noFallback,Code.end);
    pub fn initStack(self: *Self, source: []const Object) void {
        self.sp = self.process.endOfStack() - source.len;
        for (source, self.sp[0..source.len]) |src, *dst|
            dst.* = src;
        trace("\ninitial stack: {x} {x}",.{@intFromPtr(self.sp),@intFromPtr(self.process.endOfStack())});
    }
    pub fn stack(self: *Self, sp: [*]Object) []Object {
        self.sp = sp;
        trace("\nfinal stack: {x} {x}",.{@intFromPtr(sp),@intFromPtr(self.process.endOfStack())});
        return self.ctxt.stack(self.sp, &self.process);
    }
    pub fn run(self: *Self, source: []const Object, method: CompiledMethodPtr) []Object {
        var cache = SendCacheStruct.init();
        self.initStack(source);
        self.ctxt.setReturn(Code.endThread);
        if (@TypeOf(trace) == @TypeOf(std.debug.print) and trace == std.debug.print) method.write(stdout) catch unreachable;
        trace("\nrun: {} {*}",.{cache.dontCache(),cache.dontCache().current()});
        return self.stack(method.execute(self.sp, &self.process, &self.ctxt, cache.dontCache()));
    }
};
const p = struct {
    usingnamespace controlPrimitives;
};
fn push42(_: [*]const Code, sp: [*]Object, _: *Process, _: CodeContextPtr, _: Object, _: SendCache) [*]Object {
    const newSp = sp - 1;
    newSp[0] = Object.from(42);
    return newSp;
}
test "SendCache direct" {
    if (dispatchCache) {
        const expectEqual = std.testing.expectEqual;
        var method = compileMethod(Sym.yourself, 0, 0, .{
            &p.send0,  Sym.value,
            &p.primitiveFailed,
        });
        const myDnu = Code.prim(&push42);
        var cache = SendCacheStruct.initWith(&myDnu);
        //    _ = cache;    method.setLiterals(Object.empty, Object.empty, null);
        trace("\ncache: {}",.{cache});
        trace("\nmethod:< {}",.{method});
        method.setLiterals(Object.empty, Object.empty, &cache);
        trace("\nmethod:> {}",.{method});
        var te = TestExecution.new();
        te.init();
        var objs = [_]Object{ Nil, True };
        const compiledMethod = method.asCompiledMethodPtr();
        var result = te.run(objs[0..], compiledMethod);
        try expectEqual(result.len, 3);
        try expectEqual(result[0], Object.from(42));
    } else
        std.debug.print("dispatch cache not enabled - succeeds vacuously ",.{});
}
test "send with dispatch direct" {
    const expectEqual = std.testing.expectEqual;
    var method = compileMethod(Sym.yourself, 0, 0, .{
        &p.send0,  Sym.value,
        &p.primitiveFailed,
    });
    var methodV = compileMethod(Sym.value, 0, 0, .{
        &push42,
        &p.primitiveFailed,
    });
    method.setLiterals(Object.empty, Object.empty, null);
    dispatch.init();
    methodV.asCompiledMethodPtr().forDispatch(ClassIndex.UndefinedObject);
    var te = TestExecution.new();
    te.init();
    var objs = [_]Object{ Nil, True };
    const compiledMethod = method.asCompiledMethodPtr();
    var result = te.run(objs[0..], compiledMethod);
    try expectEqual(result.len, 3);
    try expectEqual(result[0], Object.from(42));
}
test "simple return via TestExecution" {
    const expectEqual = std.testing.expectEqual;
    var method = compileMethod(Sym.yourself, 0, 0, .{
        &p.pushLiteral,     comptime Object.from(42),
        &p.returnNoContext,
    });
    var te = TestExecution.new();
    te.init();
    var objs = [_]Object{ Nil, True };
    const compiledMethod = method.asCompiledMethodPtr();
    var result = te.run(objs[0..], compiledMethod);
    try expectEqual(result.len, 3);
    try expectEqual(result[0], Object.from(42));
    try expectEqual(result[1], Nil);
    try expectEqual(result[2], True);
}
test "context return via TestExecution" {
    const expectEqual = std.testing.expectEqual;
    var method = compileMethod(Sym.@"at:", 0, 0, .{
        &p.pushContext,       "^",
        &p.pushLiteral,       comptime Object.from(42),
        &p.returnWithContext,
    });
    var te = TestExecution.new();
    te.init();
    var objs = [_]Object{ Nil, True };
    const compiledMethod = method.asCompiledMethodPtr();
    trace("\nmethod: {*}", .{compiledMethod});
    var result = te.run(objs[0..], compiledMethod);
    try expectEqual(result.len, 1);
    try expectEqual(result[0], True);
}
test "context returnTop via TestExecution" {
    const expectEqual = std.testing.expectEqual;
    var method = compileMethod(Sym.yourself, 3, 0, .{
        &p.pushContext, "^",
        &p.pushLiteral, comptime Object.from(42),
        &p.returnTop,
    });
    var te = TestExecution.new();
    te.init();
    var objs = [_]Object{ Nil, True };
    const compiledMethod = method.asCompiledMethodPtr();
    var result = te.run(objs[0..], compiledMethod);
    try expectEqual(result.len, 2);
    try expectEqual(result[0], Object.from(42));
}
test "context returnTop twice via TestExecution" {
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
    method1.setLiterals(Object.empty, &[_]Object{Object.from(method2.asCompiledMethodPtr())}, null);
    var te = TestExecution.new();
    te.init();
    var objs = [_]Object{ Nil, True };
    const compiledMethod = method1.asCompiledMethodPtr();
    var result = te.run(objs[0..], compiledMethod);
    try expectEqual(result.len, 2);
    try expectEqual(result[0], Object.from(42));
}
test "context returnTop with indirect via TestExecution" {
    const expectEqual = std.testing.expectEqual;
    var method = compileMethod(Sym.yourself, 3, 0, .{
        //        &p.noop,
        &p.pushContext,
        "^",
        &p.pushLiteralIndirect,
        "0Obj",
        &p.returnTop,
    });
    method.setLiterals(Object.empty, &[_]Object{Object.from(42)}, null);
    var te = TestExecution.new();
    te.init();
    var objs = [_]Object{ Nil, True };
    const compiledMethod = method.asCompiledMethodPtr();
    var result = te.run(objs[0..], compiledMethod);
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
    var te = TestExecution.new();
    te.init();
    const result = te.run(objs[0..], method.asCompiledMethodPtr());
    try expectEqual(result.len, 1);
    try expectEqual(result[0], Object.from(0));
}
