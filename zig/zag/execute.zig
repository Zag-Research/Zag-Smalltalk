const std = @import("std");
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
pub const lookup = @import("dispatch.zig").lookup;
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
pub const tailCall: std.builtin.CallModifier = .never_inline;// .always_tail
const noInlineCall: std.builtin.CallModifier = .never_inline;
pub const MethodReturns = [*]Object;

pub fn check(pc: [*]const Code, sp: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) [*]Object {
    if (process.debugger()) |debugger|
        return @call(tailCall, debugger, .{ pc, sp, process, context, selector });
    return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector });
}

pub const ThreadedFn = *const fn (programCounter: [*]const Code, stackPointer: [*]Object, process: *Process, context: CodeContextPtr, selector: Object) MethodReturns;
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
    pub fn execute(self: *Self, sp: [*]Object, process: *Process, context: CodeContextPtr) [*]Object {
        const pc = self.codePtr();
        //        std.debug.print("execute [{*}]: {*} {}\n",.{pc,pc[0].prim,sp[0]});
        //        return @call(tailCall,pc[0].prim,.{pc+1,sp,process,context,self.selector});
        return pc[0].prim(pc + 1, sp, process, context, self.selector);
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
        const selfOffset = self.stackStructure.classIndex;
        try writer.print("\n**** {} {} {}",.{codeOffset / 8, all.len, refs.len});
        try writer.print("\n** {} {}",.{self.header, realHO});
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
    inline fn ref(comptime u: u24) Code {
        return Code{ .object = indexSymbol(u) };
    }
    inline fn header(h: heap.HeapObject) Code {
        return Code{ .header = h };
    }
    pub inline fn codeRef(c: [*]const Code) Code {
        return Code{ .codeRef = @constCast(c) };
    }
    pub fn end(_: [*]const Code, sp: [*]Object, _: *Process, _: *Context, _: Object) [*]Object {
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
pub const CountSizes = struct { codes: usize, refs: usize = 0, objects: usize = 0 };
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
            comptime_int, comptime_float => {
                c += 1;
            },
            else => switch (@typeInfo(@TypeOf(field))) {
                .Pointer => |pointer| {
                    switch (@typeInfo(pointer.child)) {
                        .Fn => c += 1,
                        else => {
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
    const expectEqual = std.testing.expectEqual;
    const r1 = countNonLabels(.{
        ":abc",
        &p.dnu,
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
}

pub fn CompileTimeMethod(comptime counts: CountSizes) type {
    const codeSize = counts.codes;
    const refsSize = counts.refs;
    return extern struct { // structure must exactly match CompiledMethod
        header: HeapObject,
        stackStructure: Object,
        selector: Object,
        code: [codeSize]Code,
        references: [refsSize]Object,
        footer: HeapObject,
        const codeOffsetInUnits = CompiledMethod.codeOffsetInUnits;
        const Self = @This();
        // comptime {
        //     if (checkEqual(CompiledMethod.codeOffset,@offsetOf(Self,"code"))) |s|
        //         @compileError("CompiledMethod prefix not the same as CompileTimeMethod == " ++ s);
        // }
        pub fn init(name: Object, locals: u16, maxStack: u16) Self {
            const footer = HeapObject.calcHeapObject(ClassIndex.CompiledMethod, codeOffsetInUnits + codeSize, name.hash24(), Age.static, refsSize, @sizeOf(Object), false) catch @compileError("too many refs");
            //  @compileLog(codeSize,refsSize,footer,heap.Format.allocationInfo(5,null,0,false));
            //  trace("\nfooter={}",.{footer});
            return .{
                .header = footer.asHeader(),
                .selector = name,
                .stackStructure = Object.packedInt(locals, maxStack, locals + name.numArgs()),
                .code = undefined,
                .references = [_]Object{object.NotAnObject} ** refsSize,
                .footer = footer,
            };
        }
        pub fn checkFooter(self: *Self) void {
            trace("\ncheckFooter: {}\n    header={}\n    footer={}\n     allocInfo={}\n   a1={x}\n   a2={x}",.{self.selector,self.header,self.footer,Format.allocationInfo(codeOffsetInUnits + codeSize, refsSize, @sizeOf(Object), false),@intFromPtr(self),@intFromPtr(self.header.realHeapObject())});
        }
        pub fn withCode(name: Object, locals: u16, maxStack: u16, code: [codeSize]Code) Self {
            const footer = HeapObject.calcHeapObject(ClassIndex.CompiledMethod, codeOffsetInUnits + codeSize, name.hash24(), Age.static, refsSize, @sizeOf(Object), false) catch @compileError("too many refs");
            return .{
                .header = footer.asHeader(),
                .selector = name,
                .stackStructure = Object.packedInt(locals, maxStack, locals + name.numArgs()),
                .code = code,
                .references = [_]Object{object.NotAnObject} ** refsSize,
                .footer = footer,
            };
        }
        pub fn asCompiledMethodPtr(self: *const Self) *CompiledMethod {
            return @as(*CompiledMethod, @ptrCast(@constCast(self)));
        }
        pub fn asFakeObject(self: *const Self) Object {
            return @as(Object, @bitCast(self));
        }
        pub fn setLiterals(self: *Self, replacements: []const Object, refs: []const Object) void {
            for (replacements, 1..) |replacement, index| {
                const match = indexSymbol(@as(u24, @truncate(index)));
                if (self.selector.equals(match)) {
                    self.stackStructure.classIndex = @enumFromInt(@intFromEnum(self.stackStructure.classIndex)-(match.numArgs()-replacement.numArgs()));
                    self.selector = replacement;
                }
                for (&self.code) |*c| {
                    if (c.object.equals(match))
                        c.* = Code.object(replacement);
                }
            }
            for (refs, self.references[0..refs.len]) |obj, *srefs|
                srefs.* = obj;
            if (self.references.len > 0) {
                for (&self.code) |*c| {
                    if (c.object.isIndexSymbol())
                        c.* = Code.uint((@intFromPtr(&self.references[c.object.hash24() & (Code.refFlag - 1)]) - @intFromPtr(c)) / @sizeOf(Object) - 1);
                }
            }
        }
        pub fn getCodeSize(_: *Self) usize {
            return codeSize;
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
        &p.dnu,
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
    r1.setLiterals(Object.empty, &r1r);
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
    inline for (tup) |field| {
        switch (@TypeOf(field)) {
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
                                    code[n] = Code.ref(intOf(field[0..]) + Code.refFlag);
                                    n += 1;
                                    found = true;
                                } else {
                                    comptime var lp = 0;
                                    inline for (tup) |t| {
                                        if (@TypeOf(t) == ThreadedFn) lp = lp + 1 else switch (@typeInfo(@TypeOf(t))) {
                                            .Pointer => |tPointer| {
                                                switch (@typeInfo(tPointer.child)) {
                                                    .Array => {
                                                        if (t[0] == ':') {
                                                            if (comptime std.mem.endsWith(u8, t, field)) {
                                                                code[n] = Code.int(lp - n - 1);
                                                                n = n + 1;
                                                                found = true;
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
    var m = compileMethod(Sym.yourself, 0, 0, .{ ":abc", &p.dnu, "def", True, comptime Object.from(42), ":def", "abc", "*", "^", 3, "0mref", null });
    const mcmp = m.asCompiledMethodPtr();
    m.setLiterals(Object.empty, &[_]Object{Object.from(mcmp)});
    var t = m.code[0..];
    //    for (t,0..) |tv,idx|
    //        trace("\nt[{}]: 0x{x:0>16}",.{idx,tv.uint});
    //    try expectEqual(t[0].prim,controlPrimitives.noop);
    try expectEqual(t[0].prim, p.dnu);
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
pub const trace = std.debug.print;
//pub inline fn trace(_: anytype, _: anytype) void {}
pub const controlPrimitives = struct {
    const ContextPtr = CodeContextPtr;
    pub inline fn checkSpace(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, needed: usize) void {
        _ = process;
        _ = pc;
        _ = context;
        _ = sp;
        _ = needed;
    }
    pub fn verifySelector(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const method = (&pc[0]).compiledMethodPtr(1); // must be first word in method, pc already bumped
        trace("\nverifySelector: {} {} {*}", .{ method.selector, selector, pc });
        if (!method.selector.hashEquals(selector)) return @call(tailCall, dnu, .{ pc, sp, process, context, selector });
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp, process, context, selector });
    }
    pub fn branch(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const offset = pc[0].int;
        trace("\nbranch offset: {}\n", .{offset});
        if (offset >= 0) {
            const target = pc + 1 + @as(u64, @intCast(offset));
            if (process.needsCheck()) return @call(tailCall, check, .{ target, sp, process, context, selector });
            trace("\nbranch target: {}", .{target[0].uint});
            return @call(tailCall, target[0].prim, .{ target + 1, sp, process, context, selector });
        }
        const target = pc + 1 - @as(u64, @intCast(-offset));
        if (process.needsCheck()) return @call(tailCall, check, .{ target, sp, process, context, selector });
        return @call(tailCall, target[0].prim, .{ target + 1, sp, process.decCheck(), context, selector });
    }
    pub fn ifTrue(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const v = sp[0];
        if (True.equals(v)) return @call(tailCall, branch, .{ pc, sp + 1, process, context, selector });
        if (False.equals(v)) return @call(tailCall, pc[1].prim, .{ pc + 2, sp + 1, process, context, selector });
        @panic("non boolean");
    }
    pub fn ifFalse(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        trace("\nifFalse: {any}", .{context.stack(sp, process)});
        const v = sp[0];
        if (False.equals(v)) return @call(tailCall, branch, .{ pc, sp + 1, process, context, selector });
        if (True.equals(v)) return @call(tailCall, pc[1].prim, .{ pc + 2, sp + 1, process, context, selector });
        @panic("non boolean");
    }
    pub fn ifNil(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const v = sp[0];
        if (Nil.equals(v)) return @call(tailCall, branch, .{ pc, sp + 1, process, context, selector });
        return @call(tailCall, pc[1].prim, .{ pc + 2, sp + 1, process, context, selector });
    }
    pub fn ifNotNil(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const v = sp[0];
        if (Nil.equals(v)) return @call(tailCall, pc[1].prim, .{ pc + 2, sp + 1, process, context, selector });
        return @call(tailCall, branch, .{ pc, sp + 1, process, context, selector });
    }
    pub fn primFailure(_: [*]const Code, _: [*]Object, _: *Process, _: ContextPtr, _: Object) [*]Object {
        @panic("primFailure");
    }
    pub fn dup(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp - 1;
        newSp[0] = newSp[1];
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector });
    }
    pub fn over(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp - 1;
        newSp[0] = newSp[2];
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector });
    }
    pub fn drop(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, selector });
    }
    pub fn pushLiteral(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp - 1;
        newSp[0] = pc[0].object;
        trace("\npushLiteral: {}", .{newSp[0]});
        return @call(tailCall, pc[1].prim, .{ pc + 2, newSp, process, context, selector });
    }
    pub fn pushLiteral0(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp - 1;
        newSp[0] = Object.from(0);
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector });
    }
    pub fn pushLiteral1(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp - 1;
        newSp[0] = Object.from(1);
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector });
    }
    pub fn pushLiteral2(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp - 1;
        newSp[0] = Object.from(2);
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector });
    }
    pub fn pushLiteral_1(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp - 1;
        newSp[0] = Object.from(-1);
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector });
    }
    pub fn pushLiteralIndirect(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp - 1;
        newSp[0] = @as(*const Code, @ptrCast(pc)).literalIndirect();
        return @call(tailCall, pc[1].prim, .{ pc + 2, newSp, process, context, selector });
    }
    pub fn pushLiteralNil(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp - 1;
        newSp[0] = Nil;
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector });
    }
    pub fn pushLiteralTrue(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp - 1;
        newSp[0] = True;
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector });
    }
    pub fn pushLiteralFalse(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp - 1;
        newSp[0] = False;
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector });
    }
    pub fn pushThisContext(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp - 1;
        newSp[0] = Object.from(context);
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector });
    }
    pub fn pushLocal(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp - 1;
        newSp[0] = context.getLocal(pc[0].uint);
        trace("\npushLocal: {} {any} {any}", .{ pc[0].uint, context.stack(newSp, process), context.allLocals(process) });
        return @call(tailCall, pc[1].prim, .{ pc + 2, newSp, process, context, selector });
    }
    pub fn pushLocal0(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const newSp = sp - 1;
        newSp[0] = context.getLocal(0);
        trace("\npushLocal: {any} {*}", .{ context.stack(newSp, process), pc });
        return @call(tailCall, pc[0].prim, .{ pc + 1, newSp, process, context, selector });
    }
    pub fn pushLocalField(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const ref = pc[0].uint;
        const local = context.getLocal(ref & 0xfff);
        const newSp = sp - 1;
        newSp[0] = local.getField(ref >> 12);
        trace("\npushLocalField: {} {} {any} {any}", .{ ref, local, context.stack(newSp, process), context.allLocals(process) });
        return @call(tailCall, pc[1].prim, .{ pc + 2, newSp, process, context, selector });
    }
    pub fn popLocalField(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const ref = pc[0].uint;
        const local = context.getLocal(ref & 0xfff);
        trace("\npopLocalField: {} {}", .{ ref, sp[0] });
        local.setField(ref >> 12, sp[0]);
        return @call(tailCall, pc[1].prim, .{ pc + 2, sp + 1, process, context, selector });
    }
    pub fn popLocal(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        trace("\npopIntoLocal: {} {}", .{ pc[0].uint, sp[0] });
        context.setLocal(pc[0].uint, sp[0]);
        return @call(tailCall, pc[1].prim, .{ pc + 2, sp + 1, process, context, selector });
    }
    pub fn popLocal0(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        context.setLocal(0, sp[0]);
        return @call(tailCall, pc[0].prim, .{ pc + 1, sp + 1, process, context, selector });
    }
    pub fn storeLocal(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        trace("\nstoreIntoLocal: {} {}", .{ pc[0].uint, sp[0] });
        context.setLocal(pc[0].uint, sp[0]);
        return @call(tailCall, pc[1].prim, .{ pc + 2, sp, process, context, selector });
    }
    pub fn primitiveFailed(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        _ = .{ pc, sp, process, context, selector };
        @panic("primitiveFailed");
    }
    pub fn fallback(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const arity = selector.numArgs();
        const newPc = lookup(selector, sp[arity].get_class());
        context.setReturn(pc);
        std.debug.print("\nin fallback {} {} {*} {}\n", .{ selector, sp[arity].get_class(), newPc, newPc[0] });
        return @call(tailCall, newPc[0].prim, .{ newPc + 1, sp, process, context, selector });
    }
    pub fn send0(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, _: Object) [*]Object {
        const selector = pc[0].object;
        const newPc = lookup(selector, sp[0].get_class());
        context.setReturn(pc + 1);
        trace("\nin send0 {} {} {any}\n", .{ selector, sp[0].get_class(), process.getStack(sp) });
        return @call(tailCall, newPc[0].prim, .{ newPc + 1, sp, process, context, selector });
    }
    pub fn send1(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, _: Object) [*]Object {
        const selector = pc[0].object;
        const newPc = lookup(selector, sp[1].get_class());
        trace("\nsend1: {} {} {} {x}",.{selector,sp[1],sp[1].get_class(),@intFromPtr(newPc)});
        context.setReturn(pc + 1);
        return @call(tailCall, newPc[0].prim, .{ newPc + 1, sp, process, context, selector });
    }
    pub fn perform(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, _: Object) [*]Object {
        const selector = sp[0];
        const numArgs = selector.numArgs();
        if (numArgs != 0) @panic("wrong number of args");
        const newPc = lookup(selector, sp[numArgs+1].get_class());
        context.setReturn(pc);
        return @call(tailCall, newPc[0].prim, .{ newPc + 1, sp + 1, process, context, selector });
    }
    pub fn performWith(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, _: Object) [*]Object {
        const selector = sp[1];
        sp[1] = sp[0];
        if (selector.numArgs() != 1) @panic("wrong number of args");
        const newPc = lookup(selector, sp[2].get_class());
        context.setTPc(pc + 1);
        return @call(tailCall, newPc[0].prim, .{ newPc + 1, sp + 1, process, context, selector });
    }
    pub fn call(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        context.setReturn(pc + 1);
        const offset = pc[0].uint;
        const method = pc[1 + offset].object.to(CompiledMethodPtr);
        const newPc = method.codePtr();
        return @call(tailCall, newPc[0].prim, .{ newPc + 1, sp, process, context, selector });
    }
    pub fn callRecursive(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        context.setReturn(pc + 1);
        const offset = pc[0].int;
        const newPc = pc + 1 - @as(u64, @intCast(-offset));
        trace("\ncallRecursive: {any}", .{context.stack(sp, process)});
        return @call(tailCall, newPc[0].prim, .{ newPc + 1, sp, process, context, selector });
    }
    pub fn pushContext(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        const method = @as(CompiledMethodPtr, @ptrFromInt(@intFromPtr(pc - pc[0].uint) - CompiledMethod.codeOffset));
        const stackStructure = method.stackStructure;
        const locals = stackStructure.low16() & 255;
        const maxStackNeeded = stackStructure.mid16();
        const selfOffset = stackStructure.high16();
        const ctxt = context.push(sp, process, method, locals, maxStackNeeded, selfOffset);
        const newSp = ctxt.asObjectPtr();
        trace("\npC: sp={*} newSp={*} {} {*}", .{ sp, newSp, pc[0].uint, method });
        trace("\npushContext: {any} {} {} {}", .{ process.getStack(sp), locals, method.selector, selfOffset });
        return @call(tailCall, pc[1].prim, .{ pc + 2, newSp, process, ctxt, selector });
    }
    pub fn returnWithContext(_: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        trace("\nreturnWithContext: {any} -> ", .{context.stack(sp, process)});
        const result = context.pop(process);
        const newSp = result.sp;
        var callerContext = result.ctxt;
        trace("{any}", .{callerContext.stack(newSp, process)});
        trace("\nrWC: sp={*} newSp={*}\n", .{ sp, newSp });
        return @call(tailCall, callerContext.getNPc(), .{ callerContext.getTPc(), newSp, process, @constCast(callerContext), selector });
    }
    pub fn returnTop(_: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        trace("\nreturnTop: {any} ", .{context.stack(sp, process)});
        const top = sp[0];
        var result = context.pop(process);
        const newSp = result.sp;
        newSp[0] = top;
        var callerContext = result.ctxt;
        trace("-> {any}", .{callerContext.stack(newSp, process)});
        return @call(tailCall, callerContext.getNPc(), .{ callerContext.getTPc(), newSp, process, @constCast(callerContext), selector });
    }
    pub fn returnNoContext(_: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        trace("\nreturnNoContext: {} {any} N={*} T={*}", .{ context.method.selector, context.stack(sp, process), context.getNPc(), context.getTPc() });
        return @call(tailCall, context.getNPc(), .{ context.getTPc(), sp, process, context, selector });
    }
    pub fn dnu(pc: [*]const Code, sp: [*]Object, process: *Process, context: ContextPtr, selector: Object) [*]Object {
        _ = .{ pc, sp, process, context, selector };
        unreachable;
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
    var yourself = CompiledMethod.init(Sym.yourself,Code.end);
    pub fn run(self: *Self, source: []const Object, method: CompiledMethodPtr) []Object {
        self.initStack(source);
        self.ctxt.setReturn(Code.endThread);
        if (@TypeOf(trace) == @TypeOf(std.debug.print) and trace == std.debug.print) method.write(stdout) catch unreachable;
        return self.stack(method.execute(self.sp, &self.process, &self.ctxt));
    }
};
const p = struct {
    usingnamespace controlPrimitives;
};
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
    method1.setLiterals(Object.empty, &[_]Object{Object.from(method2.asCompiledMethodPtr())});
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
    method.setLiterals(Object.empty, &[_]Object{Object.from(42)});
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
    trace("result = {any}\n", .{result});
    try expectEqual(result.len, 1);
    try expectEqual(result[0], Object.from(0));
}
