const std = @import("std");
const expectEqualSlices = std.testing.expectEqualSlices;
const assert = std.debug.assert;
const zag = @import("zag.zig");
const config = zag.config;
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const Object = zag.object.Object;
const Nil = zag.object.Nil;
const True = zag.object.True;
const False = zag.object.False;
const execute = zag.execute;
const ThreadedFn = execute.ThreadedFn;
const PC = execute.PC;
const SP = execute.SP;
const Execution = execute.Execution;
const Process = zag.Process;
const Context = zag.Context;
const Extra = execute.Extra;
const Result = execute.Result;
const stringOf = zag.heap.CompileTimeString;
const tf = zag.threadedFn.Enum;
const Sym = zag.symbol.symbols;

const modules = [_]Module{
    Module.init(testModule),
    Module.init(@import("primitives/Object.zig")),
    Module.init(@import("primitives/Smallinteger.zig")),
    // Module.init(@import("primitives/Behavior.zig").module),
    Module.init(@import("primitives/BlockClosure.zig")),
    // Module.init(@import("primitives/Boolean.zig").module),
    Module.init(if (config.includeLLVM) @import("primitives/llvm-primitives.zig") else struct {
        const moduleName = "llvm";
    }),
    // Module.init(@import("dispatch.zig")),
    Module.init(@import("controlWords.zig").module),
};

const Module = struct {
    name: []const u8,
    primitives: []const Primitive,
    const Primitive = struct {
        name: []const u8,
        number: u32,
        primitive: ?ThreadedFn.Fn,
        primitiveError: ?ThreadedFn.Fn,
        inlinePrimitive: ?ThreadedFn.Fn,
        moduleInit: ?*const fn () void,
        method: ?*const execute.CompiledMethod,
    };
    fn init(M: anytype) Module {
        return Module{ .name = M.moduleName, .primitives = &findPrimitives(M) };
    }
    fn countPrimitives(M: anytype) usize {
        const decls = @typeInfo(M).@"struct".decls;
        var n: usize = 0;
        for (decls) |decl| {
            const ds = @field(M, decl.name);
            switch (@typeInfo(@TypeOf(ds))) {
                .comptime_int, .int, .@"fn", .pointer, .bool => {},
                else => {
                    if (std.meta.hasFn(ds, "primitive") or std.meta.hasFn(ds, "primitiveError"))
                        n += 1;
                },
            }
        }
        return n;
    }
    fn findPrimitives(M: anytype) [countPrimitives(M)]Primitive {
        return blk: {
            const decls = @typeInfo(M).@"struct".decls;
            var mm: [countPrimitives(M)]Primitive = undefined;
            var n = 0;
            for (decls) |decl| {
                const ds = @field(M, decl.name);
                switch (@typeInfo(@TypeOf(ds))) {
                    .comptime_int, .int, .@"fn", .pointer, .bool => {},
                    else => {
                        if (std.meta.hasFn(ds, "primitive") or std.meta.hasFn(ds, "primitiveError")) {
                            mm[n] = Primitive{
                                .name = if (@hasDecl(ds, "name")) @field(ds, "name") else decl.name,
                                .number = if (@hasDecl(ds, "number")) @field(ds, "number") else 0,
                                .primitive = if (@hasDecl(ds, "primitive")) &@field(ds, "primitive") else null,
                                .primitiveError = if (@hasDecl(ds, "primitiveError")) &@field(ds, "primitiveError") else null,
                                .inlinePrimitive = if (@hasDecl(ds, "inlinePrimitive")) &@field(ds, "inlinePrimitive") else null,
                                .moduleInit = if (@hasDecl(ds, "moduleInit")) &@field(ds, "moduleInit") else null,
                                .method = if (@hasDecl(ds, "method")) &@field(ds, "method") else null,
                            };
                            n += 1;
                        }
                    },
                }
            }
            break :blk mm;
        };
    }
    fn findNumberedPrimitive(primNumber: usize) ?Primitive {
        for (&modules) |m| {
            for (m.primitives) |p| {
                if (p.number == primNumber)
                    return p;
            }
        }
        return null;
    }
    fn findNamedPrimitive(moduleName: []const u8, primName: []const u8) ?Primitive {
        for (&modules) |m| {
            if (std.mem.eql(u8, m.name, moduleName)) {
                for (m.primitives) |p| {
                    if (std.mem.eql(u8, p.name, primName))
                        return p;
                }
            }
        }
        return null;
    }
};
const testModule = if (config.is_test) struct {
    const moduleName = "test module";
    const moduleString = stringOf(moduleName).init().obj();
    const zName = stringOf("primitive998").init().obj();
    const primitiveNotDefined = stringOf("primitiveNotDefined").init().obj();
    pub const primitive998 = struct {
        pub const number = 998;
        pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            if (sp.next.immediate_class() != sp.top.immediate_class()) {
                return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
            } else {
                const newSp = sp.dropPut(Object.from(sp.next.equals(sp.top), null));
                return @call(tailCall, process.check(context.npc.f), .{ context.tpc, newSp, process, context, extra });
            }
        }
        pub fn primitiveError(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            if (sp.next.immediate_class() != sp.top.immediate_class()) {
                const newSp = sp.push(Sym.value.asObject());
                return @call(tailCall, Extra.primitiveFailed, .{ pc, newSp.?, process, context, extra });
            } else {
                const newSp = sp.dropPut(Object.from(sp.next == sp.top, null));
                return @call(tailCall, process.check(context.npc.f), .{ context.tpc, newSp, process, context, extra });
            }
        }
        pub fn inlinePrimitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            if (sp.next.immediate_class() != sp.top.immediate_class()) {
                return @call(tailCall, Extra.inlinePrimitiveFailed, .{ pc, sp, process, context, extra });
            } else {
                const newSp = sp.dropPut(Object.from(sp.next == sp.top, null));
                return @call(tailCall, process.check(pc.prim2()), .{ pc.next2(), newSp, process, context, extra });
            }
        }
    };
} else struct {
    const moduleName = "test module";
};

fn noPrim(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
    return @call(tailCall, process.check(pc.prev().prim()), .{ pc, sp, process, context, extra.encoded() });
    // the following should be exactly the same, but causes an error instead
    // return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
}
fn noPrimWithError(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
    const newSp = sp.push(Nil()).?;
    return @call(tailCall, Extra.primitiveFailed, .{ pc, newSp, process, context, extra });
}

pub const threadedFunctions = struct {
    pub const primitive = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            if (extra.isEncoded()) {
                const newPc = pc.next();
                return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), sp, process, context, extra.decoded() });
            }
            const primNumber = pc.uint();
            const method: *execute.CompiledMethod = @constCast(extra.getMethod().?);
            if (Module.findNumberedPrimitive(primNumber)) |prim| {
                if (prim.primitive) |p| {
                    method.executeFn = p;
                    method.jitted = p;
                } else if (config.is_test)
                    @panic("found primitive:error: need primitive:");
            } else {
                method.executeFn = noPrim;
                method.jitted = noPrim;
            }
            return @call(tailCall, method.executeFn, .{ pc, sp, process, context, extra });
        }
        test "primitive found" {
            try Execution.runTest(
                "primitive: found",
                .{
                    tf.primitive,
                    998,
                    tf.pushLiteral,
                    99,
                },
                &[_]Object{
                    Object.from(42, null),
                    Object.from(17, null),
                },
                &[_]Object{
                    False(),
                },
            );
        }
        test "primitive with error" {
            try Execution.runTest(
                "primitive: with error",
                .{
                    tf.primitive,
                    998,
                    tf.pushLiteral,
                    99,
                },
                &[_]Object{
                    True(),
                    Object.from(17, null),
                },
                &[_]Object{
                    Object.from(99, null),
                    True(),
                    Object.from(17, null),
                },
            );
        }
        test "primitive not found" {
            try Execution.runTest(
                "primitive: not found",
                .{
                    tf.primitive,
                    999,
                    tf.pushLiteral,
                    99,
                },
                &[_]Object{
                    Object.from(42, null),
                    Object.from(17, null),
                },
                &[_]Object{
                    Object.from(99, null),
                    Object.from(42, null),
                    Object.from(17, null),
                },
            );
        }
    };
    pub const primitiveError = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            if (extra.isEncoded()) {
                const newPc = pc.next();
                return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), sp, process, context, extra.decoded() });
            }
            const primNumber = pc.uint();
            if (Module.findNumberedPrimitive(primNumber)) |prim| {
                if (prim.primitiveError) |p| {
                    const method: *execute.CompiledMethod = @constCast(extra.getMethod().?);
                    method.executeFn = p;
                    method.jitted = p;
                    return @call(tailCall, p, .{ pc, sp, process, context, extra });
                } else if (config.is_test)
                    @panic("found primitive: need primitive:error:");
            }
            const method: *execute.CompiledMethod = @constCast(extra.getMethod().?);
            method.executeFn = noPrimWithError;
            method.jitted = noPrimWithError;
            return @call(tailCall, noPrimWithError, .{ pc, sp, process, context, extra });
        }
        test "primitive:error: found" {
            try Execution.runTest(
                "primitive:error: found",
                .{
                    tf.primitiveError,
                    998,
                    tf.pushLiteral,
                    99,
                },
                &[_]Object{
                    Object.from(42, null),
                    Object.from(17, null),
                },
                &[_]Object{
                    False(),
                },
            );
        }
        test "primitive:error: with error" {
            try Execution.runTest(
                "primitive:error: with error",
                .{
                    tf.primitiveError,
                    998,
                    tf.pushLiteral,
                    99,
                },
                &[_]Object{
                    True(),
                    Object.from(17, null),
                },
                &[_]Object{
                    Object.from(99, null),
                    Sym.value.asObject(),
                    True(),
                    Object.from(17, null),
                },
            );
        }
        test "primitive:error: not found" {
            try Execution.runTest(
                "primitive:error: not found",
                .{
                    tf.primitiveError,
                    999,
                    tf.pushLiteral,
                    99,
                },
                &[_]Object{
                    Object.from(42, null),
                    Object.from(17, null),
                },
                &[_]Object{
                    Object.from(99, null),
                    Nil(),
                    Object.from(42, null),
                    Object.from(17, null),
                },
            );
        }
    };
    pub const primitiveModule = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            if (extra.isEncoded()) {
                const newPc = pc.next().next();
                return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), sp, process, context, extra.decoded() });
            }
            if (pc.object().arrayAsSlice(u8) catch null) |primName| {
                if (pc.next().object().arrayAsSlice(u8) catch null) |modName| {
                    if (Module.findNamedPrimitive(modName, primName)) |prim| {
                        if (prim.primitive) |p| {
                            const method: *execute.CompiledMethod = @constCast(extra.getMethod().?);
                            method.executeFn = p;
                            method.jitted = p;
                            return @call(tailCall, p, .{ pc, sp, process, context, extra });
                        } else if (config.is_test)
                            @panic("found primitive: need primitive:error:");
                    }
                }
            }
            const method: *execute.CompiledMethod = @constCast(extra.getMethod().?);
            method.executeFn = noPrim;
            method.jitted = noPrim;
            return @call(tailCall, noPrim, .{ pc, sp, process, context, extra });
        }
        const primitive998 = testModule.zName;
        const primitiveNotDefined = testModule.primitiveNotDefined;
        test "primitive:module: found" {
            var exe = Execution.initTest("primitive:module: found", .{
                tf.primitiveModule,
                "0name",
                "1module",
                tf.pushLiteral,
                99,
            });
            try exe.resolve(&[_]Object{ primitive998.asObject(), testModule.moduleString.asObject() });
            try exe.execute(&[_]Object{
                Object.from(42, null),
                Object.from(17, null),
            });
            try expectEqualSlices(Object, &[_]Object{
                False(),
            }, exe.stack());
        }
        test "primitive:module: with error" {
            var exe = Execution.initTest("primitive:module: with error", .{
                tf.primitiveModule,
                "0name",
                "1module",
                tf.pushLiteral,
                99,
            });
            try exe.resolve(&[_]Object{ primitive998.asObject(), testModule.moduleString.asObject() });
            try exe.execute(&[_]Object{
                True(),
                Object.from(17, null),
            });
            try expectEqualSlices(Object, &[_]Object{
                Object.from(99, null),
                True(),
                Object.from(17, null),
            }, exe.stack());
        }
        test "primitive:module: not found" {
            var exe = Execution.initTest("primitive:module: not found", .{
                tf.primitiveModule,
                "0name",
                "1module",
                tf.pushLiteral,
                99,
            });
            try exe.resolve(&[_]Object{ primitiveNotDefined.asObject(), testModule.moduleString.asObject() });
            try exe.execute(&[_]Object{
                Object.from(42, null),
                Object.from(17, null),
            });
            try expectEqualSlices(Object, &[_]Object{
                Object.from(99, null),
                Object.from(42, null),
                Object.from(17, null),
            }, exe.stack());
        }
    };
    pub const primitiveModuleError = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            if (extra.isEncoded()) {
                const newPc = pc.next().next();
                return @call(tailCall, process.check(newPc.prim()), .{ newPc.next(), sp, process, context, extra.decoded() });
            }
            if (pc.object().arrayAsSlice(u8) catch null) |primName| {
                if (pc.next().object().arrayAsSlice(u8) catch null) |modName| {
                    if (Module.findNamedPrimitive(modName, primName)) |prim| {
                        if (prim.primitiveError) |p| {
                            const method: *execute.CompiledMethod = @constCast(extra.getMethod().?);
                            method.executeFn = p;
                            method.jitted = p;
                            return @call(tailCall, p, .{ pc, sp, process, context, extra });
                        } else if (config.is_test)
                            @panic("found primitive: need primitive:error:");
                    }
                }
            }
            const method: *execute.CompiledMethod = @constCast(extra.getMethod().?);
            method.executeFn = noPrimWithError;
            method.jitted = noPrimWithError;
            return @call(tailCall, noPrimWithError, .{ pc, sp, process, context, extra });
        }
        const primitive998 = testModule.zName;
        const primitiveNotDefined = testModule.primitiveNotDefined;
        test "primitive:module:error: found" {
            var exe = Execution.initTest("primitive:module:error: found", .{
                tf.primitiveModuleError,
                "0name",
                "1module",
                tf.pushLiteral,
                99,
            });
            try exe.resolve(&[_]Object{ primitive998.asObject(), testModule.moduleString.asObject() });
            try exe.execute(&[_]Object{
                Object.from(42, null),
                Object.from(17, null),
            });
            try expectEqualSlices(Object, &[_]Object{
                False(),
            }, exe.stack());
        }
        test "primitive:module:error: with error" {
            var exe = Execution.initTest("primitive:module:error: with error", .{
                tf.primitiveModuleError,
                "0name",
                "1module",
                tf.pushLiteral,
                99,
            });
            try exe.resolve(&[_]Object{ primitive998.asObject(), testModule.moduleString.asObject() });
            try exe.execute(&[_]Object{
                True(),
                Object.from(17, null),
            });
            try expectEqualSlices(Object, &[_]Object{
                Object.from(99, null),
                Sym.value.asObject(),
                True(),
                Object.from(17, null),
            }, exe.stack());
        }
        test "primitive:module:error: not found" {
            var exe = Execution.initTest("primitive:module:error: not found", .{
                tf.primitiveModuleError,
                "0name",
                "1module",
                tf.pushLiteral,
                99,
            });
            try exe.resolve(&[_]Object{ primitiveNotDefined.asObject(), testModule.moduleString.asObject() });
            try exe.execute(&[_]Object{
                Object.from(42, null),
                Object.from(17, null),
            });
            try expectEqualSlices(Object, &[_]Object{
                Object.from(99, null),
                Nil(),
                Object.from(42, null),
                Object.from(17, null),
            }, exe.stack());
        }
    };
    pub const inlinePrimitive = struct {
        pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
            const primNumber = pc.object().primitive();
            if (Module.findNumberedPrimitive(primNumber)) |prim| {
                if (prim.inlinePrimitive) |p| {
                    @constCast(pc.prev().asCodePtr()).patchPrim(p);
                    return @call(tailCall, p, .{ pc, sp, process, context, extra });
                }
            }
            @panic("found primitive:error: need primitive:");
        }
        test "inlinePrimitive found" {
            var exe = Execution.initTest("inlinePrimitive: found", .{
                tf.inlinePrimitive,
                "0prim",
                tf.inlinePrimitive,
                "0prim",
                tf.pushLiteral,
                99,
            });
            try exe.resolve(&[_]Object{Sym.value.withPrimitive(998)});
            try exe.execute(&[_]Object{
                Object.from(42, null),
                Object.from(17, null),
                False(),
            });
            try expectEqualSlices(Object, &[_]Object{
                Object.from(99, null),
                True(),
            }, exe.stack());
        }
        test "inlinePrimitive not found" {
            if (true) return error.SkipZigTest;
            try Execution.runTest(
                "inlinePrimitive: not found",
                .{
                    tf.inlinePrimitive,
                    999,
                },
                &[_]Object{
                    Object.from(42, null),
                    Object.from(17, null),
                },
                &[_]Object{
                    Object.from(99, null),
                    Object.from(42, null),
                    Object.from(17, null),
                },
            );
        }
    };
};
pub const primitiveThreadedFunctions = .{
    // @import("primitives/Object.zig").threadedFns,
    // @import("primitives/Smallinteger.zig").threadedFns,
    // @import("primitives/Behavior.zig").threadedFns,
    @import("primitives/BlockClosure.zig").threadedFns,
        // @import("primitives/Boolean.zig").threadedFns,
};
pub fn init() void {
    @import("primitives/Object.zig").init();
    // @import("primitives/Smallinteger.zig").init();
    // @import("primitives/Behavior.zig").init();
    // @import("primitives/BlockClosure.zig").init();
    // @import("primitives/Boolean.zig").init();
}
test "Verify primitives loaded" {}

// all numeric primitives defined in Pharo image
// SmallInteger - 1 2 3 4 5 6 7 8 9 10 11 12 13
// SmallInteger and LargePositiveInteger - 14 15 16 17 (bit operations)
// Integer @ - 18
// BlockClosure - 19
// LargeInteger - 20 21 22 23 24 25 26 29 30 31 32 33
// Float related - 38 39 40 41 42 43 44 45 46 47 48 49 50 51 52 53 54 55 56 57 58 59
// at: - 60
// at:put: - 61
// size - 62
// at: ^Character - 63
// at:put: Character - 64
// ReadStream next - 65
// WriteStream nextPut: - 66
// ReadStream atEnd - 67
// special - 68 69
// basicNew - 70
// basicNew: - 71
// instVarAt: - 73
// instVarAt:put: - 74
// misc - 75 76 77 78 79 80
// perform: - 83
// perform:withArguments: - 84
// semaphore/process - 85 86 87 88 89
// snapShot - 97
// perform:withArguments:inSuperClass: - 100
// replaceFrom:to:with:startingAt: - 105
// 108
// == 110
// class/species - 111
// SmalltalkImage - 112 113 114 115 116 119 121 124 125 128 129 130 131
// 132 135 136 138 139 140 142 143 144 145 148 149 158 159 165 166 167 168 169 170 171 175 176 177 178 180 188 195 196 197 198 199
// BlockClosure - 206 207 208 209
// Context - 210 211 212
// Cog - 214 215
// timer related - 230 231 232 233 235 240 242
// AST - 243 246 249
// SmallFloat64 - 541 542 543 544 545 546 547 548 549 550 551 552 553 554 555 556 557 558 559
// SmalltalkImage unbindExternalPrimitives - 570
// VirtualMachine - 254 571 572 573
// Native arrays - 600 601 602 603 604 605 606 607 608 609 610 611 612 613 614 615 616 617 618 619 620 621 622 623 624 625 626 627 628 629
// ExternalAddress - 630 631 632 633 634 635 636 637 638 639 640 641 642 643 644 645 646 647 648 649 650 651 652 653 654 655 656 657 658 659
// ASTSmallInteger asCharacter - 2000
// ASTStdIOStream unicodeRead - 2001
// ASTStdIOStream nextPut: - 2002
// ASTStdIOStream nextPutAll: - 2003
// ASTStdIOStream nextPutError: - 2004
// ASTStdIOStream nextPutErrorAll: - 2005
// ASTBehavour basicIdentityHash - 2075
// ASTFloat basicIdentityHash - 2171
