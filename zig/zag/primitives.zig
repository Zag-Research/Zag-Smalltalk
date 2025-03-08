// all primitives defined in Pharo image
// SmallInteger - 1 2 3 4 5 6 7 8 9 10 11 12 13
// SmallInteger and LargePositiveInteger - 14 15 16 17
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

const std = @import("std");
const config = @import("config.zig");
const tailCall = config.tailCall;
const trace = config.trace;
const stdCall = config.stdCall;
const zag = @import("zag.zig");
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
const Context = zag.context;
const Extra = execute.Extra;
const tf = zag.threadedFn.Enum;

pub const Module = struct {
    name: []const u8,
    primitives: []const Primitive,
    const Primitive = struct {
        name: []const u8,
        primitive: ThreadedFn.Fn,
    };
    var numberedPrimitives = [_]ThreadedFn.Fn{&noPrim} ** 1000;
    var numberedPrimitivesWithError = [_]ThreadedFn.Fn{&noPrimWithError} ** 1000;
    pub fn noPrim(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
        return @call(tailCall, pc.prev().prim(), .{ pc, sp, process, context, extra.encoded() });
    }
    pub fn noPrimWithError(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
        const newSp = sp.push(Nil);
        return @call(tailCall, pc.prev().prim(), .{ pc, newSp, process, context, extra.encoded() });
    }
    pub fn init(M: anytype) Module {
        return Module{.name = M.name, .primitives = findPrimitives(M)};
    }
    fn countPrimitives(M: anytype) usize {
        const decls = @typeInfo(M).@"struct".decls;
        var n: usize = 0;
        for (decls) |decl| {
            const ds = @field(M,decl.name);
            if (std.meta.hasFn(ds,"primitive")) {
                if (!@hasDecl(ds,"number"))
                    n += 1;
            }
        }
        return n;
    }
    pub fn findPrimitives(M: anytype) [countPrimitives(M)]Module {
        return blk: {
            const decls = @typeInfo(M).@"struct".decls;
            var mm: [countPrimitives(M)]Module = undefined;
            var n = 0;
            for (decls) |decl| {
                const ds = @field(M,decl.name);
                if (std.meta.hasFn(ds,"primitive")) {
                    if (@hasDecl(ds,"number")) {
                        numberedPrimitives[@field(ds,"number")] = &@field(ds,"primitive");
                    } else {
                        mm[n] = Primitive{.name = @field(ds,"name"), .primitive = &@field(ds,"primitive")};
                        n += 1;
                    }
                }
            }
            break :blk mm;
        };
    }
};
const modules = [_]Module{
    Module.init(@import("primitives/Object.zig")),
    // @import("primitives/Smallinteger.zig").module,
    // @import("primitives/Behavior.zig").module,
    // @import("primitives/BlockClosure.zig").module,
    // @import("primitives/Boolean.zig").module,
    // @import("primitives/llvm.zig").module,
};
pub const primitive = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
        if (extra.isEncoded()) {
            const newPc = pc.next();
            return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, extra.decoded() });
        }
        const primNumber = pc.uint();
        if (primNumber < Module.numberedPrimitives.len) {
            const prim = Module.numberedPrimitives[primNumber];
            const method = extra.method;
            method.executeFn = prim;
            method.jitted = prim;
            return @call(tailCall, prim, .{ pc, sp, process, context, extra });
        }
        {
            const newPc = pc.next();
            return @call(tailCall, newPc.prim(), .{ newPc.next(), sp, process, context, extra });
        }   
    }
    test "primitive found" {
        try Execution.runTest(
            "primitive found",
            .{
                tf.primitive,
                110,
            },
            &[_]Object{Object.from(42),Object.from(17),},
            &[_]Object{False,},
        );
    }
    test "primitive not found" {
        try Execution.runTest(
            "primitive not found",
            .{
                tf.primitive,
                999,
            },
            &[_]Object{Object.from(42),Object.from(17),},
            &[_]Object{Object.from(42),Object.from(17),},
        );
    }
};
pub const primitiveError = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
        _ = .{pc,sp,process,context,extra,unreachable};
    }
};
pub const primitiveModule = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
        _ = .{pc,sp,process,context,extra,unreachable};
    }
};
pub const primitiveModuleError = struct {
    pub fn threadedFn(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) SP {
        _ = .{pc,sp,process,context,extra,unreachable};
    }
};

pub const threadedFns = struct {
    pub usingnamespace @import("primitives/Object.zig");
    // pub usingnamespace @import("primitives/Smallinteger.zig").threadedFns;
    // pub usingnamespace @import("primitives/Behavior.zig").threadedFns;
    // pub usingnamespace @import("primitives/BlockClosure.zig").threadedFns;
    // pub usingnamespace @import("primitives/Boolean.zig").threadedFns;
};
pub fn init() void {
    @import("primitives/Object.zig").init();
    // @import("primitives/Smallinteger.zig").init();
    // @import("primitives/Behavior.zig").init();
    // @import("primitives/BlockClosure.zig").init();
    // @import("primitives/Boolean.zig").init();
}
