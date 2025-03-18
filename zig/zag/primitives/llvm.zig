const std = @import("std");
const expectEqual = std.testing.expectEqual;
const zag = @import("../zag.zig");
const config = zag.config;
const tailCall = config.tailCall;
const trace = config.trace;
const checkEqual = zag.utilities.checkEqual;
const object = zag.object;
const Object = object.Object;
const ClassIndex = object.ClassIndex;
const Nil = object.Nil;
const NotAnObject = object.NotAnObject;
const True = object.True;
const False = object.False;
//const class = @import("class.zig");
const symbol = zag.symbol;
const Sym = symbol.symbols;
const phi32 = zag.utilities.inversePhi(u32);
const execute = zag.execute;
const llvm = zag.llvm;
const core = llvm.core;
const LLVMtype = llvm.types;
const empty = &[0]Object{};
const PC = execute.PC;
const SP = execute.SP;
const Execution = execute.Execution;
const Process = zag.Process;
const Context = zag.Context;
const Extra = execute.Extra;
const Result = execute.Result;
const CompiledMethod = execute.CompiledMethod;
const stringOf = zag.heap.CompileTimeString;
const tf = zag.threadedFn.Enum;

pub const moduleName = "llvm";
pub fn init() void {}
const LLVMTypeEnum = enum(u8) {
    invalid,
    value,
    context,
    builder,
};
const llvmClass = @intFromEnum(object.ClassIndex.LLVM);
fn detag(obj: Object, tag: LLVMTypeEnum) !*Object {
    if (from(obj)) |spec| {
        if (spec.tag == @intFromEnum(tag))
            return spec.ptr();
    }
    return error.InvalidTag;
}
fn getTag(obj: Object) ?LLVMTypeEnum {
    if (from(obj)) |spec|
        return spec.tag;
    return null;
}
fn from(obj: Object) ?Object.Special {
    const spec = obj.rawSpecial();
    if (spec.imm == llvmClass) return spec;
    return null;
}
fn llvmTag(llvmPtr: ?*opaque {}, tag: LLVMTypeEnum) Object {
    return Object.Special.objectFrom(llvmClass, @intFromEnum(tag), llvmPtr);
}
pub fn asLLVMValueRef(tagObject: Object) !LLVMtype.LLVMValueRef {
    return @ptrCast(try detag(tagObject, .value));
}
pub fn asLLVMContextRef(tagObject: Object) !LLVMtype.LLVMContextRef {
    return @ptrCast(try detag(tagObject, .context));
}
pub fn asLLVMBuilderRef(tagObject: Object) !LLVMtype.LLVMContextRef {
    return @ptrCast(try detag(tagObject, .builder));
}
pub const llvmString = stringOf("llvm").init().obj();

pub const createBuilderObject = struct {
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const builder: LLVMtype.LLVMBuilderRef = null; // should be calling the LLVM creator function
        if (builder == null) return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
        sp.top = llvmTag(builder, .builder);
        return @call(tailCall, process.check(context.nPc()), .{ context.tPc(), sp, process, context, undefined });
    }
    test "createBuilderObject" {
        const name = stringOf("createBuilderObject").init().asObject();
        var exe = Execution.initTest("llvm createBuilderObject", .{
            tf.@"primitive:module:",
            "0name",
            "1llvm",
            tf.pushLocal,
            42,
        });
        try exe.resolve(&[_]Object{ name, llvmString.asObject() });
        try exe.execute(&[_]Object{Object.from(17)});
        const result = exe.stack()[0];
        try expectEqual(Object.from(42), result);
    }
};

inline fn singleIndexGEP(builder: LLVMtype.LLVMBuilderRef, elementType: LLVMtype.LLVMTypeRef, base: LLVMtype.LLVMValueRef, offset: i64, name: []const u8) LLVMtype.LLVMValueRef {
    // singleIndex - for pointer and integer offsets
    const offset_bits: u64 = @bitCast(offset);
    const signExtend = offset < 0;
    const idx = [_]LLVMtype.LLVMValueRef{core.LLVMConstInt(elementType, offset_bits, @intFromBool(signExtend))};
    const idx_ptr: [*c]LLVMtype.LLVMValueRef = @constCast(@ptrCast(&idx[0]));
    return core.LLVMBuildGEP2(builder, elementType, base, idx_ptr, 1, @ptrCast(name));
}

pub const @"register:plus:asName:" = struct {
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        //get builder instance from module?
        const builder = asLLVMBuilderRef(sp.at(4)) catch return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
        const registerToModify = asLLVMValueRef(sp.third) catch return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
        const offset = sp.next.to(u64);
        var buffer: [16]u8 = undefined;
        const name = sp.top.asZeroTerminatedString(&buffer) catch return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
        const newSp = sp.unreserve(3);
        const tagObjectTy = core.LLVMGetTypeByName(module, "TagObject");
        sp.top = singleIndexGEP(builder, tagObjectTy, registerToModify, offset, name);
        return @call(tailCall, process.check(context.npc.f), .{ context.tpc, newSp, process, context, undefined });
    }
};
pub const newLabel = struct {
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        _ = .{ pc, sp, process, context, extra };
        return @call(tailCall, process.check(context.npc.f), .{ context.tpc, sp, process, context, undefined });
    }
};
pub const @"literalToRegister:" = struct {
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        const valueToPush = sp.top;
        _ = .{ valueToPush, pc, sp, process, context, extra };
        return @call(tailCall, process.check(context.npc.f), .{ context.tpc, sp, process, context, undefined });
    }
};
pub const @"add:to:" = struct {
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        _ = .{ pc, sp, process, context, extra };
        return @call(tailCall, process.check(context.npc.f), .{ context.tpc, sp, process, context, undefined });
    }
};

test "primitives" {}

extern fn llvmPL(_: *anyopaque, _: *anyopaque, _: *anyopaque, _: *anyopaque, c_int) *anyopaque;
const llvmPLCM = CompiledMethod.init(Sym.yourself, @ptrCast(&llvmPL));
// old tests
// test "simple llvm" {
//     std.debug.print("Test: simple llvm\n", .{});
//     const expectEqual = std.testing.expectEqual;
//     Process.resetForTest();
//     const pl = if (true) &p.pushLiteral else llvmPLCM;
//     var method = compileMethod(Sym.yourself, 0, 0, .testClass, .{
//         pl,                    0,
//         &p.returnTopNoContext,
//     });
//     var te = Execution.new();
//     te.init(null);
//     var objs = [_]Object{ Nil, True };
//     const result = te.run(objs[0..], &method);
//     try expectEqual(result.len, 3);
//     try expectEqual(result[0], Object.from(0));
//     try expectEqual(result[1], Nil);
//     try expectEqual(result[2], True);
// }
// test "llvm external" {
//     std.debug.print("Test: llvm external\n", .{});
//     const expectEqual = std.testing.expectEqual;
//     Process.resetForTest();
//     const pl = if (true) &p.pushLiteral else llvmPLCM;
//     var method = compileMethod(Sym.yourself, 1, 0, .testClass, .{
//         &p.pushContext,   "^",
//         ":label1",        &p.pushLiteral,
//         42,               &p.popLocal,
//         0,                &p.pushLocal,
//         0,                pl,
//         0,                &p.pushLiteral,
//         true,             &p.classCase,
//         ClassIndex.False, "label3",
//         &p.branch,        "label2",
//         ":label3",        &p.pushLocal,
//         0,                ":label4",
//         &p.returnTop,     ":label2",
//         pl,               0,
//         &p.branch,        "label4",
//     });
//     method.resolve();
//     const debugging = false;
//     if (debugging) {
//         @setRuntimeSafety(false);
//         for (&method.code, 0..) |*tv, idx|
//             trace("\nt[{}]=0x{x:0>8}: 0x{x:0>16}", .{ idx, @intFromPtr(tv), tv.object.rawU() });
//     }
//     var objs = [_]Object{ Nil, Nil, Nil };
//     var te = Execution.new();
//     te.init(null);
//     const result = te.run(objs[0..], &method);
//     try expectEqual(result.len, 1);
//     try expectEqual(result[0], Object.from(0));
// }
