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
const llvm = @import("llvm");
const core = llvm.core;
const LLVMtype = llvm.types;

const LLVMAttributeRef = LLVMtype.LLVMAttributeRef;
const LLVMBasicBlockRef = LLVMtype.LLVMBasicBlockRef;
const LLVMBool = LLVMtype.LLVMBool;
const LLVMBuilderRef = LLVMtype.LLVMBuilderRef;
const LLVMComdatRef = LLVMtype.LLVMComdatRef;
const LLVMContextRef = LLVMtype.LLVMContextRef;
const LLVMDIBuilderRef = LLVMtype.LLVMDIBuilderRef;
const LLVMDiagnosticInfoRef = LLVMtype.LLVMDiagnosticInfoRef;
const LLVMJITEventListenerRef = LLVMtype.LLVMJITEventListenerRef;
const LLVMMemoryBufferRef = LLVMtype.LLVMMemoryBufferRef;
const LLVMMetadataRef = LLVMtype.LLVMMetadataRef;
const LLVMModuleFlagEntry = LLVMtype.LLVMModuleFlagEntry;
const LLVMModuleProviderRef = LLVMtype.LLVMModuleProviderRef;
const LLVMModuleRef = LLVMtype.LLVMModuleRef;
const LLVMNamedMDNodeRef = LLVMtype.LLVMNamedMDNodeRef;
const LLVMPassManagerRef = LLVMtype.LLVMPassManagerRef;
const LLVMPassRegistryRef = LLVMtype.LLVMPassRegistryRef;
const LLVMTypeRef = LLVMtype.LLVMTypeRef;
const LLVMUseRef = LLVMtype.LLVMUseRef;
const LLVMValueMetadataEntry = LLVMtype.LLVMValueMetadataEntry;
const LLVMValueRef = LLVMtype.LLVMValueRef;

pub const moduleName = "llvm";

// Compiling: zig build -Dllvm-path='./zag/primitives/llvm.zig'

pub fn main() void {}
fn Converter(T: type) type {
    const tag = switch (T) {
        LLVMAttributeRef => 1,
        LLVMBasicBlockRef => 2,
        LLVMBool => 3,
        LLVMBuilderRef => 4,
        LLVMComdatRef => 5,
        LLVMContextRef => 6,
        LLVMDIBuilderRef => 7,
        LLVMDiagnosticInfoRef => 8,
        LLVMJITEventListenerRef => 9,
        LLVMMemoryBufferRef => 10,
        LLVMMetadataRef => 11,
        LLVMModuleFlagEntry => 12,
        LLVMModuleProviderRef => 13,
        LLVMModuleRef => 14,
        LLVMNamedMDNodeRef => 15,
        LLVMPassManagerRef => 16,
        LLVMPassRegistryRef => 17,
        LLVMTypeRef => 18,
        LLVMUseRef => 19,
        LLVMValueMetadataEntry => 20,
        LLVMValueRef => 21,
        else => @compileError("Converter needs extansion for type: " ++ @typeName(T)),
    };
    const llvmClass = @intFromEnum(object.ClassIndex.LLVM);
    return struct {
        fn asObject(llvmPtr: T) Object {
            return Object.Special.objectFrom(llvmClass, tag, @ptrCast(llvmPtr));
        }
        fn asLLVM(tagObject: Object) !T {
            const spec = tagObject.rawSpecial();
            if (spec.imm == llvmClass) {
                if (spec.tag == tag)
                    return @ptrCast(spec.ptr());
            }
            return error.InvalidTag;
        }
    };
}

const AttributeRef = Converter(LLVMAttributeRef);
const BasicBlockRef = Converter(LLVMBasicBlockRef);
const Bool = Converter(LLVMBool);
const BuilderRef = Converter(LLVMBuilderRef);
const ComdatRef = Converter(LLVMComdatRef);
const ContextRef = Converter(LLVMContextRef);
const DIBuilderRef = Converter(LLVMDIBuilderRef);
const DiagnosticInfoRef = Converter(LLVMDiagnosticInfoRef);
const JITEventListenerRef = Converter(LLVMJITEventListenerRef);
const MemoryBufferRef = Converter(LLVMMemoryBufferRef);
const MetadataRef = Converter(LLVMMetadataRef);
const ModuleFlagEntry = Converter(LLVMModuleFlagEntry);
const ModuleProviderRef = Converter(LLVMModuleProviderRef);
const ModuleRef = Converter(LLVMModuleRef);
const NamedMDNodeRef = Converter(LLVMNamedMDNodeRef);
const PassManagerRef = Converter(LLVMPassManagerRef);
const PassRegistryRef = Converter(LLVMPassRegistryRef);
const TypeRef = Converter(LLVMTypeRef);
const UseRef = Converter(LLVMUseRef);
const ValueMetadataEntry = Converter(LLVMValueMetadataEntry);
const ValueRef = Converter(LLVMValueRef);

const noLLVM = true;

pub const llvmString = stringOf("llvm").init().obj();

pub const createBuilderObject = struct {
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        if (noLLVM) return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
        const builder: LLVMBuilderRef = null; // should be calling the LLVM creator function
        if (builder == null) return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
        sp.top = BuilderRef.asObject(builder);
        return @call(tailCall, process.check(context.nPc()), .{ context.tPc(), sp, process, context, undefined });
    }
    test "createBuilderObject" {
        if (noLLVM) return error.SkipZigTest;
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

inline fn singleIndexGEP(builder: LLVMBuilderRef, elementType: LLVMTypeRef, base: LLVMValueRef, offset: i64, name: []const u8) LLVMValueRef {
    // singleIndex - for pointer and integer offsets
    const offset_bits: u64 = @bitCast(offset);
    const signExtend = offset < 0;
    const idx = [_]LLVMValueRef{core.LLVMConstInt(elementType, offset_bits, @intFromBool(signExtend))};
    const idx_ptr: [*c]LLVMValueRef = @constCast(@ptrCast(&idx[0]));
    return core.LLVMBuildGEP2(builder, elementType, base, idx_ptr, 1, @ptrCast(name));
}

pub const @"register:plus:asName:" = struct {
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        if (noLLVM) return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
        //get builder instance from module?
        const builder = BuilderRef.asLLVM(sp.at(4)) catch return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
        const registerToModify = ValueRef.asLLVM(sp.third) catch return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
        const offset = sp.next.to(i64);
        const name = sp.top.arrayAsSlice(u8) catch return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
        const newSp = sp.unreserve(3);
        const module: LLVMModuleRef = undefined;
        const tagObjectTy = core.LLVMGetTypeByName(module, "TagObject");
        sp.top = ValueRef.asObject(singleIndexGEP(@ptrCast(builder), tagObjectTy, registerToModify, offset, name));
        return @call(tailCall, process.check(context.npc.f), .{ context.tpc, newSp, process, context, undefined });
    }
};
pub const newLabel = struct {
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        if (noLLVM) return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
        return @call(tailCall, process.check(context.npc.f), .{ context.tpc, sp, process, context, undefined });
    }
};
pub const @"literalToRegister:" = struct {
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        // const valueToPush = sp.top;
        if (noLLVM) return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
        return @call(tailCall, process.check(context.npc.f), .{ context.tpc, sp, process, context, undefined });
    }
};
pub const @"add:to:" = struct {
    pub fn primitive(pc: PC, sp: SP, process: *Process, context: *Context, extra: Extra) Result {
        if (noLLVM) return @call(tailCall, Extra.primitiveFailed, .{ pc, sp, process, context, extra });
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
