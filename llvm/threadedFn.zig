const std = @import("std");

const SP = @import("../zig/zag/execute.zig").SP;

const llvm = @import("llvm");
const target = llvm.target;
const types = llvm.types;
const core = llvm.core;
const analysis = llvm.analysis;
const orc = llvm.orc;
const lljit = llvm.jit;
const engine = llvm.engine;
const target_machine = llvm.target_machine;

// Build: `zig build -DExamples=true`
// Run: `./zig-out/bin/example`

pub fn main() void {

    // LLVM target specific setup logic
    target.initNativeTarget();

    // Create LLVM module (context created by default)
    const module: types.LLVMModuleRef = core.LLVMModuleCreateWithName("module");
    const builder: types.LLVMBuilderRef = core.LLVMCreateBuilder();

    // Create our module
    _ = try createModule(module, builder);
}

pub fn createModule(module: types.LLVMModuleRef, builder: types.LLVMBuilderRef) !types.LLVMModuleRef {
    const ctx = core.LLVMGetGlobalContext();

    // Build the Stack parameter type
    const stackName = "Stack";
    const namedStackTy = core.LLVMStructCreateNamed(ctx, stackName);

    const i64Ty = core.LLVMInt64TypeInContext(ctx);
    var stackFields = [_]types.LLVMTypeRef{
        i64Ty, // top
        i64Ty, // next
        i64Ty, // third
    };
    // false => not packed
    core.LLVMStructSetBody(namedStackTy, stackFields[0..], 3, 0);

    // Create a global variable of type %Stack
    const zeroStruct = core.LLVMConstNull(namedStackTy);
    const globalVarName = "stackGlobal";
    const globalVar = core.LLVMAddGlobal(module, namedStackTy, globalVarName);
    core.LLVMSetLinkage(globalVar, types.LLVMLinkage.LLVMExternalLinkage);
    core.LLVMSetInitializer(globalVar, zeroStruct);
    core.LLVMSetAlignment(globalVar, 8);

    // Create a function: (SP) -> SP
    const spTy = core.LLVMPointerType(namedStackTy, 0);
    const fnReturnTy = spTy;
    var paramTypes = [_]types.LLVMTypeRef{spTy};

    const fnTy = core.LLVMFunctionType(fnReturnTy, paramTypes[0..], 1, 0);
    const fnName = "";
    const fnVal = core.LLVMAddFunction(module, fnName, fnTy);

    // Optionally set calling convention here
    // core.LLVMSetFunctionCallConv(fnVal, llvm.LLVMX86StdcallCallConv);

    const entryBB = core.LLVMAppendBasicBlockInContext(ctx, fnVal, "entry");
    core.LLVMPositionBuilderAtEnd(builder, entryBB);

    // Parameter 0 is the incoming pointer
    const param_sp = core.LLVMGetParam(fnVal, 0);

    // Return it directly
    _ = core.LLVMBuildRet(builder, param_sp);

    // Verify the module and capture the message
    var errorMessage: ?[*:0]u8 = null;
    if (analysis.LLVMVerifyModule(module, types.LLVMVerifierFailureAction.LLVMPrintMessageAction, &errorMessage) != 0) {
        if (errorMessage) |msg| {
            defer core.LLVMDisposeMessage(msg); // ensures cleanup
            std.debug.print("Verification failed: {s}\n", .{msg});
            return error.ModuleVerificationFailure;
        } else {
            return error.UnknownCauseModuleVerificationFailure;
        }
    } else {
        std.debug.print("Module verification passed.\n", .{});
    }

    // Dump the IR
    std.debug.print("\n--- IR DUMP ---\n", .{});
    core.LLVMDumpModule(module);
    std.debug.print("--- END IR ---\n\n", .{});

    return module;
}

inline fn buildGEP(builder: types.LLVMBuilderRef, elementType: types.LLVMTypeRef, base: types.LLVMValueRef, offset: i64, name: []const u8) types.LLVMValueRef {
    const offset_bits: u64 = @bitCast(offset);
    const signExtend = offset < 0;
    const idx = [_]types.LLVMValueRef{core.LLVMConstInt(elementType, offset_bits, @intFromBool(signExtend))};
    const idx_ptr: [*c]types.LLVMValueRef = @constCast(@ptrCast(&idx[0]));
    return core.LLVMBuildGEP2(builder, elementType, base, idx_ptr, 1, @ptrCast(name));
}
