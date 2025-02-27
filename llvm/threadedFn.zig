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

// Build: zig build -Dllvm-path="./libs/threadedFn.zig"
// Run: ./zig-out/bin/threadedFn

pub fn main() !void {

    // LLVM target specific setup logic
    target.initNativeTarget();

    // Create LLVM module (context created by default)
    const module: types.LLVMModuleRef = core.LLVMModuleCreateWithName("module");
    const builder: types.LLVMBuilderRef = core.LLVMCreateBuilder();

    // Create a ThreadedFn in IR
    _ = try populateModule(module, builder);
}

pub fn populateModule(module: types.LLVMModuleRef, builder: types.LLVMBuilderRef) !types.LLVMModuleRef {
    const ctx = core.LLVMGetGlobalContext();

    // Create a `TagObject` packed struct type
    const namedTagObjectTy = core.LLVMStructCreateNamed(ctx, "TagObject");
    var tagObjectFields = [_]types.LLVMTypeRef{
        core.LLVMIntTypeInContext(ctx, 3), // tag
        core.LLVMIntTypeInContext(ctx, 5), // group
        core.LLVMIntTypeInContext(ctx, 56), // hash
    };
    core.LLVMStructSetBody(namedTagObjectTy, &tagObjectFields[0], 3, 1);

    // Create a `Stack` non-packed struct type
    const namedStackTy = core.LLVMStructCreateNamed(ctx, "Stack");
    var namedStackFields = [_]types.LLVMTypeRef{
        namedTagObjectTy, // top
        namedTagObjectTy, // next
        namedTagObjectTy, // third
    };
    core.LLVMStructSetBody(namedStackTy, &namedStackFields[0], 3, 0);

    // Create a `Code` union struct
    const namedUnionTy = core.LLVMStructCreateNamed(ctx, "CodeUnion");
    const namedUnionField = core.LLVMArrayType(core.LLVMInt8TypeInContext(ctx), 8);
    var fields = [_]types.LLVMTypeRef{namedUnionField};
    core.LLVMStructSetBody(namedUnionTy, &fields[0], 1, 0);

    // `ThreadedFn` sp parameter
    const spTy = core.LLVMPointerType(namedStackTy, 0);

    // Create `ThreadedFn` PC
    const pcTy = core.LLVMPointerType(namedUnionTy, 0);

    // ----- Create the `ThreadedFn` ------
    var paramTypes: [2]types.LLVMTypeRef = .{ pcTy, spTy };
    const funcType = core.LLVMFunctionType(spTy, &paramTypes[0], 2, 0);
    const myThreadedFn: types.LLVMValueRef = core.LLVMAddFunction(module, "pushLiteral", funcType);

    // Set pc name
    const pcParam: types.LLVMValueRef = core.LLVMGetParam(myThreadedFn, 0);
    core.LLVMSetValueName(pcParam, "pc");

    // Set sp name
    const spParam: types.LLVMValueRef = core.LLVMGetParam(myThreadedFn, 1);
    core.LLVMSetValueName(spParam, "sp");

    // Setup function body
    const entryBB: types.LLVMBasicBlockRef = core.LLVMAppendBasicBlock(myThreadedFn, "entry");
    core.LLVMPositionBuilderAtEnd(builder, entryBB);

    // Only for testing purposes - enables emission of types textually in the IR
    _ = core.LLVMBuildAlloca(builder, namedStackTy, "test-stack");
    _ = core.LLVMBuildAlloca(builder, namedTagObjectTy, "test-tagObject");
    _ = core.LLVMBuildAlloca(builder, namedUnionTy, "test-union");
    _ = core.LLVMBuildRet(builder, spParam); // only to adhere to the return type

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

    std.debug.print("\n--- IR DUMP ---\n", .{});
    core.LLVMDumpModule(module);
    std.debug.print("--- END IR ---\n\n", .{});

    // Cleanup builder
    core.LLVMDisposeBuilder(builder);

    return module;
}

inline fn buildGEP(builder: types.LLVMBuilderRef, elementType: types.LLVMTypeRef, base: types.LLVMValueRef, offset: i64, name: []const u8) types.LLVMValueRef {
    const offset_bits: u64 = @bitCast(offset);
    const signExtend = offset < 0;
    const idx = [_]types.LLVMValueRef{core.LLVMConstInt(elementType, offset_bits, @intFromBool(signExtend))};
    const idx_ptr: [*c]types.LLVMValueRef = @constCast(@ptrCast(&idx[0]));
    return core.LLVMBuildGEP2(builder, elementType, base, idx_ptr, 1, @ptrCast(name));
}
