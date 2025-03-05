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
    const threadedFnSig = try createThreadedFnSig(module, "pushLiteral");
    _ = try createPushLiteralFn(threadedFnSig, module, builder);
    _ = try verifyModule(module);

    // Cleanup builder
    core.LLVMDisposeBuilder(builder);

    printModule(module);
}

pub fn createPushLiteralFn(threadedFn: types.LLVMValueRef, module: types.LLVMModuleRef, builder: types.LLVMBuilderRef) !void {

    // Setup function body
    const entryBB: types.LLVMBasicBlockRef = core.LLVMAppendBasicBlock(threadedFn, "entry");
    core.LLVMPositionBuilderAtEnd(builder, entryBB);

    // Get the parameters that will be modified
    const pcParam = core.LLVMGetParam(threadedFn, 0);
    const spParam = core.LLVMGetParam(threadedFn, 1);

    // Get references to the types we need
    const tagObjectTy = core.LLVMGetTypeByName(module, "TagObject");
    const tagObjectPtrTy = core.LLVMPointerType(tagObjectTy, 0);
    const pcTy = core.LLVMGetTypeByName(module, "PC");
    const codeUnionTy = core.LLVMGetTypeByName(module, "CodeUnion");
    const codeUnionPtrTy = core.LLVMPointerType(codeUnionTy, 0);

    // Calculate &(pc->code) => CodeUnion**
    const codeUnionPtrField = core.LLVMBuildStructGEP2(builder, pcTy, pcParam, 0, "pc_code_ptr_field");
    // Load the value pointed at by codeUnionPtrField (CodeUnion**), which is CodeUnion*
    const codeUnionPtr = core.LLVMBuildLoad2(builder, codeUnionPtrTy, codeUnionPtrField, "pc_code_ptr_val");

    // Cast the CodeUnion* to a TagObject*
    const tagObjectPtr = core.LLVMBuildBitCast(builder, codeUnionPtr, tagObjectPtrTy, "tag_object_ptr");

    // Load the TagObject from that pointer
    _ = core.LLVMBuildLoad2(builder, tagObjectTy, tagObjectPtr, "object_val");

    // Return the tack pointer
    _ = core.LLVMBuildRet(builder, spParam);
}

pub fn createThreadedFnSig(module: types.LLVMModuleRef, fnName: [:0]const u8) !?*types.LLVMOpaqueValue {
    const ctx = core.LLVMGetGlobalContext();
    const typeInt64 = core.LLVMInt64TypeInContext(ctx);

    // Create a `TagObject` packed struct type
    const tagObjectTy = core.LLVMStructCreateNamed(ctx, "TagObject");
    var tagObjectFields = [_]types.LLVMTypeRef{
        core.LLVMIntTypeInContext(ctx, 3), // tag
        core.LLVMIntTypeInContext(ctx, 5), // class
        core.LLVMIntTypeInContext(ctx, 56), // hash
    };
    core.LLVMStructSetBody(tagObjectTy, &tagObjectFields[0], 3, 1);

    // Create a `Stack` non-packed struct type
    const stackTy = core.LLVMStructCreateNamed(ctx, "Stack");
    var stackFields = [_]types.LLVMTypeRef{
        tagObjectTy, // top
        tagObjectTy, // next
        tagObjectTy, // third
    };
    core.LLVMStructSetBody(stackTy, &stackFields[0], 3, 0);
    const stackPtrTy = core.LLVMPointerType(stackTy, 0);

    // Create a `Code` union struct
    const codeUnionTy = core.LLVMStructCreateNamed(ctx, "CodeUnion");
    const codeUntionField = core.LLVMArrayType(core.LLVMInt8TypeInContext(ctx), 8); // max size of union is 64-bits
    var codeUnionFields = [_]types.LLVMTypeRef{codeUntionField};
    core.LLVMStructSetBody(codeUnionTy, &codeUnionFields[0], 1, 0);
    const codeUnionPtr = core.LLVMPointerType(codeUnionTy, 0);

    // Create a `PC` packed struct
    const pcTy = core.LLVMStructCreateNamed(ctx, "PC");
    var pcFields = [_]types.LLVMTypeRef{codeUnionPtr}; // codeUnionPtr is a 'const' at the declaration site
    core.LLVMStructSetBody(pcTy, &pcFields[0], 1, 1);
    const pcPtrTy = core.LLVMPointerType(pcTy, 0);

    // 'Forward declaration' using opaque pointers
    const contextTy = core.LLVMStructCreateNamed(ctx, "Context");
    const contextPtrTy = core.LLVMPointerType(contextTy, 0);

    const compiledMethodTy = core.LLVMStructCreateNamed(ctx, "CompiledMethod");
    const compiledMethodPtrTy = core.LLVMPointerType(compiledMethodTy, 0);

    const processPtrTy = core.LLVMPointerType(core.LLVMVoidType(), 0);

    // ----- Create the `ThreadedFn` ------
    var paramTypes: [5]types.LLVMTypeRef = .{ pcPtrTy, stackPtrTy, processPtrTy, contextPtrTy, typeInt64 }; // change pcTy to pcPtrTy
    const threadedFnTy = core.LLVMFunctionType(stackPtrTy, &paramTypes[0], 5, 0);
    const threadedFnPtrTy = core.LLVMPointerType(threadedFnTy, 0);
    const threadedFn: types.LLVMValueRef = core.LLVMAddFunction(module, fnName, threadedFnTy);

    // Add the `CompiledMethod` fields
    var compiledMethodFields = [_]types.LLVMTypeRef{
        typeInt64, // heap header
        tagObjectTy, // stack structure
        typeInt64, // signature
        threadedFnPtrTy, // executionFn
        threadedFnPtrTy, // jitted
        core.LLVMArrayType(codeUnionTy, 1), // code
    };
    core.LLVMStructSetBody(compiledMethodTy, &compiledMethodFields[0], 6, 0);

    // Add the `Context` fields
    var contextFields = [_]types.LLVMTypeRef{
        typeInt64, // heapHeader
        compiledMethodPtrTy, // method
        pcTy, // tpc
        threadedFnPtrTy, // npc - represent as ptr (since threadedFn is NOT a datatype but a LLVM function definition + npc is represented internally as a pointer to a threadedFn on the zig side too
        contextPtrTy, // prevContext
        typeInt64, // trapContextNumber
        core.LLVMArrayType(tagObjectTy, 1), // temps
    };
    core.LLVMStructSetBody(contextTy, &contextFields[0], 7, 0);

    // Get the parameters
    const pcParam = core.LLVMGetParam(threadedFn, 0);
    const spParam = core.LLVMGetParam(threadedFn, 1);
    const processParam = core.LLVMGetParam(threadedFn, 2);
    const ctxParam = core.LLVMGetParam(threadedFn, 3);
    const sigParam = core.LLVMGetParam(threadedFn, 4);

    // Set the parameter names
    core.LLVMSetValueName(pcParam, "pc");
    core.LLVMSetValueName(spParam, "sp");
    core.LLVMSetValueName(processParam, "process");
    core.LLVMSetValueName(ctxParam, "context");
    core.LLVMSetValueName(sigParam, "signature");

    // // Only for testing purposes - enables emission of types textually in the IR
    // _ = core.LLVMBuildAlloca(builder, stackTy, "test-stack");
    // _ = core.LLVMBuildAlloca(builder, tagObjectTy, "test-tagObject");
    // _ = core.LLVMBuildAlloca(builder, codeUnionTy, "test-union");
    // _ = core.LLVMBuildAlloca(builder, contextTy, "test-context");
    // _ = core.LLVMBuildRet(builder, spParam); // only to adhere to the return type

    return threadedFn;
}

pub fn verifyModule(module: types.LLVMModuleRef) !void {
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
}

pub fn printModule(module: types.LLVMModuleRef) void {
    std.debug.print("\n--- IR DUMP ---\n", .{});
    core.LLVMDumpModule(module);
    std.debug.print("--- END IR ---\n\n", .{});
}

inline fn singleIndexGEP(builder: types.LLVMBuilderRef, elementType: types.LLVMTypeRef, base: types.LLVMValueRef, offset: i64, name: []const u8) types.LLVMValueRef {
    // singleIndex - for pointer and integer offsets
    const offset_bits: u64 = @bitCast(offset);
    const signExtend = offset < 0;
    const idx = [_]types.LLVMValueRef{core.LLVMConstInt(elementType, offset_bits, @intFromBool(signExtend))};
    const idx_ptr: [*c]types.LLVMValueRef = @constCast(@ptrCast(&idx[0]));
    return core.LLVMBuildGEP2(builder, elementType, base, idx_ptr, 1, @ptrCast(name));
}
