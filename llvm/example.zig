const std = @import("std");
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

    // LLVM setup logic
    _ = target.LLVMInitializeNativeTarget();
    _ = target.LLVMInitializeNativeAsmPrinter();
    _ = target.LLVMInitializeNativeAsmParser();

    // Create downward growing stack - smaller indices for more pushes
    var stack: [5]i64 = .{ 0, 0, 0, 4, 5 };
    std.debug.print("Stack before: {any}\n", .{stack});
    _ = &stack[3]; // top is at index 3 = value 4

    // Create LLVM module (context created by default)
    const module: types.LLVMModuleRef = core.LLVMModuleCreateWithName("module");
    const builder: types.LLVMBuilderRef = core.LLVMCreateBuilder();

    // Create our module
    _ = try createModule(module, builder);

    // Create the LLJIT Builder
    const jitBuilder = lljit.LLVMOrcCreateLLJITBuilder();

    // Create the LLJIT Instance
    var jit: types.LLVMOrcLLJITRef = undefined;
    if (lljit.LLVMOrcCreateLLJIT(&jit, jitBuilder) != null) {
        std.debug.print("Failed to create LLJIT\n", .{});
        return;
    }

    // Add IR module to JIT instance
    const dylib = lljit.LLVMOrcLLJITGetMainJITDylib(jit);
    if (dylib == null) {
        std.debug.print("Failed to get the main JIT dylib.\n", .{});
    }
    const threadRef = orc.LLVMOrcCreateNewThreadSafeContext();
    const threadSafeModule = orc.LLVMOrcCreateNewThreadSafeModule(module, threadRef);

    if (lljit.LLVMOrcLLJITAddLLVMIRModule(jit, dylib, threadSafeModule) != null) {
        std.debug.print("Failed to add module to JIT\n", .{});
        return;
    }

    // Look up synbol to execute
    var result: orc.LLVMOrcExecutorAddress = 0;
    if (lljit.LLVMOrcLLJITLookup(jit, &result, "stack_add_top_two") != null) {
        std.debug.print("Failed to lookup symbol\n", .{});
        return;
    }

    const stackAddTopTwoFn: *const fn (*i64) callconv(.C) *i64 = @ptrFromInt(result);
    const newSp = stackAddTopTwoFn(&stack[3]);
    const valueAtNewSp: *const i64 = @ptrCast(newSp);
    std.debug.print("Value at stack pointer: {}\n", .{valueAtNewSp.*});
}

pub fn createModule(module: types.LLVMModuleRef, builder: types.LLVMBuilderRef) !types.LLVMModuleRef {
    const i64Type: types.LLVMTypeRef = core.LLVMInt64Type();
    const i64PtrType: types.LLVMTypeRef = core.LLVMPointerType(i64Type, 0);

    // Create the addition_func function
    var paramTypes: [1]types.LLVMTypeRef = .{i64PtrType};
    const funcType: types.LLVMTypeRef = core.LLVMFunctionType(i64PtrType, &paramTypes[0], 1, 0);
    const fnAddTopTwo: types.LLVMValueRef = core.LLVMAddFunction(module, "stack_add_top_two", funcType);

    // Set param name
    const spParam: types.LLVMValueRef = core.LLVMGetParam(fnAddTopTwo, 0);
    core.LLVMSetValueName(spParam, "sp");

    // Setup the function body
    const entryBlock: types.LLVMBasicBlockRef = core.LLVMAppendBasicBlock(fnAddTopTwo, "entry");
    core.LLVMPositionBuilderAtEnd(builder, entryBlock);

    // Load top stack element, update sp
    const val1 = core.LLVMBuildLoad2(builder, i64Type, spParam, "val1");
    const sp_1 = buildGEP(builder, i64Type, spParam, 1, "sp_1");

    // Load the next element, update sp
    const val2 = core.LLVMBuildLoad2(builder, i64Type, sp_1, "val2");
    const sp_2 = buildGEP(builder, i64Type, sp_1, 1, "sp_2");

    // Perform Add
    const sumVal = core.LLVMBuildAdd(builder, val1, val2, "sumVal");

    // Push sumVal on the stack
    const sp_3 = buildGEP(builder, i64Type, sp_2, -1, "sp_3");
    _ = core.LLVMBuildStore(builder, sumVal, sp_3);

    _ = core.LLVMBuildRet(builder, sp_3);

    // Verify the module and capture the message
    var errorMessage: ?[*:0]u8 = null;
    if (analysis.LLVMVerifyModule(module, types.LLVMVerifierFailureAction.LLVMPrintMessageAction, &errorMessage) != 0) {
        if (errorMessage) |msg| {
            std.debug.print("Verification failed: {s}\n", .{msg});
            core.LLVMDisposeMessage(msg); // Free the error message
        } else {
            std.debug.print("Verification failed, but no message was provided.\n", .{});
        }
    } else {
        std.debug.print("Module verification passed.\n", .{});
    }

    std.debug.print("\n--- ORC JIT IR DUMP ---\n", .{});
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
