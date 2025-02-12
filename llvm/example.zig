const std = @import("std");
const llvm = @import("llvm");
const target = llvm.target;
const types = llvm.types;
const core = llvm.core;
const analysis = llvm.analysis;
const orc = llvm.orc;
const lljit = llvm.jit;

// Build: `zig build -DExamples=true`
// Run: `./zig-out/bin/example`

pub fn main() void {

    // Create downward growing stack - smaller indices for more pushes
    var stack: [5]i64 = .{ 0, 0, 0, 4, 5 };
    std.debug.print("Stack before: {any}\n", .{stack});
    _ = &stack[3]; // top is at index 3 -> 4

    // Create our module
    _ = try createModule();

    // Build an LLJIT instance (Orc)
    _ = try createLLJIT();

    // -------- WIP ---------
    // // Wrap our module in a ThreadSafeModule and add it to LLJIT
    // try addModuleToLLJIT(lljit, module_ref);

    // // Look up the symbol "stack_add_top_two"
    // const fn_ptr = try lookupAddTopTwo(jit, "stack_add_top_two");

    // // Call the JIT'd function from Zig
    // const new_sp = fn_ptr(sp);

    // // Inspect results
    // std.debug.print("After call:\n", .{});
    // std.debug.print("  sp offset = {d}\n", .{new_sp - &stack[0]});
    // std.debug.print("  stack = {any}\n", .{stack});
}

pub fn createModule() !types.LLVMModuleRef {

    // LLVM setup logic
    _ = target.LLVMInitializeNativeTarget();
    _ = target.LLVMInitializeNativeAsmPrinter();
    _ = target.LLVMInitializeNativeAsmParser();

    const i64Type: types.LLVMTypeRef = core.LLVMInt64Type();
    const i64PtrType: types.LLVMTypeRef = core.LLVMPointerType(i64Type, 0);

    // Create LLVM module (context created by default)
    const module: types.LLVMModuleRef = core.LLVMModuleCreateWithName("module");
    const builder: types.LLVMBuilderRef = core.LLVMCreateBuilder();

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

pub fn printTextualIR(module: types.LLVMModuleRef) void {
    const llvm_ir_c_str = core.LLVMPrintModuleToString(module);

    // Find the length of the null-terminated C string
    var length: usize = 0;
    while (llvm_ir_c_str[length] != 0) : (length += 1) {}

    // Create a Zig slice from the C string
    const llvm_ir_zig_str = llvm_ir_c_str[0..length];
    std.debug.print("Current LLVM IR:\n{s}\n", .{llvm_ir_zig_str});

    // Free the memory allocated for the IR string
    core.LLVMDisposeMessage(llvm_ir_c_str);
}

fn createLLJIT() !types.LLVMOrcLLJITRef {

    // Create an LLJITBuilder
    const builder = lljit.LLVMOrcCreateLLJITBuilder();
    if (builder == null) {
        return error.OOM;
    }

    // Optionally configure the builder (e.g., set target triple)...

    // Actually create the JIT
    var outJIT: types.LLVMOrcLLJITRef = undefined;

    const retCode = lljit.LLVMOrcCreateLLJIT(&outJIT, builder);
    if (retCode != 0) {
        // We failed building the LLJIT. Usually you’d retrieve an error string,
        // but the older c-api might not provide one easily.
        return error.JITError;
    }

    return outJIT;
}
