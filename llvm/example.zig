const std = @import("std");
const llvm = @import("llvm");
const target = llvm.target;
const types = llvm.types;
const core = llvm.core;

//Build: `zig build -DExamples=true`

pub fn main() void {
    // Create a Zig stack
    var stack: [10]i32 = undefined;
    @memset(&stack, 0);
    stack[8] = 10;
    stack[9] = 20; // top of stack

    std.debug.print("\n\nStack before LLVM call: {any}\n", .{stack});

    try llvmGen(&stack);
    // Future: Use the ExecutionEngine to execute the generated IR,
    // then get a pointer to the globalStack and print it's contents in main()
}

pub fn llvmGen(stackPtr: *[10]i32) !void {

    // Initialization logic
    _ = target.LLVMInitializeNativeTarget();
    _ = target.LLVMInitializeNativeAsmPrinter();

    // LLVM module creates a global context by default
    const module: types.LLVMModuleRef = core.LLVMModuleCreateWithName("llvmModule");
    const builder: types.LLVMBuilderRef = core.LLVMCreateBuilder();

    const i32_type = core.LLVMInt32Type();
    const arr_type = core.LLVMArrayType(i32_type, 10);

    // Create a global variable for the stack ([10 x i32])
    const globalStack = core.LLVMAddGlobal(module, arr_type, "globalStack");
    core.LLVMSetLinkage(globalStack, types.LLVMLinkage.LLVMInternalLinkage); // internal-linkage is to only allow accessibility in the current module

    // Create a constant array to initialize the global storage array (the global stack)
    var constElems: [10]types.LLVMValueRef = undefined;
    for (0..constElems.len) |i| {
        const elem = &constElems[i];
        elem.* = core.LLVMConstInt(i32_type, @intCast(stackPtr[i]), 0);
    }
    const arrayInit = core.LLVMConstArray(i32_type, &constElems[0], 10);
    core.LLVMSetInitializer(globalStack, arrayInit);

    // Can now define functions that can manipulate this global stack within this module
    // Define function "manipulateStack" that adds top two elements on stack, pushes the result
    const fnType = core.LLVMFunctionType(core.LLVMVoidType(), null, 0, 0);
    const fnRef = core.LLVMAddFunction(module, "manipulateStack", fnType);
    const entryBB = core.LLVMAppendBasicBlock(fnRef, "entryBB");
    core.LLVMPositionBuilderAtEnd(builder, entryBB);

    // Get value of globalStack[8]
    const idx8 = [_]types.LLVMValueRef{ core.LLVMConstInt(i32_type, 0, 0), core.LLVMConstInt(i32_type, 8, 0) };
    const idx8_ptr: [*c]types.LLVMValueRef = @constCast(@ptrCast(&idx8[0]));
    const ptr8 = core.LLVMBuildGEP2(builder, i32_type, globalStack, idx8_ptr, @intCast(idx8.len), "ptr8");
    const val8 = core.LLVMBuildLoad2(builder, i32_type, ptr8, "val8");

    // Get value of globalStack[9]
    const idx9 = [_]types.LLVMValueRef{ core.LLVMConstInt(i32_type, 0, 0), core.LLVMConstInt(i32_type, 9, 0) };
    const idx9_ptr: [*c]types.LLVMValueRef = @constCast(@ptrCast(&idx9[0]));
    const ptr9 = core.LLVMBuildGEP2(builder, i32_type, globalStack, idx9_ptr, @intCast(idx8.len), "ptr9");
    const val9 = core.LLVMBuildLoad2(builder, i32_type, ptr9, "val9");

    // Compute the addition and store the result in globalStack[9]
    const sum = core.LLVMBuildAdd(builder, val8, val9, "sum");
    _ = core.LLVMBuildStore(builder, sum, ptr9);

    _ = core.LLVMBuildRetVoid(builder);

    // Cleanup
    core.LLVMDisposeBuilder(builder);
    return;
}
