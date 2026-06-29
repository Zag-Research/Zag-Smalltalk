- [OpenSmalltalkVM threaded FFI](https://www.youtube.com/watch?v=-kDxpc_eQIE)
- [[Multi-threaded_OpenSmalltalk_VM-Choosing_a_Strategy_for_Parallelization.pdf]]
- see also [[Ideas]]

## How to connect to FFIs
# Conversation with Gemini

If I have an executing Zig program, how can I dynamically find the address of global functions. I also want to load dynamic libraries and find functions within those.

To dynamic-load and interact with symbols at runtime in Zig, you will use two different approaches depending on whether you are interacting with an external dynamic library or inspecting your currently executing binary.

## 1. Loading External Dynamic Libraries

For loading external libraries (`.so`, `.dll`, or `.dylib`), Zig provides a cross-platform helper in the standard library called `std.DynLib`.

When looking up functions, they must be exposed via the **C calling convention** (`callconv(.C)`) to match the standard ABI layout.

Code snippet

```
const std = @import("std");

pub fn main() !void {
    // 1. Open the dynamic library 
    // (Adjust extension based on OS: .so, .dll, or .dylib)
    var lib = try std.DynLib.open("path/to/libmath.so");
    defer lib.close();

    // 2. Define the exact function signature pointer using the C ABI
    const AddFn = *const fn (a: i32, b: i32) callconv(.C) i32;

    // 3. Look up the symbol by name
    if (lib.lookup(AddFn, "add")) |add_func| {
        const result = add_func(10, 20);
        std.debug.print("Result from external library: {}\n", .{result});
    } else {
        std.debug.print("Failed to find symbol 'add'\n", .{});
    }
}
```

> ⚠️ **Important Caveat:** If your dynamic library relies on standard system facilities (like printing or memory allocation), you must link your main Zig executable against `libc` using the `-lc` flag or `exe.linkLibC();` in your `build.zig`. Otherwise, the runtime relocation processing may trigger segmentation faults.

## 2. Finding Global Functions in the Current Executing Program

Finding functions inside your _own running binary_ requires specific compilation conditions:

1. **Explicit Exports:** Zig functions are not visible to dynamic lookups by default. You must mark the targets as `pub export fn` to push them into the binary's symbol table.
    
2. **Linker Instructions:** You must instruct the linker to expose symbols dynamically. In your `build.zig` file, make sure to enable the `rdynamic` option on your executable:
    
    Code snippet
    
    ```
    exe.root_module.rdynamic = true;
    ```
    

Because Zig strictly minimizes the OS bindings bundled directly in `std` to save binary space, you will query your current process's symbol table using clean, platform-specific `extern` declarations.

### On Linux / macOS (POSIX)

On POSIX environments, passing a `null` pointer to the standard dynamic linking loader (`dlopen`) instructs the OS to return a handle to the main executable itself.

Code snippet

```
const std = @import("std");

// Manually hook the native dynamic linking library hooks
extern "dl" fn dlopen(filename: ?[*:0]const u8, flags: c_int) callconv(.C) ?*anyopaque;
extern "dl" fn dlsym(handle: ?*anyopaque, symbol: [*:0]const u8) callconv(.C) ?*anyopaque;
extern "dl" fn dlclose(handle: ?*anyopaque) callconv(.C) c_int;

const RTLD_LAZY = 1;

// This function must be exported to exist in the dynamic symbol table
pub export fn targetGlobalFunction(x: i32) callconv(.C) void {
    std.debug.print("Successfully called global function with value: {}\n", .{x});
}

pub fn main() !void {
    // Passing null opens the current executing process image
    const handle = dlopen(null, RTLD_LAZY) or {
        std.debug.print("Could not open self image\n", .{});
        return;
    };
    defer _ = dlclose(handle);

    const FuncSig = *const fn (i32) callconv(.C) void;

    if (dlsym(handle, "targetGlobalFunction")) |sym| {
        const func: FuncSig = @ptrCast(sym);
        func(42);
    } else {
        std.debug.print("Symbol not found. Did you forget `exe.root_module.rdynamic = true`?\n", .{});
    }
}
```

### On Windows

On Windows, `GetModuleHandleW(null)` fetches the `HMODULE` handle targeting the primary `.exe` image currently driving the process. You can then feed this handle to `GetProcAddress`.

Code snippet

```
const std = @import("std");

// Bind to Win32 loader primitives
extern "kernel32" fn GetModuleHandleW(lpModuleName: ?[*:0]const u16) callconv(.winapi) ?*anyopaque;
extern "kernel32" fn GetProcAddress(hModule: *anyopaque, lpProcName: [*:0]const u8) callconv(.winapi) ?*anyopaque;

pub export fn windowsGlobalFunction() callconv(.C) void {
    std.debug.print("Hello dynamically from the running Windows binary!\n", .{});
}

pub fn main() !void {
    const handle = GetModuleHandleW(null) or {
        std.debug.print("Failed to acquire current module handle.\n", .{});
        return;
    };

    const FuncSig = *const fn () callconv(.C) void;

    if (GetProcAddress(handle, "windowsGlobalFunction")) |sym| {
        const func: FuncSig = @ptrCast(sym);
        func();
    } else {
        std.debug.print("Could not find the exported function symbol.\n", .{});
    }
}
```

Is it possible to set up an array of parameters (determined dynamically) and pass to the identified function?

Yes, it is possible, but **how** you do it depends entirely on whether you control the signature of the target functions.

Because Zig is a statically typed language, the compiler needs to know a function's exact parameter footprint at compile time to arrange the CPU registers and stack frame correctly. You cannot natively take a standard runtime array (like `[]i32`) and unpack it into a function that expects distinct arguments (like `fn(i32, i32, i32)`).

Here are the two primary ways to achieve this.

## Solution 1: The Tagged Union Slice (Recommended)

If you are designing a plugin system, scripting engine, or routing mechanism where you control the target functions, the cleanest "Zig way" is to pass a **slice of a tagged union**.

This allows you to dynamically assemble arguments of different types into a single array and pass that array directly.

Code snippet

```
const std = @import("std");

// 1. Define a variant type that can hold any permissible parameter
pub const Variant = union(enum) {
    int: i32,
    float: f64,
    string: []const u8,
};

// 2. Define the unified function signature
pub const DynamicFn = *const fn (args: []const Variant) callconv(.C) Variant;

// 3. Implement functions matching this signature
pub export fn dynamicAdd(args: []const Variant) callconv(.C) Variant {
    if (args.len < 2) return Variant{ .int = 0 };
    
    // Unpack dynamically with safety checks
    const a = args[0].int;
    const b = args[1].int;
    return Variant{ .int = a + b };
}

pub fn main() !void {
    // 4. Construct your parameters dynamically at runtime
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    const allocator = gpa.allocator();
    defer _ = gpa.deinit();

    var dynamic_args = std.ArrayList(Variant).init(allocator);
    defer dynamic_args.deinit();

    // Push parameters determined at runtime
    try dynamic_args.append(Variant{ .int = 15 });
    try dynamic_args.append(Variant{ .int = 27 });

    // 5. Call the function (pretending we resolved 'dynamicAdd' via dlsym)
    const func: DynamicFn = dynamicAdd;
    const result = func(dynamic_args.items);

    std.debug.print("Result: {}\n", .{result.int}); // Output: 42
}
```

## Solution 2: Foreign Function Interface (`libffi`)

If you **do not** control the target functions (e.g., you are loading a third-party C library containing functions like `fn open_connection(host: [*]const u8, port: c_int, timeout: c_int)`), a standard array will not work. You cannot unpack runtime data into native CPU registers or stack slots out-of-the-box.

To handle this, you must use **`libffi` (Foreign Function Interface)**. `libffi` is a C library specifically designed to assemble CPU call frames at runtime based on an array of types and values.

### How to use `libffi` in Zig:

1. Link `libffi` in your `build.zig` (`exe.linkSystemLibrary("ffi");`).
    
2. Import the C headers into Zig using `@cImport`.
    
3. Build an array of target argument types (`ffi_type`), an array of pointers to your data, and use `ffi_call`.
    

Here is a conceptual look at how `libffi` bridges this gap in Zig:

Code snippet

```
const std = @import("std");
const c = @cImport({
    @cInclude("ffi.h");
});

// A legacy third-party function we found dynamically
fn externalCFunction(a: i32, b: i32) callconv(.C) i32 {
    return a + b;
}

pub fn main() !void {
    // 1. Prepare the argument values somewhere in memory
    var arg1: i32 = 10;
    var arg2: i32 = 20;

    // 2. Setup an array of pointers pointing to those values
    var args = [_]?*anyopaque{ &arg1, &arg2 };

    // 3. Setup an array describing the TYPES of the arguments to the CPU
    var types = [_]*c.ffi_type{ &c.ffi_type_sint32, &c.ffi_type_sint32 };

    // 4. Initialize a "Call Interface" (CIF)
    var cif: c.ffi_cif = undefined;
    if (c.ffi_prep_cif(&cif, c.FFI_DEFAULT_ABI, 2, &c.ffi_type_sint32, &types[0]) == c.FFI_OK) {
        
        var rc: i32 = 0;
        const fn_ptr = @ptrCast(&externalCFunction);

        // 5. Perform the dynamic call. libffi handles the assembly/registers.
        c.ffi_call(&cif, fn_ptr, @ptrCast(&rc), &args[0]);

        std.debug.print("Dynamic call result: {}\n", .{rc}); // Output: 30
    }
}
```

### Summary Nudge

- Use **Solution 1** if you are writing the plugin API yourself. It is type-safe, native, fast, and cross-platform.
    
- Use **Solution 2** if you are building an interactive REPL, language interpreter, or bridge to uncompiled, arbitrary third-party C binaries.
    

  

Your Toronto Metropolitan University chats aren’t used to improve our models. Gemini is AI and can make mistakes. [Your privacy & Gemini Opens in a new window](https://support.google.com/gemini/answer/14620100?authuser=1)