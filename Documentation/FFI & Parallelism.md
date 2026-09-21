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
    

How big is the default stack for a thread in macos, linux, and windows?

The default stack size for a thread depends on the operating system, but it also heavily depends on whether you are looking at the **Main Thread** (created when the process starts) or a **Spawned Thread** (created later by your code).

## Default Stack Size Comparison

|Operating System|Main Thread Stack Size|Spawned Thread Stack Size|Controlled By|
|---|---|---|---|
|**Linux (glibc)**|**~8 MiB**|**~8 MiB**|System environment (`ulimit -s`)|
|**macOS**|**8 MiB**|**512 KiB**|Hardcoded OS constraints|
|**Windows**|**1 MiB**|**1 MiB**|Executable header (Linker default)|

## Operating System Breakdown

### Linux (GNU/glibc)

On standard Linux distributions using `glibc` (like Ubuntu, Fedora, and Debian), stack sizes are tied directly to the shell's resource limits.

- **The Mechanism:** You can view or change this limit for your current shell session by running `ulimit -s`. By default, it is almost always `8192` (which translates to **8 MiB**).
    
- When you spawn a background thread via `pthread_create`, the `glibc` implementation queries the environment's `RLIMIT_STACK` and gives the new thread an identical 8 MiB ceiling.
    
- **Exception:** Lightweight Linux distributions utilizing `musl` libc (like Alpine Linux) default to a vastly restricted **128 KiB** stack for spawned threads to minimize footprints.
    

### macOS

Apple treats the primary execution thread and worker threads as entirely separate entities.

- **The Difference:** The main thread gets a comfortable **8 MiB** playground. However, any subsequent background threads you spawn default to just **512 KiB**.
    
- This aggressive reduction frequently trips up developers porting applications from Linux to Mac. Programs that safely process large local arrays or utilize deep recursion on a background thread in Linux will instantly trigger a `Segmentation Fault / Stack Overflow` on macOS unless you manually override the thread attribute sizes using `pthread_attr_setstacksize`.
    

### Windows

Windows handles thread sizing based on the compiled binary itself rather than global OS profiles.

- **The Mechanism:** The **1 MiB** limit is the default target configured by standard compilers (like MSVC, Clang, or GCC/MinGW) and stamped directly into the executable's PE (Portable Executable) header metadata.
    
- When your app boots or invokes the Win32 `CreateThread` API, the Windows kernel looks at the binary's header to figure out how much space to reserve. It applies this exact same rule equally to both the main thread and background threads unless you explicitly supply a custom stack size flag during compilation or during the API call.
    

> 💡 **Virtual Memory vs. Physical Memory** Don't worry about an 8 MiB stack bloating your system's RAM consumption. These values refer strictly to **Virtual Address Space Reservation**. The operating system relies on _demand paging_—it allocates actual physical memory page-by-page (typically in 4 KiB blocks) only as your application pushes variables deeper onto the stack.

If you allocate thread local variables, where so they go in the 3 systems?

When you declare a thread-local variable (using `threadlocal` in Zig, `_Thread_local` in C, or `thread_local` in C++), it is **not** allocated on that thread's stack, nor is it placed in the standard global heap.

Instead, it goes into a dedicated memory region called the **Thread-Local Storage (TLS) block**.

To make this efficient, the CPU reserves a specific hardware register—called the **Thread Pointer (TP)**—to point directly to the active thread's control block. When your code accesses a thread-local variable, the CPU takes the address in that register and applies a pre-calculated offset to find the variable instantly.

The underlying implementation varies across operating systems:

## 1. Linux (ELF Binary Format)

On Linux, TLS is managed by the kernel, the compiler, and the C library (`glibc` or `musl`) following the ELF TLS ABI specification.

- **Where it goes:** The linker bundles all thread-local variables into special executable sections called `.tdata` (for initialized data) and `.tbss` (for zero-initialized data). When a new thread is spawned, the OS allocates a fresh block of memory combining a **Thread Control Block (TCB)** and a copy of those TLS templates.
    
- **The Pointer Mechanism:** On x86_64 architectures, Linux uses the **`FS` segment register** to point to the TCB. On ARM64, it uses the **`TPIDR_EL0`** register.
    
- **The Layout (Variant II):** On x86_64 Linux, the TLS data grows _downward_ (negatively) from the Thread Pointer. Your thread-local variables live at negative offsets relative to the address stored in the `FS` register (e.g., `FS:-4`, `FS:-8`).
    
- **Dynamic Loading (`dlopen`):** If a dynamic library with thread-local variables is loaded at runtime, Linux utilizes a **Dynamic Thread Vector (DTV)**—an array of pointers inside the TCB that points to lazily allocated TLS blocks for those dynamically loaded modules.
    

## 2. Windows (PE Binary Format)

Windows manages TLS via the kernel loader and exposes it through the subsystem architecture.

- **Where it goes:** The Windows linker compiles thread-local data into a `.tlss` section. When an application starts or a thread is created, the Windows kernel allocates a data block for that thread and initializes it.
    
- **The Pointer Mechanism:** Windows allocates a **Thread Environment Block (TEB)** for every thread. The CPU's **`GS` segment register** (on x64) or **`FS` register** (on x86) points directly to this TEB.
    
- **The Layout:** Inside the TEB structure, at a fixed offset (index `0x58` on x64), lives a pointer called the `ThreadLocalStoragePointer`. This pointer arrays out to the actual memory blocks allocated for the static TLS data of each loaded DLL and the main EXE.
    
- **Dynamic TLS:** If you hit the limits of static TLS or load DLLs dynamically, Windows switches to an array inside the TEB capable of holding up to 1,088 dynamic TLS slots, managed via the Win32 APIs `TlsAlloc` and `TlsGetValue`.
    

## 3. macOS (Mach-O Binary Format)

macOS implements a unique system known as **Thread Local Variables (TLVs)**, managed by the Mach-O loader (`dyld`) and `libpthread`.

- **Where it goes:** The Mach-O format defines a section called `__thread_vars` (which holds descriptors and initialization functions) and `__thread_data`/`__thread_bss` (which hold the raw templates).
    
- **The Pointer Mechanism:** Similar to Windows, macOS points the **`GS` segment register** (on x86_64) or **`TPIDR_EL0`** (on ARM64) to an internal OS-managed `pthread` structure representing the thread.
    
- **The Layout:** Rather than relying purely on fixed linker offsets, macOS uses a _descriptor-based_ approach. When you look up a thread-local variable, the code executes a tiny bootstrap function defined in the Mach-O binary. This function queries `dyld` to get the thread's specific allocation block for that variable, caching the resulting pointer offset in the thread's local storage slots allocated within the `pthread` structure.
    

## Quick Reference Summary

|Operating System|Architecture Register|Control Block|Primary Storage Layout|
|---|---|---|---|
|**Linux (x86_64)**|`FS`|Thread Control Block (TCB)|Negative byte offsets directly behind the TCB pointer.|
|**Windows (x64)**|`GS`|Thread Environment Block (TEB)|An array pointed to by `ThreadLocalStoragePointer` inside the TEB.|
|**macOS (Apple Silicon)**|`TPIDR_EL0`|`pthread` structure|Offset determined dynamically by `dyld` descriptors and cached per-thread.|

How much execution-time overhead is created by referencing a thread-local variable?

The execution-time overhead of referencing a thread-local variable ranges from **virtually zero** to **noticeable**, depending entirely on _where_ the variable is defined and _how_ your code was compiled.

Compilers use one of four different **TLS Access Models** depending on whether the variable lives inside the main executable or a dynamically loaded library (`.so`, `.dll`).

## The 4 Access Models and Their Overhead

### 1. Local Exec (LE) — Fastest

- **When it's used:** The variable is defined in the main executable and accessed only by the main executable.
    
- **Overhead:** **~0 to 1 CPU cycle.**
    
- **How it works:** The compiler calculates the variable's offset from the Thread Pointer at compile time. Accessing it is a single CPU instruction, exactly like accessing a standard global variable or reading from the stack.
    
- **x86_64 Assembly Example:**
    
    Code snippet
    
    ```
    mov rax, qword ptr fs:[-8]  ; Directly read from the Thread Pointer (fs) with a fixed offset
    ```
    

### 2. Initial Exec (IE) — Fast

- **When it's used:** The variable lives inside a dynamic library, but that library is linked at startup (not loaded later via `dlopen`).
    
- **Overhead:** **~1 to 3 CPU cycles.**
    
- **How it works:** Because the library is loaded at boot, the OS loader calculates the offset. The compiler can't hardcode it, so it has to look up the offset in the **Global Offset Table (GOT)** first, then add it to the Thread Pointer. This introduces one extra memory indirection.
    
- **x86_64 Assembly Example:**
    
    Code snippet
    
    ```
    mov rax, qword ptr [rip + g_variable@gottpoff] ; Look up the offset in the GOT
    mov rdx, qword ptr fs:[rax]                    ; Add to Thread Pointer and read
    ```
    

### 3. Local Dynamic (LD) & General Dynamic (GD) — Slowest

- **When it's used:** The variable lives inside a shared library that might be loaded dynamically at runtime via `dlopen()` or `LoadLibrary()`.
    
- **Overhead:** **~10 to 50+ CPU cycles.**
    
- **How it works:** The compiler has no idea where this variable will live in memory or if its thread block has even been allocated yet. It cannot use a direct register offset. Instead, it must call an OS/libc helper function (typically `__tls_get_addr`).
    
- **The Penalty:** This triggers a full function call. The CPU must save registers, push arguments, jump to the runtime linker code, check the Dynamic Thread Vector (DTV) array, and return. If the thread hasn't accessed this library's TLS before, `__tls_get_addr` may even allocate memory lazily on the spot.
    
- **x86_64 Assembly Example:**
    
    Code snippet
    
    ```
    lea rdi, [rip + g_variable@tlsgd] ; Pass variable descriptor
    call __tls_get_addr@PLT           ; Full function call to resolve address
    mov rax, qword ptr [rax]          ; Read the value
    ```
    

## Summary of Relative Performance

|TLS Model|Instructions Required|Memory Accesses|Relative Overhead|Best Used For|
|---|---|---|---|---|
|**Local Exec**|1|0 extra (Direct)|**Negligible**|Application-specific logic, performance-critical loops.|
|**Initial Exec**|2|1 extra (GOT read)|**Very Low**|Standard system libraries linked at launch.|
|**General Dynamic**|4+ (plus a `call`)|Multiple|**Moderate to High**|Dynamic plugins, hot-swappable modules.|

## Hardware and Microarchitectural Micro-Overheads

Even with the lightning-fast **Local Exec** model, there are subtle hardware taxations to keep in mind:

- **Segment Register Penalties:** On x86_64, accessing memory via the `FS` or `GS` segment registers can sometimes bypass the CPU's fastest execution pipeline paths or slightly increase instruction decoding complexity compared to a standard flat register lookup.
    
- **Cache Friendliness:** Because every thread has its own copy of the data, thread-local variables are inherently protected against **False Sharing** (where two threads fight over the same CPU cache line). This actually makes TLS significantly faster than shared global variables in multi-threaded code.
    
- **Pointer Cache Invalidation:** If you choose a Dynamic model, the branch predictors inside the CPU have to work harder to predict the jump to `__tls_get_addr`, which can occasionally cause minor pipeline stalls in tight, nested loops.
    

### The Takeaway

If you are referencing a `threadlocal` variable inside a heavy performance loop, ensure it's compiled with **Local Exec** (or ensure the compiler can optimize it by caching the address into a local stack variable _before_ entering the loop). If it is buried inside a dynamically loaded plugin, try to resolve the variable once and pass it around via a standard reference pointer.

If a thread is blocked on I/O, how can I get it to fail so I can restart it?

When a thread is blocked on synchronous kernel I/O (like reading from a socket, pipe, or file), it is put to sleep by the operating system. Because it isn't executing CPU instructions, **traditional termination flags (like an atomic boolean) will not work**, as the thread cannot check them.

To force a blocked thread to fail and unblock, you must trigger an event in the OS kernel that forces the system call to return prematurely. Here are the three main strategies to achieve this, ordered from direct interruption to structural fixes.

## Method 1: OS-Specific Interruption Primitives (Safest Direct Approach)

Both Windows and POSIX-compliant systems (Linux/macOS) provide native mechanisms to force an ongoing kernel I/O operation to abort.

### On Windows: `CancelIoEx`

Windows has an explicit, clean API for this scenario. If Thread A is blocked on a file or socket handle, Thread B can call `CancelIoEx` targeting that specific handle.

- **How it works:** You pass the file/socket handle to `CancelIoEx`. The Windows kernel instantly cancels all pending I/O operations for that handle.
    
- **The Result:** The blocking call on Thread A will instantly return `FALSE`. A call to `GetLastError()` will return `ERROR_OPERATION_ABORTED`. Your thread can then catch this error and gracefully exit or restart.
    

### On Linux & macOS: POSIX Signals (`EINTR`)

On Unix systems, you can interrupt system calls by sending a designated signal to the specific blocked thread using `pthread_kill`.

- **How it works:** 1. Register a dummy signal handler for a signal like `SIGUSR1` so it doesn't crash the entire process. 2. From Thread B, call `pthread_kill(thread_id, SIGUSR1)`.
    
- **The Result:** The kernel wakes up Thread A to execute the signal handler. Once the handler finishes, the blocking system call (like `read` or `recv`) aborts and returns `-1`, setting `errno` to `EINTR` (Interrupted System Call). In Zig, this maps to `error.Interrupted`.
    

## Method 2: Closing the Handle from Another Thread (The Brute Force)

If you have a thread blocked on a network socket or pipe, another thread can simply close that socket or pipe out from underneath it.

- **On Windows:** Closing a socket handle via `closesocket()` or a file handle via `CloseHandle()` while another thread is blocked on it will immediately force the blocking thread to return with an error code indicating the handle is invalid.
    
- **On Linux / macOS:** Closing a file descriptor (`close(fd)`) while a thread is blocked on it _usually_ forces the system call to return immediately with `EBADF` (Bad File Descriptor).
    

> ⚠️ **The Linux Race Condition Warning:** Relying on `close()` to break a blocked thread on POSIX systems introduces a severe race condition. The moment Thread B closes `fd 5`, that integer becomes available to the OS. If Thread C suddenly opens a completely unrelated file or socket, the OS might assign it `fd 5`. If Thread A wakes up slightly late, it might inadvertently start reading or writing data to Thread C's new file descriptor. Because of this, **`pthread_kill` is vastly preferred over `close()` on Unix systems.**

## Method 3: The Architectural Fix (Timeouts & Non-Blocking I/O)

If you find yourself frequently needing to force-fail threads to restart them, the underlying architecture should ideally pivot away from indefinite synchronous blocking.

### 1. Socket Timeouts

Instead of blocking forever, configure your handles with a maximum blocking duration using socket options (`SO_RCVTIMEO` and `SO_SNDTIMEO`).

Code snippet

```
// Conceptual Zig example setting a 5-second timeout on a socket descriptor
const os = std.os;
const timeout = os.timeval{ .tv_sec = 5, .tv_usec = 0 };
try os.setsockopt(socket_fd, os.SOL.SOCKET, os.SO.RCVTIMEO, std.mem.asBytes(&timeout));
```

With this set, the thread will automatically fail with `error.WouldBlock` or `error.Timeout` every 5 seconds, giving it a natural window to check a standard dynamic exit flag before resuming its I/O loop.

### 2. Polling / Event Multiplexing (`epoll`, `kqueue`, `IOCP`)

Instead of dedicating a thread to block on a single asset, make the file descriptor **non-blocking**. Use an OS event multiplexer to manage the wait state.

Under this architecture, your thread blocks on an event loop infrastructure rather than the raw raw data stream. When you want to tear down or restart the thread, you simply write a single byte to an internal "control pipe" or "eventfd" linked to that loop. The multiplexer wakes up instantly, detects the command to shut down, and allows the thread to cycle safely out of execution without relying on kernel aborts.er/14620100?authuser=1)