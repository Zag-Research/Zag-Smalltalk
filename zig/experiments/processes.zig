const std = @import("std");
const builtin = @import("builtin");
const assert = std.debug.assert;
//const largerPowerOf2 = zag.utilities.largerPowerOf2;
inline fn bitsToRepresent(value: anytype) u7 {
    const T = @TypeOf(value);
    switch (@typeInfo(T)) {
        .comptime_int => {
            comptime var n = value;
            n |= n >> 32;
            n |= n >> 16;
            n |= n >> 8;
            n |= n >> 4;
            n |= n >> 2;
            n |= n >> 1;
            return comptime @ctz(~@as(u64, n));
        },
        .int => |int_info| switch (int_info.signedness) {
            .unsigned => return @intCast(int_info.bits - @clz(value)),
            else => {},
        },
        else => {},
    }
    @compileError("bitsToRepresent not implemented for " ++ @typeName(T));
}
inline fn largerPowerOf2(value: anytype) u64 {
    if (value <= 1) return 1;
    const bits = bitsToRepresent(value - 1);
    return @as(u64, 1) << @as(u6, @intCast(bits));
}
const Context = struct {
    fn initStatic(_: *Context) void {}
};
const Object = u64;
const HeapObject = struct {
    header: u64,
    inline fn fromObjectPtr(op: [*]const Object) HeapObjectArray {
        return @ptrFromInt(@intFromPtr(op));
    }
};
const HeapObjectArray = [*]HeapObject;
const SP = [*]Object;

pub const OsHandle = if (builtin.os.tag == .windows)
    std.os.windows.HANDLE
else
    std.c.pthread_t;
const process_total_size = 64 * 1024;

const Process = @This();
next: ?*Process = null,
os_target: ?OsHandle = null,
threadId: std.Thread = undefined,
id: u64 = undefined,
trapContextNumber: u64 = 0,
status: ProcessStatus = .running,
request: ProcessRequest = .normal,
context: *Context = undefined,
currHeap: HeapObjectArray = undefined,
currHp: HeapObjectArray = undefined,
currEnd: HeapObjectArray = undefined,
otherHeap: HeapObjectArray = undefined,
sp: [*]Object = undefined,
staticContext: Context = undefined,
// ignored: u64 = 0,
nursery0: [process_nursery_size]HeapObject = undefined,
nursery1: [process_nursery_size]HeapObject = undefined,
_fill: [fill_size]u64 = undefined,
stack: [process_stack_size]Object = undefined,
comptime {
    assert(process_stack_size < process_nursery_size);
    // @compileLog(@sizeOf(Process));
    // @compileLog(process_total_size);
    assert(@sizeOf(Process) == process_total_size - 8);
}
const threadlocalOffset = 8;
const fields_size = 12 * 8 + @sizeOf(Context) + threadlocalOffset;
const processAvail = (process_total_size - fields_size) / @sizeOf(Object);
const approx_nursery_size = (processAvail - processAvail / 16) / 2;
const approx_stack_size = processAvail - approx_nursery_size * 2;
const process_stack_size: usize = largerPowerOf2(approx_stack_size) - 1;
const process_nursery_size = (processAvail - process_stack_size) / 2;
const fill_size = processAvail - process_stack_size - process_nursery_size * 2;
const stack_mask_overflow: usize = largerPowerOf2(process_stack_size * @sizeOf(Object));
pub const stack_mask = stack_mask_overflow - @sizeOf(Object);
pub const stack_mask_shift = @ctz(stack_mask_overflow);
pub const StackMask = @import("std").meta.Int(.unsigned, stack_mask_shift);
fn init(self: *Process, threadId: std.Thread, id: u64) void {
    self.threadId = threadId;
    self.id = id;
    self.currHeap = HeapObject.fromObjectPtr(@ptrCast(&self.nursery0));
    self.currEnd = self.currHeap + process_nursery_size;
    self.currHp = self.currHeap;
    self.otherHeap = HeapObject.fromObjectPtr(@ptrCast(&self.nursery1));
    self.context = &self.staticContext;
    self.staticContext.initStatic();
    self.sp = self.endOfStack();
    if (@intFromPtr(&self.stack[0]) & stack_mask != 0) @panic("stack not properly aligned");
}
pub inline fn endOfStack(self: *Process) SP {
    return @ptrCast(@as([*]Object, @ptrCast(&self.stack[0])) + process_stack_size);
}
const ProcessStatus = enum {
    running, // thread is actually executing
    blocked, // waiting on I/O or calling some FFI
    gcMarking, // the process is marking reachable objects
    waiting, // waiting for return to .normal
    exited, // thread has finished
};

const ProcessRequest = enum {
    normal, // thread is alternating between running and blocking
    quit, // thread is asked to quit - if blocked, interrupted
    save, // thread is asked to save the process object to the image
    gcMark, // the GC thread is asking the process to mark reachable globals

};
pub const GlobalRegistry = struct {
    // Struct-local static variables (Singletons)
    var head: ?*Process = null;
    var mutex: std.Thread.Mutex = .{};
    var cond: std.Thread.Condition = .{};
    var spawn_mutex: std.Thread.Mutex = .{};
    var processId: u64 = 0;

    // Interlock state to track if the newly spawned thread has checked in
    var child_process: ?*Process = null;

    /// Spawns a new thread and blocks until that thread has successfully
    /// populated its thread-local structure and linked itself to the head.
    pub fn spawnWithInterlock(comptime worker_fn: anytype) !*Process {
        // Lock the spawning pipeline. This is NOT released by cond.wait!
        spawn_mutex.lock();
        defer spawn_mutex.unlock();
        // Lock for the condition of the new thread registering. This IS released by cond.wait!
        mutex.lock();
        defer mutex.unlock();

        // Reset the interlock handshake flag
        child_process = null;

        // Spawn the thread
        const thread = try std.Thread.spawn(.{}, worker_fn, .{});

        // Loop until the child wakes up and sets `child_process`
        // Note that we need the loop because *occasionally* cond.wait will return spuriously
        // this is a performance trade-off deep inside OS logic
        while (true) {
            if (child_process) |process| {
                processId += 1;
                process.init(thread, processId);
                return process;
            }
            cond.wait(&mutex);
        }
    }

    /// Called by the newly spawned thread to pass its threadlocal storage node
    pub fn registerChild(node: *Process, os_handle: OsHandle) void {
        mutex.lock();
        defer mutex.unlock();

        // Populate the node and push to the head of the list
        node.os_target = os_handle;
        node.next = head;
        head = node;

        // Set the flag and signal the parent to wake up and release the mutex
        child_process = node;
        cond.signal();
    }

    /// Safely unlinks a thread-local node when a thread terminates
    pub fn unregister(node: *Process) void {
        mutex.lock();
        defer mutex.unlock();

        var current = &head;
        while (current.*) |item| {
            if (item == node) {
                current.* = item.next;
                return;
            }
            current = &item.next;
        }
    }

    /// Checks if there are any registered processes
    pub fn anyProcesses() bool {
        return head != null;
    }

    /// Aborts I/O that a process is performing
    pub fn interruptProcess(process: *Process) void {
        mutex.lock();
        defer mutex.unlock();

        const handle = process.os_target orelse @panic("process not initialized");

        if (builtin.os.tag == .windows) {
            _ = std.os.windows.kernel32.CancelIoEx(handle, null);
        } else {
            _ = std.c.pthread_kill(handle, 23); // SIGURG
        }
    }

    /// Executed by an outside inspector thread to iterate over every Process
    /// Note that the GlobalRegistry mutex is locked for the entire iteration
    pub fn iterate(aFn: *const fn (*Process) void) void {
        mutex.lock();
        defer mutex.unlock();

        var current = head;
        while (current) |proc| {
            aFn(proc);
            current = proc.next;
        }
    }
};

// Every thread that runs will get its own separate instantiation of this memory slot.
threadlocal var thisProcess: Process = undefined;

// The routine worker threads will run
fn workerRoutine() void {
    thisProcess = Process{};
    const my_os_handle = if (builtin.os.tag == .windows)
        undefined // this is only set when we're doing I/O
    else
        std.c.pthread_self();

    // Perform the interlocking registration step
    GlobalRegistry.registerChild(&thisProcess, my_os_handle);

    // Always clean up the pointer before thread destruction!
    defer GlobalRegistry.unregister(&thisProcess);

    // Simulate the thread doing work
    std.Thread.sleep(50 * std.time.ns_per_ms);
}

fn printProc(proc: *Process) void {
    std.debug.print("Thread-Local Process Found -> ID: {}\n", .{proc.id});
}
pub fn main() !void {
    // Spawn 3 worker threads, each initializing their own copy of `thisProcess`
    const t1 = try GlobalRegistry.spawnWithInterlock(workerRoutine);
    if (false) GlobalRegistry.interruptProcess(t1);
    _ = try GlobalRegistry.spawnWithInterlock(workerRoutine);
    _ = try GlobalRegistry.spawnWithInterlock(workerRoutine);

    // Let the threads boot up and register themselves
    std.Thread.sleep(10 * std.time.ns_per_ms);

    // From the MAIN thread, we can now cross boundaries and iterate through
    // the variables residing inside the other threads' TLS blocks!
    std.debug.print("--- Current Active Thread Processes ---\n", .{});
    GlobalRegistry.iterate(&printProc);

    // Clean up and wait for threads to finish
    while (GlobalRegistry.anyProcesses())
        std.Thread.sleep(10 * std.time.ns_per_ms);
}
