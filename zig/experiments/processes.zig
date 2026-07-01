const std = @import("std");
const builtin = @import("builtin");

pub const OsHandle = if (builtin.os.tag == .windows)
    std.os.windows.HANDLE
else
    std.c.pthread_t;

pub const Process = struct {
    next: ?*Process = null,
    os_target: ?OsHandle = null,
    threadId: std.Thread = undefined,
    id: u64 = undefined,
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
                process.threadId = thread;
                processId += 1;
                process.id = processId;
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
threadlocal var tls_process: Process align(1024) = undefined;

// The routine worker threads will run
fn workerRoutine() void {
    tls_process = Process{};
    const my_os_handle = if (builtin.os.tag == .windows)
        undefined // this is only set when we're doing I/O
    else
        std.c.pthread_self();

    // Perform the interlocking registration step
    GlobalRegistry.registerChild(&tls_process, my_os_handle);

    // Always clean up the pointer before thread destruction!
    defer GlobalRegistry.unregister(&tls_process);

    // Simulate the thread doing work
    std.Thread.sleep(50 * std.time.ns_per_ms);
}

fn printProc(proc: *Process) void {
    std.debug.print("Thread-Local Process Found -> ID: {}\n", .{proc.id});
}
pub fn main() !void {
    // Spawn 3 worker threads, each initializing their own copy of `tls_process`
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
