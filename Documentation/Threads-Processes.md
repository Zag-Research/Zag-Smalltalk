## Threads
Smalltalk has processes, but they are typically only used for I/O and watchdog/idle processes. That said, we would like to have threads efficiently and correctly implement processes.

Zag maps threads/processes to Operating-System level threads.

There are shared data-structures that we don't want to protect with Mutex or even RWLocks. These include:
1. Garbage collection. Each thread has its own nursery heaps that don't overlap, but the main heap is shared, so anything that collects into that heap must synchronize.
2. The dispatch tables for method dispatch are shared and have to be updated when new code is compiled, so any update to them must be synchronized (they aren't modified in place, regardless, a new table is created and then put into place).
3. Any other structural update to classes must be synchronized.
4. When we eventually get to modifying object structure live, we will need to essentially do a garbage collection.

Threads/processes will synchronize at safe points. Every message dispatch marks a safe-point. Similarly, the `loop` word that closes in-lined loops will check for synchronization. This is simply a check of `thisProcess.request == .normal`.

### Execution/Mutator/IO threads/processes
Each thread has a `threadlocal` `Process` structure:
```zig
const ProcessStatus = enum {
	running, // thread is actually executing
	blocked, // waiting on I/O or calling some FFI
	paused, // waiting for return to .normal
	exited, // thread has finished
};

const ProcessRequest = enum {
	normal, // thread is alternating between running and blocking
	quit, // thread is asked to quit - if blocked, interrupted
	save, // thread is asked to save the process object to the image
	gcPause, //

};
```
On entry to a method, a `loop` threaded word (from inlining), or completion (failure or otherwise) of an I/O or FFI, `process.request == .normal` is checked. If it's not `.normal` then special handling is required
### Global Arena Collector thread
