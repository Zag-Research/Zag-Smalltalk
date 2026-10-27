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
```
There are 3 times when a process checks to see if someone needs a stable/complete `Process` object:
1) On entry to a method,
2) a `loop` threaded word (from inlining),
3) completion (failure or otherwise) of an I/O or FFI.
It checks that by testing if `process.request == .normal`. If it's not `.normal` then special handling is required - no lock is required for this test. In the first 2 cases, the process is executing, so the `Process` fields are not in synch with the parameters that are passed through registers.

 There is one other related case, where in a primitive that is going to do an I/O operation or a FFI call, we will sync the states and then check the `process.request` to see whether it should do something before starting the I/O or FFI.

### GC Marking
If the request is `gcMark` then the GC thread  is asking all the threads to mark objects in the Global Arena which are reachable from the current process.
- entering a method or a `loop` branch  are both in running state. In this case we change the state to `gcMarking` and start walking the roots.
### Global Arena Collector thread
