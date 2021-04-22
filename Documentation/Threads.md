## Threads
One of the features of Rust is that it has a clean model for using multiple threads as a form of multi-processing.

Smalltalk has processes, but they are typically only used for I/O andwatchdog/idle processes. That said, we would like to have threads efficiently and correctly implement processes.

There are shared data-structures that we don't want to protect with Mutex or even RWLocks. These include:
1. Garbage collection. Each thread has its own nursery heaps that don't overlap, but the main heap is shared, so anything that collects into that heap must synchronize.
2. The dispatch tables for method dispatch are shared and have to be updated when new code is executed, so any update to them must be synchronized (they aren't modified in place, regardless, a new table is created and then put into place).
3. And other update to class structures must be synchronized.
4. When we eventually get to modifying object structure live, we will need to essentially do a garbage collection.

Threads/processes will synchronize at safe points. Every so-many message dispatches will mark a safe-point. Similarly, periodic points of in-lined loops will check for synchronization. This should be as cheap as possible