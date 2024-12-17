## Threads
Smalltalk has processes, but they are typically only used for I/O and watchdog/idle processes. That said, we would like to have threads efficiently and correctly implement processes.

Zag maps threads/processes to Operating-System level threads.

There are shared data-structures that we don't want to protect with Mutex or even RWLocks. These include:
1. Garbage collection. Each thread has its own nursery heaps that don't overlap, but the main heap is shared, so anything that collects into that heap must synchronize.
2. The dispatch tables for method dispatch are shared and have to be updated when new code is compiled, so any update to them must be synchronized (they aren't modified in place, regardless, a new table is created and then put into place).
3. Any other structural update to classes must be synchronized.
4. When we eventually get to modifying object structure live, we will need to essentially do a garbage collection.

Threads/processes will synchronize at safe points. Every so-many message dispatches will mark a safe-point. Similarly, periodic points of in-lined loops will check for synchronization. This should be as cheap as possible.

### Execution/Mutator threads
Each execution thread has a locked structure that includes a condition variable. This condition variable is the only long-term wait that such a thread will ever block on.

There are several reasons for a thread to block:
1. It has collected a batch of roots for the global collector, and is waiting for them to be converted.
2. It needs to promote an object to the global space while a global collection is taking place, and is waiting for it to complete.
3. *Maybe* It  has hit a write barrier while global collection is taking place, and it waiting for it to complete.
4. It is waiting for an I/O thread to wake it up

### Input/Output Threads
In order for execution threads to be able to cooperate with the global collector thread they cannot block for I/O. Therefore every blockable operation serializes through an I/O thread.

### Global Arena Collector thread
