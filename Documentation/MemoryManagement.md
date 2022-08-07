# Memory Management


Safe and efficient memory management is an essential part of a Smalltalk system. While the original Smalltalk system approach of creating activation records as heap objects has been replaced with stack allocation, idiomatic Smalltalk still creates a lot of short-term objects that need to be collected.

AST Smalltalk has several features that minimize garbage creation:
- activation records (contexts) are stack allocated
- `nil`, `true`, `false`, Symbols, Characters, SmallIntegers and Floats are all encoded as immediate values
- SmallIntegers have a wide range (2^51) so extension to BigIntegers (which would be heap-allocated) is rare
- BlockClosures are stack allocated
- code blocks are generated outside the heap

AST Smalltalk has several features that minimize the amount of work required by garbage collection:
- all interpreter stacks and code addresses are outside the range of valid heap pointers, so by being careful to never save anything in a stack frame that looks like a heap reference, no scanning is required for the interpreter stack;
- scanning for roots can proceed very quickly. A single comparison filters all non-heap, non-float values, and if necessary a second filters the floats
- the format of indexable objects encodes if they are pointer-free, so don't need to be scanned
- each thread has its own nursery and teen heaps, so there is no contention on allocation
- the list of weak objects that must be scanned at the end of the collection is outside the heap

## Heap structure

The heap is structured as per-execution-thread arenas (accessible only by the execution thread itself and the global collector thread) and a global arena accessible by all threads.

## Per-Thread Arenas
Each thread/process has its own nursery heap, typically about 8kib. All allocations are done in the nursery except for large objects that would not fit. The heap grows up. When there is no room for the current allocation, the heap will be collected to the teen arena. Because these are thread-private, there is no locking required for allocation in the nursery or teen arenas. The teen arena is several times larger than the nursery.

The nursery and teen arenas are both collected using a copying collector. Copying collectors are very fast when a significant portion of the content is garbage, because they only examine the live content of the heap. The roots for collection are the stack of contexts.

If, after an allocation in the teen arena (from a nursery collection, or a large-object allocation), there is not enough free space in the teen arena for a full nursery collection, then the nursery arena will be collected into the teen arena and then the teen arena will be collected. This is collected into the other teen arena, and if there still isn't enough space for a full copy from the nursery, then the teen arena will be copied into the shared GlobalArena, so it must obtain a lock. The actual collection is done with a copying collector as, (a) much of the content is likely garbage, and (b) we must have room into which to copy the nursery.

### Copying Collector
Copying collectors are much faster if you mostly have garbage. Every BlockClosure or temporary array you create almost instantly becomes garbage, so a typical minor collect might be only 10-20% live data ^[experimental data to follow]. Even more importantly, allocations are practically free, just bump a pointer. They are also very cache friendly. This means they are ideal for a per-thread arena, where no locks are required. This is why Zag uses this for per-thread arenas.

## Global Arena
The GlobalArena is the eventual repository for all live data. One invariant is that objects in the GlobalArena cannot reference objects in a nursery or teen arena, so such objects must be promoted to the GlobalArena (this is the reason for a nursery collection before any teen collection). There are several sources of this data:
1. collections from thread teen arenas
2. the symbol table
3. the strings that have the representation of symbols
4. dispatch tables
5. class objects
6. large allocations
7. setting a field in an existing global object to a reference to a local object requires that the local object be promoted (leaving behind forwarding pointers)
8. Contexts spilled from the stack (from deep recursion)

Roots for the GlobalArena include:
1. the symbol table reference
2. the class table reference
3. roots from all threads
4. the old-dispatch reference (if there is a parallel global collector running) **ToDo: I don't know what this referenced**

If you have an arena that is accessible to multiple threads, then moving becomes a big deal - you'd have to stop all threads to move anything, and you can't collect in parallel. So here, Zag uses a mark and sweep collector that doesn't move anything once allocated. Any allocation here requires a lock, but is otherwise very fast. Zag uses a similar allocation scheme to [Mist](https://github.com/martinmcclure/mist) - just using Fibonacci numbers instead of powers of 2. This means almost no space is wasted, versus with powers-of-2 (like Mist), on average 1/4 of memory is wasted. Free space is easily coalesced in the sweep phase.

The Global Arena uses a non-moving mark and sweep collector. There is a dedicated thread that periodically does a garbage collect.

### Global Arena Structure
The Global Arena uses a Fibonacci heap

#### Free-space allocation
Free-space is split up into fibonacci-number-sized pieces and put on the appropriate queue:
- 1-word free-space is not allocated on any queue. 
- 4-word free-space is allocated as 2 entries on the 2-queue. 
- Otherwise, free-space is split into two pieces: the largest fibonacci number that will fit, which is put on the appropriate queue, and loop to allocate the rest.

## Large data allocation
For objects of 4094^[this exact size will be tuned with experience and may become smaller] words or more, separate pages are allocated for each object. This allows them to be separately freed when they are no longer accessible. This prevents internal memory leaks. It also supports mapping large files, so for example a "read whole file" for anything large will simply map the file as an indirect string, and for anything smaller allocate the string and read the data into it.

The objects with large allocations are linked together so that at the start of the sweep of the global arena we can go though this list finding all the unused objects that have large allocations and free up their allocation.

## Notes
- when the stack is being copied, 
## Web resources
- [Boehm MS vs Copying](https://hboehm.info/gc/complexity.html)
- [Cornell course copying](http://www.cs.cornell.edu/courses/cs312/2003fa/lectures/sec24.htm)
- [uta copying](https://lambda.uta.edu/cse5317/notes/node48.html)
- [GNU Smalltalk partially-copying collector](https://www.gnu.org/software/smalltalk/manual/html_node/GC.html)