# Memory Management


Safe and efficient memory management is an essential part of a Smalltalk system. While the original Smalltalk system approach of creating activation records as heap objects has been replaced with stack allocation, idiomatic Smalltalk still creates a lot of short-term objects that need to be collected.

Zag Smalltalk has several features that minimize garbage creation:
- activation records (contexts) are stack allocated
- `nil`, `true`, `false`, Symbols, Characters, SmallIntegers and most Floats are encoded as immediate values
- SmallIntegers have a wide range (2^55) so extension to BigIntegers (which would be heap-allocated) is rare
- BlockClosures are stack allocated
- JITed code blocks are generated outside the heap
- several kinds of common BlockClosures are encoded as immediate values

Zag Smalltalk also has several features that minimize the amount of work required by garbage collection:
- all execution stacks and code addresses are outside the range of valid heap pointers, so by being careful to never save anything in a stack frame that looks like a heap reference, no scanning is required for the stacks;
- scanning for roots can proceed very quickly (a single comparison filters all non-heap values);
- the format of indexable objects encodes if they are pointer-free, so don't need to be scanned;
- each process has its own nursery heap, so there is no contention on allocation or nursery collection;
- the list of weak objects that must be scanned at the end of the collection is outside the heap.

## Heap structure

The heap is structured as per-process arenas (accessible only by the execution process itself) and a global arena accessible by all processes.

## Per-Process Arenas
Each thread/process has its own nursery heap pair, typically about 30kib each. All allocations are done in the nursery except for large objects that would not fit and `CompiledMethod` objects. The heap grows up. When there is no room for the current allocation, the heap will be collected to the other arena. Because these are process-private, there is no locking required for allocation in the nursery arena, nor for collection.

The nursery is collected using a copying collector. Copying collectors are very fast when a significant portion of the content is garbage, because they only examine the live content of the heap. The roots for collection are the stack and current context.

If, for an allocation in the nursery, there is not enough free space in the current arena, then the nursery arena will be collected into the other nursery arena, keeping track of how much space is occupied by each age. If there still isn't enough extra space, then the arena will be copied back to the first one, promoting enough older objects to the global arena to make space. Copying into the shared GlobalArena, requires obtaining a lock, but until this point all activity is happening within a process, so no locks are required. The actual collection is done with a copying collector as much of the content is likely garbage.

### Stack of Contexts
The execution stack is allocated in the stack area of each Process, and grows down.  If insufficient space is available then the contexts will be reified and copied to the heap. Since the stack area is at the beginning of the Process structure, it is very cheap to check for stack overflow.

The stack pointer points to the top of the working stack. If the current context (which could be ours or a sender's) is on the stack (which can be determined from the age field in the header), the working stack area is from the stack pointer to the context (technically there may be some closures in front of the context - these aren't technically part of the working stack and will be spilled to the heap if the context is spilled). If the current context is not in the stack area, then the working stack area is from the stack pointer to the end of the stack area.

On entry to a method, the working stack is that of the sender, and the contents will include `self` and the parameters to the current method (in reverse order as they are pushed in left-to-right order) and below that the remaining working stack of the sender. If the current method starts with a primitive, the primitive will work with those values; if successful, replacing `self` and the parameters with the result and then returning to the sender. Otherwise, if the current method sends any non-tail messages, creates any closures (except for a few immediate types), or captures `thisContext`, then a Context object is partially created, capturing the whole working stack of the sender, and setting the ContextPtr and stack pointer to the newly created context. See [[Execution]] for more details. This `Context` will be created as late as possible, because after inlining, there may be paths through the method that don't require the `Context`. The partial creation is because a) we create a lot of them, so we want it to be as cheap as possible; b) it is likely to go away, rather than be spilled; c) it's on the stack so we can calculate its structure if we have to reify it. It is partial in the sense that the size is not calculated.

### Block Closures
Block closures are allocated "on top" of the `Context`. Each closure that does a non-local return, or references local variables stored in the `Context`, will have a reference to the `Context`. When a closure is created and the `Context` is on the stack, the resulting object will be created on the stack. But if the `Context` is on the heap, the closure will be allocated on the heap.
### Copying Collector
Copying collectors are much faster than mark-and-sweep collectors if you mostly have garbage. Every BlockClosure or temporary array you create almost instantly becomes garbage, so a typical minor collect might be only 10-20% live data^[experimental data to follow]. Even more importantly, allocations are practically free, just bump a pointer. They are also very cache friendly. This means they are ideal for a per-process arena, where no locks are required. This is why Zag uses this for [[Garbage Collection#Per-process arenas|per-process arena collection]].

## Global Arena
The GlobalArena is the eventual repository for all live data. One invariant is that objects in the GlobalArena cannot reference objects in a process-private area which means a stack or nursery arena, so such referenced objects must be promoted to the GlobalArena. There are several sources of objects in the global arena:
1. promotions from process arenas
2. the symbol table
3. the strings that have the representation of symbols
4. dispatch tables & generated code (threaded and native execution)
5. class objects and method ASTs
6. large allocations
7. setting a field in an existing global object to a reference to a local object requires that the local object be promoted (leaving behind forwarding pointers)
8. Contexts spilled from the stack (e.g., from deep recursion)

Roots for the GlobalArena include:
1. the symbol table reference
2. the class table reference
3. roots from all processes
4. the dispatch tables
5. the old-dispatch reference (if there is a parallel global collector running) **ToDo: I don't know what this referenced**

If you have an arena that is accessible to multiple processes, then moving becomes a big deal - you'd have to stop all processes to move anything, and you can't collect in parallel. The production Java systems do this by partitioning the heap and only locking one section at a time, copying that into a new area and then letting things proceed; this works, but we think we can do better. So here, Zag uses a mark and sweep collector that doesn't move anything once allocated. Any allocation here requires a lock, but is otherwise very fast. Zag uses a similar allocation scheme to [Mist](https://github.com/martinmcclure/mist). Free space is easily coalesced in the sweep phase.

The Global Arena uses a non-moving mark and sweep collector. There is a dedicated OS thread that periodically does a [[Garbage Collection#Global arenas|global collection]].

When promoting an object to the global arena or creating a new object in the global arena if the global collector is currently marking, the age will be set to marked and scanned, otherwise it will be set to unmarked.

#### Object age fields
The age field for global objects is as follows:

| Value | Meaning                     | Notes                         |
| ----- | --------------------------- | ----------------------------- |
| 0-6   | stack or per-process-heap   | see [[Mapping#Age]]           |
| 7     | static                      | outside the stack or any heap |
| 8     | global                      |                               |
| 9     | global, marked as reachable |                               |
| 10    | a Zig struct                | ignored by GC                 |
| 11    | global, already scanned     |                               |
| 12    | AoO                         |                               |
| 13    | AoO, marked as reachable    |                               |
| 14    | free                        | part of a free list           |
| 15    | AoO, already scanned        |                               |
The difference between static and struct is that static *is* an object, so has a proper object structure and is considered by the GC as a potential source of roots, whereas struct is just an arbitration Zig allocation. The global arena maintains a Zig Allocator structure so that any Zig allocations are handled within our space.
pp
AoO objects are objects within [[Memory Management#Array of Objects]].
Static objects are only scanned once per garbage collection.

## Zig Allocator
Because the global arena is non-moving, it can be used as a Zig Allocator, which means no other allocator is required. Objects allocated this way are marked with a Struct age. When free'd, they are marked with a Free age.

### Heap object structure
All objects are allocated with the data preceded by a header. As all global objects are allocated on a power-of-2 boundary up to page-size (at least 512 and more likely 4096), this means that all fields will be on the appropriate address boundary. The length field captures the entire size of the in-heap object. If the object contains both slots and index fields, then the end of the object allocation will be a slice for the array, preceded by the array contents (if they fit within an in-heap object, else the slice will point to the mega-object area), preceded by the slots. If there are no indexable fields, the slice won't be included.

There are special cases for objects with a small number of indexable fields (0-109 bytes or 0 anything) (See [[Mapping#Format]])

## Global Arena Structure
The Global Arena uses a binary heap.
#### Free-space allocation
Free-space is split up into power-of-2-sized pieces and put on the appropriate queue:
- all allocations are on power-of-2 boundaries; hence small objects will never span 2 cache-lines. A flag can be set so no list for values smaller than a cache-line is maintained, in which case no cache line would contain multiple objects. This can reduce cache thrashing between threads/cores. Thanks to [Peter Lount](https://www.linkedin.com/in/peterlount/) for pointing out [the potential bad behaviour on multi-threaded access](https://en.algorithmica.org/hpc/cpu-cache/cache-lines/). 
- 1-word free-space is not allocated on any queue. A 0-length free object (i.e. just the header word) is stored.
- Otherwise, free-space is split into two pieces: the largest power of 2 that will fit, which is put on the appropriate list, and loop to allocate the rest.

#### Allocation
Allocation simply requires finding the enclosing power of 2, then taking the next block off that list. This is done without a lock. If the appropriate list is empty, then a value is requested off the next list (which may also be empty which will request the next...). Once a value is found from the next list, it is split in 2, with one of the resulting blocks prepended to this list, and the other one being returned to the requestor. If the list of the largest size is empty, another block is allocated from the operating system.

## Large data allocation
For objects of 2048^[this exact size will be tuned with experience and may become smaller] words or more (16KiB or more), separate pages are allocated for each object. This allows them to be separately freed when they are no longer accessible. This prevents internal memory fragmentation. It also supports mapping large files, so for example a "read whole file" for anything large will simply map the file as an indirect string, and for anything smaller allocate the string and read the data into it.

The objects with large allocations are linked together so that at the start of the sweep of the global arena we can go though this list finding all the unused objects that have large allocations and free up their allocation.

## `become:` `becomeForward:` `elementsForwardIdentityTo:` and `elementsExchangeIdentityWith:`
The become instruction swaps the two heap-objects, so that all existing references to object A reference object B and all existing references to object B reference object A. In the original Smalltalk an object table was used to allow objects to be moved around (a compacting collector) and so `become` was simply a swap of the two pointers. It's hard to do this cheaply if you don't have an object table. See [Gilad Bracha's comments](https://gbracha.blogspot.com/2009/07/miracle-of-become.html)

We have a compile-time (of the runtime) flag to support or not support forwarding (because it slows down many operations) which, by implication, also controls the `become:` cases in the global arena other than number 1. In the nursery they are OK, because nurseries are small, the `become:` only affects one process, and we immediately do a nursery collect.

There is a complex approach that we considered, but full `become:` is rare enough, and the size of in-heap portion of objects is bounded, so we instead take a simpler approach, which has 4 cases (after promoting a process-local object if the other one is global):
1. if the in-heap portion of the objects are the same size, then the contents are swapped except for the hashes
2. if it's a forwarding become, the source header is replaced with with a simple forwarding pointer.
3. if both are forwarding pointers, we just swap those (after swapping the hashes)
4. otherwise, the larger is copied to a fresh location, the smaller is copied to the old location of the larger (with the rest becoming free space), and a forward to the fresh location replaces the header of the smaller, with the rest becoming free space
5. if a forward points to a forward, replace the original forward - which means that there is never more than one forward that needs to be followed.

This simpler approach is better because the previous approach would have slowed down every heap access with checking for an exchange table, even if `become:` was never used. With the approach above, the only check in heap access is to check for a forwarding pointer which is already required to support object promotion from process-local heaps to the global heap.

Because a `become:` can move the contents of an object, a `become:` must happen between blocks of native code. This means that `become:` must be a full send (and by implication, so must `perform:`). Therefore a global `become:` waits for all other processes to become quiescent before doing the operations. This makes `become:` a relatively expensive operation. (This is actually only critical if the rest of the smaller object in 4 above is free'd.)

The following operations are affected (slowed down) by supporting `become:` 
1. accessing any instance variable or indexing

### Array of Objects
Normally a Smalltalk array contains objects, which in the case of memory objects is a pointer, and the actual objects may be scattered across memory. In this situation, iterating through the array imposes not only an extra level of indirection, but also very poor cache locality.^[Note that compacting collectors somewhat mitigate this.]

The allocation of an array of objects creates an object that encompasses a sequence of objects of a given size. The objects at those locations are complete objects (including headers) and do not have to be homogeneous, but have to be no larger than that size. In fact, when the AoO is first allocated, all the elements will be initialized to Nil. This means that polymorphic dispatch could be used as long as the AoO is created with the largest of the objects. The `at:put:` message will fail if a larger object is provided. Elements also cannot be `Float` (for encoding reasons) but this is not a limitation since an array of floats is already efficiently handled.

The low 6 bits of the hash field for an element are the log2 of the size of the total array and allow accessing its header.

The two advantages of arrays of objects are:
1. (small) space saving, since we save an indirect pointer, but this can be a 25% saving for, e.g., `Point`
2. significantly better cache locality for `do:` or `collect:`

The disadvantages relate to garbage collection:
1. if the elements contain pointers, additional scanning may be required
2. references to elements require a bit more work when being marked

`at:`, `do:`, `collect:`, etc. for an array of objects return the immediate value if that is what is at the particular position, or a created heap reference.
`at:put:` verifies that either:
-  the value is a non-double immediate, or 
- a heap-allocated object with a length <= the specified limit, and that value is currently on a per-process heap (or immutable) - because we must move the object into the AoO and we don't move mutable global objects (this should not be an onerous limitation) - in this case we leave a forwarding pointer behind.
Since the hash value is changed for an object stored into an AoO, anything that assumes the previous hash is correct will fail. That would make a Dictionary have bad performance before a rehash. One other limitation of AoO values is that they will be moved if they are copied to another AoO (like growing a `Dictionary`) so the address of the object would change. This makes them a bad choice for Associations where the address might be coded in something like a `CompiledMethod`. It might be good to limit them to only contain references to immutable objects.

## Notes
- when the stack is being spilled, 
## Web resources
- [Boehm MS vs Copying](https://hboehm.info/gc/complexity.html)
- [Cornell course copying](http://www.cs.cornell.edu/courses/cs312/2003fa/lectures/sec24.htm)
- [uta copying](https://lambda.uta.edu/cse5317/notes/node48.html)
- [GNU Smalltalk partially-copying collector](https://www.gnu.org/software/smalltalk/manual/html_node/GC.html)
- [OpenSmalltalk VM collector (including benchmarks)](https://dl.acm.org/doi/10.1145/3359619.3359741)
- [Comparison of jemalloc, mimalloc, tcmalloc](https://lf-hyperledger.atlassian.net/wiki/spaces/BESU/pages/22156632/Reduce+Memory+usage+by+choosing+a+different+low+level+allocator)