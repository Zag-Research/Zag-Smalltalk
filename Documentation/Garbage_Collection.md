# Garbage Collection

Garbage collection is an essential part of a Smalltalk system. While the original Smalltalk system approach of creating activation records as heap object has been replaced with stack allocation, idiomatic Smalltalk still creates a lot of short-term objects that need to be collected.

AST Smalltalk has several features that minimize garbage creation:
- activation records (contexts) are stack allocated
- `nil`, `true`, `false`, Symbols, Characters, SmallIntegers and Floats are all encoded as immediate values
- SmallIntegers have a wide range (2^49) so extension to BigIntegers (which would be heap-allocated) is rare
- non-escaping BlockClosures are stack allocated
- code blocks are generated outside the heap

AST Smalltalk has several features that minimize the amount of work required by garbage collection:
- all interpreter stacks and code addresses are outside the range of valid heap pointers, so by being careful to never save anything in a stack frame that looks like a heap reference, no scanning is required for the interpreter stack;
- scanning for roots can proceed very quickly. A single comparison filters all non-heap, non-float values, and if necessary a second filters the floats
- the format of indexable objects encodes if they are pointer-free, so don't need to be scanned
- each thread has its own nursery and teen heaps, so there is no contention on allocation
- the list of weak objects that must be scanned at the end of the collection is outside the heap

## Heap structure
Each thread/process has its own nursery heap, typically 8kib. All allocations are done in the nursery except for large objects that would not fit. This is allocated in the nursery arena, along with the Smalltalk stack. The heap grows up, and the stack grows down. When they would collide, either the heap will be collected to the teen arena, or a portion of the stack will be copied to the teen arena. Because these are thread-private, there is no locking required for allocation in the nursery or teen arenas. The teen arena is several times larger than the nursery.

The nursery and teen arenas are both collected using a copying collector. Copying collectors are very fast when a significant portion of the content is garbage, because they only examine the live content of the heap. The roots for collection are the stack and the stack extension

If, after an allocation in the teen arena (from a nursery collection, a large-object allocation, or a stack copy), there is not enough free space in the teen arena for a full nursery collection, then the nursery arena will be collected into the teen arena and then the teen arena will be collected. This is collected into the shared GlobalArena, so it must obtain a lock. The actual collection is done with a copying collector as, (a) much of the content is likely garbage, and (b) we must have room into which to copy the nursery.

The GlobalArena is the eventual repository for all live data. One invariant is that objects in the GlobalArena cannot reference objects in a nursery or teen arena, so such object must be promoted to the GlobalArena (this is the reason for a nursery collection before any teen collection). There are several sources of this data:
1. collections from thread teen arenas
2. the symbol table
3. the strings that have the representation of symbols
4. dispatch tables
5. class objects
6. large allocations
7. setting a field in an existing global object to a reference to a local object requires that the local object be promoted (leaving behind forwarding pointers)

Roots for the GlobalArena include:
1. the symbol table reference
2. the class table reference
3. the old-dispatch reference (if there is a parallel global collector running)
4. roots from all threads

## Copying Collector

## Notes
- when the stack is being copied, 
## Web resources
- [Boehm MS vs Copying](https://hboehm.info/gc/complexity.html)
- [Cornell course copying](http://www.cs.cornell.edu/courses/cs312/2003fa/lectures/sec24.htm)
- [uta copying](https://lambda.uta.edu/cse5317/notes/node48.html)
- [GNU Smalltalk partially-copying collector](https://www.gnu.org/software/smalltalk/manual/html_node/GC.html)