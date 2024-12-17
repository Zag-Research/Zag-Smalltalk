## Garbage Collection
Garbage collection is a well-researched topic. Most Smalltalk systems use a generational copying garbage collector. Copying collectors have the desirable property of compacting used memory and hence allowing the return of unused memory to the operating system. The downside is that either there can be significant pauses or very complex algorithms. Mark-and-sweep collectors have the desirable property of not moving objects around, which facilitates multi-processing (if objects can move, access to them has to be protected with more indirection, which is not what makes sense for a high-performance Smalltalk system).

For Zag, because we are supporting [[Multi-Processing|multiple hardware processes]] we want those processes to be able to progress with very limited pauses, so we are using a hybrid approach.
### Per-process arenas
Each process has a private stack and 2 heap arenas. The heap arenas are collected by a copying collector that copies live data (accessible from the stack and current context) from the current arena into the other arena then makes that other arena the current arena. It keeps track of the age of objects in the heap, and when it needs more space, the oldest objects are promoted to the global heap.
### Global arenas
The global heap is made up of arenas that are allocated from the operating system (currently 128KB).

If the number of arenas exceeds some percentage of the number of processes, it may make sense to have each process have a preferred arena to minimize cache contention.

It might make sense to periodically (or programmatically) have a stop-the-world moment where the contents of a couple of arenas that have low utilization are moved into another arena, similar to what Java does.
### Large objects
Objects larger than a particular size (currently 8KB) will have the indexable (i.e. array) part of the object allocated outside the arenas, in memory independently allocated from the operating system. This reduces the number of global arenas required, and more importantly reduces the fragmentation that comes with a non-compacting collector.