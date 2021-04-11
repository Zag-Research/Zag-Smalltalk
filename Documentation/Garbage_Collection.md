# Garbage Collection

Garbage collection is an essential part of a Smalltalk system. While the original Smalltalk system approach of creating activation records as heap object has been replaced with stack allocation, idiomatic Smalltalk still creates a lot of short-term objects that need to be collected.

AST Smalltalk has several features that minimize garbage creation:
- activation records (contexts) are stack allocated
- `nil`, `true`, `false`, Symbols, Characters, SmallIntegers and Floats are all encoded as immediate values
- SmallIntegers have a wide range (2^49) so extension to BigIntegers (which would be heap-allocated) is rare
- non-escaping BlockClosures are stack allocated
- code blocks are generated outside the heap

AST Smalltalk has several features that minimize the amount of work required by garbage collection:
- all stacks and code addresses are outside the range of valid heap pointers, so by being careful to never save anything in a stack frame that looks like a heap reference, scanning a stack for roots can proceed very quickly. A single comparison suffices for 99.95% of all possible 64-bit values
- the format of indexable objects encodes if they are pointer-free, so don't need to be scanned
- each thread has its own nursery heap, so there is no contention on allocation
- the list of weak objects that must be scanned at the end of the collection is outside the heap

## Copying Collector