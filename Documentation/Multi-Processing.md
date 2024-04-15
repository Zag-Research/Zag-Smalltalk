## Thread vs. Processes

The term for multiple simultaneous execution units in common use today is *threads* (even sometime having multiple hardware threads per core), and the term process is reserved for an operating system process (an address space, open files, etc. with 1 or more threads). In Smalltalk these are referred to as processes (even though in all other Smalltalk systems extant at the time of writing these are multiplexed on a single thread). Additionally, Zag uses an execution model called threaded-execution. Therefore, we use the term processes to refer to multiple simultaneous execution units everywhere.

There are several places in the runtime where values need to be updated in process-safe ways. Generally these are done with a hardware instruction such as Compare-and-Swap (CAS) that the hardware guarantees only one processor can update. This means that the retrieved value is consistent, because every reference is via such an instruction. We will refer to this as CAS even if it is a 2-word swap.
## Execution Data Structures

The complication comes when there are references to 2 adjacent values through normal references, where the hardware doesn't guarantee that a store from another processor didn't intervene (in fact, the operating system might have suspended this process between accesses). Here we have to make sure that we can detect that the second value isn't consistent with the first one and handle it appropriately.
#### Dispatch

*Dispatch Tables* are arrays of *Dispatch Elements* (described below). There is a dispatch table for each class. Dispatch involves accessing the dispatch table for the particular class, indexing a dispatch element based on the hash of the selector (a `Symbol`), and jumping to the code referenced by that dispatch element. Normally that code is the method for that selector and class. However the indexing (selector hash modulo the (prime) size of the table) is not guaranteed to be conflict free, so if there is a conflict, that code is a disambiguator  that chooses between two dispatch elements and then jumps to the appropriate code.

When we add a method to a dispatch table, if the calculated index doesn't reference an existing method, a CAS is used to replace the dummy dispatch element with the dispatch element for the new method. If it does reference an existing method or the CAS fails (because another process installed a method at the same time), then a disambiguator has to be installed instead. If it references a disambiguator, then a new, larger, dispatch table is created and installed because we only allow 1 level of disambiguator (although there may be several disambiguators in a given dispatch table).

*Dispatch Elements* have two potential formats, depending on the (compile-time) toggle `config.indirectDispatch`. If it's `true` then a dispatch element is simply an address of a threaded code word. In this case, the pointed-to word is accessed to point to the threaded word to execute. This is simple (and has smaller dispatch tables), but introduces an extra pipeline stall in dispatch: we read the dispatch element from the dispatch table and then have to read the word it points to before we can jump to it. The hardware has nothing it can do during this, so it is limited to memory (cache) speed.

If it's `false` the dispatch table is an array of 2-tuples: the address as above, as well as the first word from that address, so the stall is reduced by 1 memory (cache) access time.

In the second case the dispatch element is 2 words long, so we have the potential problem of going to a method with the second word being part of a disambiguator. We don't have to worry about the reverse, of going to a disambiguator with the second word being part of a normal method reference, because disambiguators aren't removed from dispatch tables once they've been added. The solution is that the second word for disambiguators is an odd address (1 less than the proper address). Therefore, if a method header sees an odd address, it will force both words to be loaded, and then re-dispatch. This is an extremely low probability event, as it only happens when another process is adding a method during a dispatch. Note that the dummy dispatch also has to recognize that a method has been added.

#### CompiledMethod pointers
A `CompiledMethod` has 3 pointers to code for different purposes. The first is a version of `validateClass`, followed by `validateClassForTail`, and `validateSelector`. The `validateSelector` is the one stored in the dispatch table and is used for polymorphic dispatch. The `validateClass` ones are both used for monomorphic dispatch (for normal and tail sends) via a Monomorphic Inline Cache.
#### Sends

The other case is for monomorphic inline caches. The threaded code generator for a message send produces a `send` instruction (or a `setupTailcall` followed by a `sendTail`) instruction followed by the symbol for the selector. The `send` or `sendTail` instruction extracts the class from the receiver object on the stack, and gets the selector word from the current instruction thread.

If the selector word is not a symbol, some other process must have swapped in something, so we re-load and re-execute the word where the `send` or `sendTail` was, because it must now be something else!

Otherwise, after it has verified that the selector word is a symbol, it looks up the dispatch element from the dispatch table for that class/selector pair.

The code address in the dispatch element is for either a `validateSelector` or a primitive. They both start by verifying that the selector is the one for this method. If not, they signal a `DoesNotUnderstand`, which will find a method, compile it, put a disambiguator in the dispatch element, and then re-dispatch. When we know nothing about the receiver of a message send, this is the best we can do.

However, on that first call from a particular call site we know exactly which class the receiver belongs to, so we could go directly to that code, and all it would need to do is verify that the class of the sender is correct, and if so, execute the code for the method (after saving the return address in the calling context). This bypasses looking up in the dispatch table, and several memory stalls. It is quite frequent that a given call site always has the same target type (the cited number is 90% of call sites). So the `send` or `sendTail` word does a CAS to swap in the `validateClass` (or `validateClassForTail`) value and the address of the `CompiledMethod` object, expecting the previous value to be the `send` or `sendTail` followed by the selector. Regardless of the outcome of the CAS (either we were successful or again some other process must have swapped in something) it now has the code we should be executing, so we re-load and re-execute the word now there.

If the `validateClass` (or `validateClassForTail`) finds the wrong class, or the second word is a symbol, then this is no longer a monomorphic call site, so we want to replace the 2 words at the call site with a `send0`, `send1`, etc. (or `send0Tail`, `send1Tail`, etc.) followed by the selector again (this is the symbol that the `vaidateClass` might have seen). These sends do the appropriate dispatch, and the call sire will no longer be changed.

## Dispatch Table swap
As mentioned above, sometimes dispatch tables need to be replaced because of a selector conflict. This is a single word and is handled with a compare and swap. It is quite a rare event.

## Process Synchronization
There are a few places where processes have to recognize a common event. For these cases, each execution process checks their process table periodically (like every 20 message sends) to see if synchronization is required.

These are required for several reasons, described below.
### Global Garbage Collection
When the global garbage collector starts a marking pass, it first goes through global, shared data structures like the dispatch table, the symbol table, and the class table, and marks objects accessible that way. Then it asks, via process synchronization, all execution processes to mark objects that are reachable by them. Once they have done that, they resume execution. When they have all done that, it resumes its collection process.