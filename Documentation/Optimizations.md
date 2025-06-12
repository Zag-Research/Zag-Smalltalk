## Optimization Opportunities

- `self` messages can be inlined
	- doesn't matter if the method is from a superclass, (even if it's overridden below the class of the current method, because that will dispatch in the table for the target class)
	- can be a bit tricky if the inlined method is from a superclass, because self messages there refer to both messages defined there and (since we're inlining) anything down to the target class. `super` lookup is relative to the class of the send, not the target
	- overrides below the current target class are irrelevant because the original message was sent to an instance of the target class, so overrides below that are not relevant
- ditto for sends to literals
- tail call elimination , i.e. recognize and turn a send into a loop in e.g. `whileTrue`
- closures are always initially allocated on the stack
- no special-case for `ifTrue:` and friends (i.e. they don't get turned into tests and branches), except that they have special threaded words (as do `value`, `value:`, etc. and `at:`, `at:put:`, `instVarAt:`, and some other high-frequency selectors). The special words handle some classes specially (e.g. `value` knows about `BlockClosure` and `Symbol`) but otherwise sends the message - which is still advantageous since the selector isn't fetched from the threaded code.
- when inlining we will recognize methods that are sent to particular classes (or messages that have limited numbers of implementations (like 2 or 3) e.g. `yourself`, `ifTrue:ifFalse:`, `whileTrue:`, `ifNil:`, many of the `is...` methods) and be able to inline them
- use a very good, single-level hash for dispatch, since all symbol hashes are unique, and have the target method verify the hash match else call DNU (where the dynamic version DNU code will see if there is a method to fill in, and do that, else real DNU)
- hashing
	- for literal values basicIdentityHash (primitive 171) will just be the values with the tag replaced with 0xFFF5, which will turn it into a non-negative SmallInteger. For in-memory objects (primitive 75), it will be the header word or'ed with 0xFFFFFF000...00, which will mask out the length and format bits, then considered as an object and have the tag replaced with 0xFFF5, which will turn it into a positive SmallInteger, 
	- tagged address for objects (tag 1) mod 16777213 will give a good 24 bit hash value. Better would muliply by 2^24/phi and take the low 24 bits.
- the object header for BlockClosure will have the number of parameters encoded in the  hash value and BlockClosure as the class, This will be followed by code address (which would look like a double, if anyone could see it), then `self`, a reference to the indirect vector if any, and the closure fields.
- when code is generated for a method, any messages that are inlined must be noted so that if any of them are replaced, the generated code can be discarded so that it can be regenerated - likely do this conservatively
- there is a global lock guarding any class updates:
	- updating dispatch tables
		1. resizing
		2. compiling a method
		3. filling in an entry
		4. compiling a block closure
	- user-code-driven changes
- exporter has map of Pharo primitives to AST primitive numbers - signals error for any unmapped ones
- if my class is <= 8 I can check with an AND and comparison. Use for `nil`, `true`, and `false` rather than compare the value
- `at:`, `at:put:` and `instVarAt:`,`instVarAt:put:` are opposite for most classes; e.g. Array has the former, but the latter signal an error, whereas Point has the opposite behaviour. Therefore, when looking up one of these on a failed dispatch, we can choose the appropriate implementations of these depending on the format of the class. Ditto for ByteArray/String, Symbol, format 9.
- previously SmallInteger was groups 8-F so they were the last values in the 64-but values. By coding them as 51-bit values and with the negative integers first it meant several optimizations fall out. Not all of the following is exactly right, although some aspects still apply:
	- comparisons
		1. a u64 comparison will give the correct signed comparison for 2 tagged values. and we only have to check one side to verify it's an integer.
		2. anding with 1 will correctly test for odd/even
	- arithmetic
		1. adding/subtracting untagged 50-bit integers to a tagged integer will always stay in the range of SmallInteger unless the result is less than u64_MINVAL, in which case there has just been overflow
		2. adding a small positive value, say less than 4 to a value for which the only other assignment is less than say 4 (including all negative) we don't need to check for overflow as we'd have to do it 2^48 times or so to overflow
		3. `or`, `xor`, etc. with positive untagged integers will work correctly
		4. `or`, `and` with *tagged* positive integers will work correctly
	- conversion
		1.  subtracting a tagged 0 (0xfffc000000000000) will give an untagged 51-bit integer
		2. adding an untagged 51-bit integer to a tagged 0 will give the correctly tagged SmallInteger. if the result is less than u64_MINVAL, there was overflow
	- hashing
		1. `or` any immediate (including doubles) with 0xfffdffff00000000 will give a 32-bit positive (`xor` with >>32 first would be even better)
		2. any in memory, take the low 32 bits of the address, multiply by 2^32/phi and shift right 8 bits. xor with the high bits wouldn't increase randomness.
- primitives will fail with a Zig error. if the primitive is supposed to return an error code, it will be passed a pointer to an Object to store the code.
- become: /elementsExchangeIdentityWith: need to preserve/swap hash values so that dictionaries continue to work correctly
- DNU goes to class initial lookup class - e.g. super, not starting at the object
- if parameters and local values are immediates we don't need to keep them on the stack, because they won't be affected by garbage collection. If there are non-local-return blocks, then self will need to be maintained and values referenced by escaping blocks will have to reside in the closure as tagged values.
-  When exiting a method, any block closure that is on the heap and has a non-local return should have its class changed to `BlockFailure` so any send will fail.  Non-local thunks must be replaced with a fallback `BlockFailure` object. This means that all closure references must be in the `Context` or the `ContextData` object (if any).
- convert all `<primitive: 42>` pragmas to a send to `primitive_42` with appropriate parameters and possibly an error block. Then they can be inlined specially, even for low-optimized compilers/tests
- **This has changed** If a value from the stack isn't a heap object, then it can be copied to a local variable (in Zig) of the appropriate type and removed from the stack, but any heap object (including block closure) created in a method has to be added to the stack, and any such values created must be copied to the stack (and re-loaded from the stack following any actual send, because a GC might have happened - ditto for any forced GC)
- For each heap, keep the address of the bottom & top of the heap arena as tagged object pointers so that when scanning, for each object, if (obj<obj_high and obj>obj_low) then it's in this arena so copy it - otherwise treat it as a literal
- `on:do:` adds to a linked list through the runtime stack of the thread so that a subsequent signal can evaluate the do block if the exception matches, so it is possible to resume. If it doesn't resume, unwind the stack to the calling point, handling `ensure:`s on the way.
- non-local returns will similarly unwind the stack to the appropriate method, handling `ensure:`s on the way.
- interpreting the length+isForward as a single u16 value means no masking is required, and values<32767 are actual, =32767 means extended and >32767 means forwarded **changed with indirect collections - must be updated**
- when a thread is asked to pause for garbage collection, it can look through its private heaps for roots for the collection arena before pausing. The challenge is where to keep them (so that the collecting thread can use them). One solution would be to keep a fixed-size buffer (like 100 entries), and beyond that to examine them directly. The advantage of the buffer is that it can be created by the thread in parallel, and can be patched afterwards in parallel, and can have the actual addresses of the pointers. Alternately, objects that contained roots could be chained together by adding an extra word to every object with format<16. The downside, beside the extra word, is that the objects would have to be scanned again in the middle of the collection while the old arena is still live - or at least before it is abandoned. Actually either way, the old arena should stay alive until all threads have patched. Actually they can run in batches... the client thread fills its buffer... then blocks.... gets woken up when all pointers have been copied, at which point the copying thread has replaced the pointers, so the client thread fills its buffer again... and repeat.
- when a method that contains a nonlocal-return-block returns (including tail-sends), it must invalidate the block so the NLR can fail
-  make thread-local objects (like the  `Context` class) have a trivial (1-element) dispatch table that sends any selector to a special method that checks if we are in the correct thread, and then uses switch to direct to the correct method. Then thread-local objects don't get promoted to the global heap, but references will still work correctly. Note that this means that thread-local objects have to be addressable from everywhere, and therefore can't be in the OS's idea of thread-local-store
-  generate a runtime with the (Smalltalk) compiler code included, so that we don't have to have an interpreter. Or possibly generate a runtime with the (Smalltalk) interpreter code (might )
-  every class that is created gets the same entry in the dispatch table - a dispatch to a single method that goes to the class and find which methods exist and create a dispatch table for them full of entries to compile that method
-  can further parameterize Treap by size of index (instead of always 32-bit), then the class treap can have 32-bit keys (the symbol hashes) and 16-bit indices, halving the size of the table. Use hash multiplier of 2^n/phi. **Done**
- when an object is promoted (or allocated) in global space, send it a `#becameShared` message so it can, for example change its class to one that locks the object for mutation operations (e.g. a WriteStream)
- tail-sends may not reuse the `Context` if it's not on the stack (because it might have been captured by, e.g. `call/cc`)
- For the large-data allocation, an auto-treap could used to do the mark/sweep for these values rather than a linked list. This would allow multiple references into an object, but to be useful this would have to encode indirection elsewhere than the size field. It also has the potential to lead to large chunks of memory being held onto even though only a tiny part of it is being accessed. But 
- other...