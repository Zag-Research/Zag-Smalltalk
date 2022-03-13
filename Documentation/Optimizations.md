## Optimization Opportunities

- `self` messages can be inlined
	- doesn't matter if the method is from a superclass, (even if it's overridden below the current class, because that will dispatch in its table)
	- can be a bit tricky if the inlined method is from a superclass, because self messages there refer to both messages defined there and (since we're inlining) anything down to the class of the current method. `super` lookup is relative to the class of the send, not the target
	- overrides below the current target class are irrelevant because the original message was sent to an instance of the target class, so overrides below that are not relevant
- if we have recognized the other argument to be the same class as self, then ditto
- tail call elimination - partially get for free from Zig/Rust, but recognize and turn a send into a loop in e.g. `whileTrue`
- closures for blocks that send no messages can be allocated on the stack
- would like to not special-case `ifTrue:` and friends
-  we will recognize methods that are sent to particular classes (or messages that have limited numbers of implementations (like 1 or 2) e.g. `yourself`, `ifTrue:ifFalse:`, `whileTrue:`, `ifNil:`) and be able to inline them
- the Object value for Symbols will have the index into the Symbol table and also the arity. Then the dispatch keys will have the same information encoded for each method so we don't have to check arity separately
- the object header for BlockClosure will have the number of parameters encoded in the  hash value and BlockClosure as the class, This will be followed by code address (which would look like a double, if anyone could see it), then `self`, a reference to the indirect vector if any, and the closure fields.
- when code is generated for a method, any messages that are inlined must be noted so that if any of them are replaced, the generated code can be invalidated so that it can be regenerated - likely do this conservatively
- some thoughts on handling primitives, that don't seem relevant now: dispatch handles special symbol values that can't come from Smalltalk code
	1. I thought of having a bit that says this is a primitive failure and it encodes the Smalltalk code in a primitive method. To get the Symbol value, just AND the low bits. However, all we actually need is to code the same Symbol, but with an extra parameter number for the error code. (This would not correspond to any actual code, because the arity is encoded in the extended hash for Symbols). This would only exist in the dispatch table for the class.
	2. I thought of having a bit that says this is looking for the interpretive code - this would have been set by the dispatch code itself when it doesn't find a compiled version. However, I realized that if the pointer is below the Smalltalk heap, then it's compiled code, and if it's above, it's source code -so we don't have to hash twice
- there is a global lock guarding any class updates:
	- updating dispatch tables
		1. resizing
		2. compiling a method
		3. filling in an entry
		4. compiling a block closure
	- user-code-driven changes
- exporter has map of Pharo primitives to AST primitive numbers - signals error for any unmapped ones
- if my class is <= 8 I can check with an AND and comparison. Use for `nil`, `true`, and `false` rather than compare the value. Can also check for the special return value
- `at:`, `at:put:` and `instVarAt:`,`instVarAt:put:` are opposite for most classes; e.g. Array has the former, but the latter signal an error, whereas Point has the opposite behavior. Therefore, when looking up one of these on a failed dispatch, we can choose the appropriate implementations of these depending on the format of the class. Ditto for ByteArray/String, Symbol, format 9.
- by making SmallInteger be tag 7, they are the last values in the 64-but values. By coding them as 49-bit values and with the negative integers first it means several optimizations fall out:
	- comparisons
		1. a u64 comparison will give the correct signed comparison for 2 tagged values. Testing if the other is larger than self doesn't even have to check if it's an integer... it has to be (though if it's less, we have to check and may fail the primitive)
		2. anding with 1 will correctly test for odd/even
	- arithmetic
		1. adding untagged positive integers to a tagged integer treated as a signed integer will always stay in the range of SmallInteger unless the result is positive, in which case there has just been overflow
		2. adding a small positive value, say less than 4 to a value for which the only other assignment is less than say 4 (including all negative) we don't need to check for overflow as we'd have to do it 2^47 times or so to overflow
		3. similarly subtracting  an untagged positive integer is in overflow if the result is less than (0xfffe000000000000) (treated as a u64)
		4. `or`, `xor`, etc. with positive untagged integers will work correctly
		5. `or`, `and` with *tagged* positive integers will work correctly
	- conversion
		1.  subtracting a tagged 0 (0xffff000000000000) will give an untagged 49-bit integer
		2. adding an untagged 49-bit integer to a tagged 0 will give the correctly tagged SmallInteger
		3. For any immediate value (class 2-8) taking a remainder of the whole u64 with a large prime (4294967291 (0xfffffffb) is the largest 32-bit prime) should give an excellent hash. Doing a 64-bit multiply wouldn't work well for e.g. floating point or true/false/nil. Remainder with 16777213 (0xfffffd) - the largest 24 bit prime - might be better as it will keep all the hashes in the same range.
- error codes in <primitive:ec:> are usually symbols, nil, or occasionally integers - need to find a good way to handle primitive failure - update: the primitives, as methods will simply push an ec on the stack (if appropriate) and return a PrimitiveFailed
- become: /elementsExchangeIdentityWith: need to preserve/swap hash values so that dictionaries continue to work correctly
- DNU goes to class initial lookup class - e.g. super, not starting at the object
- if parameters and local values are classes 2-8 (False to Float) we don't need to keep them on the stack, because they won't be affected by garbage collection. If there are non-local-return blocks, then self will need to be maintained and values referenced by escaping blocks will have to reside in the closure as tagged values.
- tagged address for objects (tag 0) mod 16777213 will give a good 24 bit hash value.
-  For closures, the high 8 bits of the hash is the number of parameters for that closure. When exiting a method, any block closure that has a non-local return should have its parameter count set to 255. Then when any `value`, `value:`, etc. is sent, those methods in `BlockClosure` will be called and will fail
- convert all `<primitive: 42>` pragmas to a send to `primitive_42` with appropriate parameters and possibly an error block. Then they can be inlined specially, even for low-optimized compilers/tests
- If a value from the stack isn't a heap object, then it can be copied to a local variable (in Zig) of the appropriate type and removed from the stack, but any heap object (including block closure) created in a method has to be added to the stack, and any such values created must be copied to the stack (and re-loaded from the stack following any actual send, because a GC might have happened - ditto for any forced GC)
- For each heap, keep the address of the bottom & top of the heap arena as tagged object pointers so that when scanning, for each object, if (obj<obj_high and obj>obj_low) then it's in this arena so copy it - otherwise treat it as a literal
- `on:do:` adds to a linked list through the runtime stack of the thread so that a subsequent signal can evaluate the do block if the exception matches, so it is possible to resume. If it doesn't resume, unwind the stack to the calling point, handling `ensure:`s on the way.
- non-local returns will similarly unwind the stack to the appropriate method, handling `ensure:`s on the way.
- interpreting the length+isForward as a single u16 value means no masking is required, and values<32767 are actual, =32767 means extended and >32767 means forwarded
- when a thread is asked to pause for garbage collection, it can look through its private heaps for roots for the collection arena before pausing. The challenge is where to keep them (so that the collecting thread can use them). One solution would be to keep a fixed-size buffer (like 100 entries), and beyond that to examine them directly. The advantage of the buffer is that it can be created by the thread in parallel, and can be patched afterwards in parallel, and can have the actual addresses of the pointers. Alternately, objects that contained roots could be chained together by adding an extra word to every object with format<16. The downside, beside the extra word, is that the objects would have to be scanned again in the middle of the collection while the old arena is still live - or at least before it is abandoned. Actually either way, the old arena should stay alive until all threads have patched. Actually they can run in batches... the client thread fills its buffer... then blocks.... gets woken up when all pointers have been copied, at which point the copying thread has replaced the pointers, so the client thread fills its buffer again... and repeat.
- other...