Indicate 👍 (thumbs up) or 👎 (thumbs down) for each

- 👍`inlinePrimitive` followed by primitive number. Followed by a primitive number, and the primitive is looked up and swapped in place of the `inlinePrimitive`, so it essentially becomes a new threadedFn, and lets us extend those without changing the size of the `threadedFn.functions` array (making image dumps more portable).  There would be an extra function named `inlinePrimitive` in structs supporting this.
	- **Must not fail.** (at least initial version or see below)
	- could handle failure by 
		- creating a special context if there isn't one and doing a full-dispatch send and then returning from that created context if we created one (note we'd have to save the `extra` parameter if we created a context, so that we can restore it before continuing with the function so that it can create a proper one when required)
		- or if the encoding for `extra` that enables creating a context on-the-fly is implemented, just create one
	- the word following the primitive could be the selector (symbol) augmented by the primitive number. that way if we wanted to handle the failure, we would have the proper symbol to send.
	- this implies that the receiver must be known, but the primitive could fail, with simply the cost of an extra send, and if they don't fail we can save the cost of context creation completely
- 👍`extra` indicates if a context has been created.... *(some of this only works with zag encoding)* it will not be a `*CompiledMethod` when a context has been created. This means a `send` or `popLocal` could create one on the fly. `tailSend` would also know what kind of cleanup it had to do. We would have to know where it should go
	- 👎 but we could push an invalid `Context` on the stack on a send/tailsend. 
	- 👍 extra could be encoded with:
		1. `*CompiledMethod` - as an immediate value with the extra field having the offset to the `self` value so that if anything wants to create a context, it has the appropriate offset. Then pushes and pops could manage this (make sure there is e.g. 128 words free on the stack on entry, and then if bumping this up ever sets 128-bit, deal with overflow - i.e. create a context)
			- possibly could count SP freespace as well as `self` offset to make the overflow easier to calculate
		2. `*ContextData` - as an immediate value with the extra field unneeded - can be reloaded on returns. or could just be the pointer if we really don't need the extra field (considered counting the space for closures, but very little value - better to be able to recognize the difference with an `and` and a `test`)
		3. differently flagged `*CompiledMethod` for failed primitive
	- 👍👍 Even better is to have the low bits of the `Extra` value have the low bits of the `sp` address of `self`. Then we just check for the low bits of the `sp` (`@intFromPtr(sp) & stack_full_mask`) to be 0 - which is an overflow and we can get the `self` address from the `sp` and `extra`. This means that words that push on the stack only have to handle a possible stack overflow - at which point we'd have to make a context anyway, so we could spill.
	- 👍 push/pop local parameter could have 2 offsets.... an offset if there is no context, and one if there is
		- the one for no context, is a negative offset from the `self offset` on the stack (so the offset for `self` would be zero, the offset for the first parameter would be 1, etc.)
		- the one for yes context, is a positive offset into the `ContextData` object (so if there were 3 locals and one parameter, the offset to `self` would be 4, the offset for the first parameter would be 3, etc.)
- 👎 we could get rid of the process pointer as we can always create it from the stack pointer - but then we'd have to find a place to pass the "check for interrupt" flag - possibly in extra
- 👎 although the test for match could be moved from `send`/`tailSend` into the method, which would maybe save 1 or 2 when running a JITted method, it would mean an extra parameter to all threadedFns and an extra word for every send, which doesn't seem like the right trade-off
- 👍 jitting, if the PIC address for a send isn't the initial case, we don't have to check the signature for 0

