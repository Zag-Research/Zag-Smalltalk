- `inlinePrimitive` followed by primitive number. Followed by a primitive number, and the primitive is looked up and swapped in place of the `inlinePrimitive`, so it essentially becomes a new threadedFn, and lets us extend those without changing the size of the `threadedFn.functions` array (making image dumps more portable).  There would be an extra function named `inlinePrimitive` in structs supporting this.
	- **Must not fail.** (at least initial version or see below)
	- could handle failure by 
		- creating a special context if there isn't one and doing a full-dispatch send and then returning from that created context if we created one (note we'd have to save the `extra` parameter if we created a context, so that we can restore it before continuing with the function so that it can create a proper one when required)
		- or if the encoding for `extra` that enables creating a context on-the-fly is implemented, just create one
	- the word following the primitive could be the selector (symbol) augmented by the primitive number. that way if we wanted to handle the failure, we would have the proper symbol to send.
- `extra` indicates if a context has been created.... *(some of this only works with zag encoding)* it will not be a `*CompiledMethod` when a context has been created. This means a `send` or `popLocal` could create one on the fly. `tailSend` would also know what kind of cleanup it had to do. We would have to know where it should go
	- (not the best plan) but we could push an invalid `Context` on the stack on a send/tailsend. 
	- extra could be encoded with:
		1. `*CompiledMethod` - as an immediate value with the extra field having the offset to the `self` value so that if anything wants to create a context, it has the appropriate offset. Then pushes and pops could manage this (make sure there is e.g. 128 words free on the stack on entry, and then if bumping this up ever sets 128-bit, deal with overflow - i.e. create a context)
		2. `*ContextData` - as an immediate value with the extra field unneeded - can be reloaded on returns. or could just be the pointer if we really don't need the extra field (considered counting the space for closures, but very little value - better to be able to recognize the difference with an `and` and a `test`)
		3. differently flagged `*CompiledMethod` for failed primitive
- we could get rid of the process pointer as we can always create it from the stack pointer - but then we'd have to find a place to pass the "check for interrupt" flag - possibly in extra
- although the test for match could be moved from `send`/`tailSend` into the method, which would maybe save 1 or 2 when running a JITted method, it would mean an extra parameter to all threadedFns and an extra word for every send, which doesn't seem like the right trade-off
- push/pop local parameter could have 2 fields.... and offset if there is no context, and one if there is
