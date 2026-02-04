Indicate 👍 (thumbs up) or 👎 (thumbs down) for each
- dispatch can be tail called with the `extra` field a signature, because the current extra field isn't used during a send (it will be re-created on the return)
- we don't need to check the signature of a method
	- we are either verifying a send cache, in which case we know that the selector is correct, we're just verifying the class
	- or we are doing dispatch, in which case we're just verifying the selector
	- this could simplify/speed-up dispatch
- an image directory will have files with a JIT type
- an image directory could be stored as a ZIP file
- ? could break Signatures up so that when using a mini-PIC we just have to check the class, and when using a dispatch we just have to check the selector, so we save constructing the full signature except when filling in the mini-PIC - create a branch to experiment (after encoding paper is in) - might be a small speedup
- ? could have sends replace themselves with the address of a prefix to the threaded words that handles the checking and return addresses. See experiments/returnAddress.zig - create a branch to experiment (after encoding paper is in) - might be a noticeable speedup
- 👎 (this doesn't work because we can't determine easily at runtime whether there are block closures (because we would miss immediate closures) so instead, any method that (directly or indirectly) creates closures that have references to local, instance, class, or shared variables or does non-local returns must avoid using tailSend) How do we do handle a tail-send when there are live block closures? e.g. `^ [:x :y | x + y] value: 2 value: 3` - Answer: in a method (or block) that creates a block closure (initially any, but really only need to be closures that might be allocated on the stack) a `tailSend` will be followed by a `returnTop`. The the `tailSend` code will dynamically check if any closure is allocated on the stack, and if so, convert to a normal `send`... which will return to the `returnTop`. This will rarely be a problem if there is inlining, because very few blocks will be created.
- 👍 a method that contains any block with non-local returns that cannot be themselves inlined (in other words, contains a `returnNonLocal` threaded word) cannot be inlined
- 👍 allow a process/thread to block by disconnecting the process from the thread. While a process is disconnected (because the thread is doing an FFI), the global marker thread may seize the process to do the marking. When the thread returns from the FFI it needs to busy-loop to re-connect the process.
- 👍`FFI` like `primitive` - i.e. plug in a native function that marks the process as disconnected, sets up a call based on the parameters and handles the return including reconnecting the process, cleaning up the stack, and returning the result or signalling an error. These would be interchangeable based on the parameters and the result, but they also have the code address, so they aren't. Initially these can be pre-defined/hard-coded, but eventually should  all be JIT'ed
- 👍 remove the idea of IO processes, and just let the current process block... integrated with FFI above
- ? have all heap addresses be the address of the start of the object. This might enhance inter-op with FFI and would simplify handling of large arrays (because extra fields could go before the header - but would require some way of flagging the start of the extended header so that linear scans know the structure) .
- 👍 define `malloc` and `free` so that FFI code can allocate. If the allocated objects are small enough they can be allocated in the normal heap area. Else allocate in separate mmap area
- 👍 extend `slots:` to allow an association with a type (including literal-sized arrays) so that we can represent FFI structs and compile access and setting appropriate to the type. Lets us fully implement system calls, etc. in Smalltalk. The most obvious way to do this (using an actual `Association`) would require changes to the Pharo compiler, so will be indicated somehow class-side.
- 👍 support C/Zig structs and types to support FFI for example, consider the following extract from `zig translate-c` on `stdlib.h`
```zig
pub const struct_timeval = extern struct {
    tv_sec: c_long,
    tv_usec: c_int,
};
pub const struct_rusage = extern struct {
    ru_utime: struct_timeval,
    ru_stime: struct_timeval,
    ru_maxrss: c_long,
    ru_ixrss: c_long,
    ... };
pub extern fn getrusage(c_int, [*c]struct_rusage) c_int;
```
-  this gets implemented in Smalltalk as:
```smalltalk
  getrusage: who into: struct

	<ffi: 'getrusage' with: 'C_int,*struct_rusage' error: 'ec'>
	self primitiveFailed
```
- and is used as:
```smalltalk
| rusage |
   rusage := Struct_rusage new.
   Libc getrusage: 0 into: rusage. " this could signal primitiveFailed "
   " in that case Libc errno will have the correct error number "
   ^ rusage ru_itime tv_usec * 0.000001 + rusage ru_itime tv_sec " return time "
```
- `rusage ru_itime` will return a read-only `Struct_timeval` and the `tv_sec` returns a `SmallInteger` value because `c_long` fits. If it was another type like `c_long_long` it would return a `SmallInteger` if the value fit, or a `LargeInteger` if it didn't. Assignment to a struct field that had type `c_long` could possibly signal an exception if the value passed didn't fit.
- 👍 `inlinePrimitive` followed by primitive number. Followed by a primitive number, and the primitive is looked up and swapped in place of the `inlinePrimitive`, so it essentially becomes a new threadedFn, and lets us extend those without changing the size of the `threadedFn.functions` array (making image dumps more portable).  There would be an extra function named `inlinePrimitive` in structs supporting this.
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
- can use `executeFn` for a variety of things:
	- they just need to go to `pc.prev().prim()`
	- counting down to being jitted (replacing with next downcount)
	- usage counting
- can optimize some cases and swap in simpler functions
	- can save stalls/instructions, but also can minimize copy-n-patch size
	- 👍 generic `send`/`tailSend` can swap in the one that knows the arity of the selector
	- 🤷 if we knew that all paths to a context-requiring word had the same context requirements, then we could swap in versions that didn't have to check. But this seems difficult to determine - well... we'd have to trace the flow of control from the start of the method... might be worth doing for CnP
	- 👍 `push`/`pop`/`store` for simple cases like `self` for methods with particular structure (like unary/binary with/without locals), also for cases where we will still need to do offsets, but e.g. no indirection, or just instance variables, or closure variables - could gather stats on how often particular patterns show up. Even if we don't match any special cases we will switch to a version that doesn't check
	- 👍 the alternates should have a field that points to the originals, and in the originals there should be a test (on process) that says not to replace, so that if we want to create a portable image, we set that flag and then go through all the methods, resetting the code to the originals. Then the `codeAddresses` array in the image will only have the originals listed. Otherwise it will have all of the methods
	- 👍 the originals could set a flag (when they swap in an optimization) saying that the method should be re-CnP'ed (now that we know more)
	- note: we always have access to the method pointer, either because extra points to it, or the `context` parameter points to our context, and it has the method pointer