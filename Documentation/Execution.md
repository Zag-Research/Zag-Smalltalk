## Execution

This system uses a dual execution model.  For each method, there is a threaded implementation and possibly a native implementation. There is no classical "interpreter". The closest is the threaded implementation. 

### Semantic Interpreter
There *is* an interpreter that runs in Smalltalk as part of the Zag-Core-Test package. It allow execution of method sends in the compiled code to verify that the Zag code executes the same as the host Smalltalk system (Pharo, Cuis, etc.). It is slow, and is not a complete implementation. In particular it uses host arrays to emulate objects, and doesn't do real memory allocation or garbage collection, but it does dispatch, on-demand compilation, program counter, stack, contexts, and closures in an analogous manner.
##### Execution details currently having their semantics clarified
- return with/no context - with/no tos - offset to self
- restructure operation for branch-returns and inlined methods
- tailSend with/no context with restructure parameter
- can optimize away pushes into restructure instruction/tailSend

### Threaded Method Implementation
The threaded implementation is a sequence of addresses of functions implementing embedded primitives  and control operations. Every `CompiledMethod` has a threaded implementation. One of the "registers" that is passed through the thread is a flag indicating whether the current thread needs to check for interruptions. Every threaded operation checks this flag before passing control along to the next function. This allows the threaded implementation to single step through the method. Control is passed using an indirect tail-call.

### Native Method Implementation
The native implementation is a sequence of functions implementing everything between actual message sends. After inlining, this can be a significant amount of code. Each function passes control to the next code via a tail-call, passing the same registers as the threaded implementation. This means that native code implements continuation-passing style, and no native activation records are created. One of the registers that is passed is the program counter... that is, the next threaded code to be executed. Because one native function can implement several threaded equivalents, these may be non-sequential.

### Common features
All functions that are part of the normal execution flow (threaded words, primitives, jitted native code) have a common signature. As an example, here is the code for the threaded word `pushLiteral` which pushes a literal object (at the next position in the threaded code) onto the stack:
```zig
pub const pushLiteral = struct {
    pub fn threadedFn(
		    pc: PC,
		    sp: SP,
		    process: *Process,
		    context: *Context,
		    extra: Extra)
		      Result {
        const newSp = sp.push(pc.object());
        return @call(tailCall, process.check(pc.prim2()),
	         .{ pc.next2(), newSp, process, context, extra });
    }
};
```

The parameters (presumably all in registers on modern architectures) are:

| Parameter | Description                               |
| --------- | ----------------------------------------- |
| pc        | pointer to the next threaded word         |
| sp        | pointer to the top of the Smalltalk stack |
| process   | pointer to the current process            |
| context   | pointer to the current context            |
| extra     | multi-purpose value                       |
The result type is mostly irrelevant, because none of these functions ever return; they always exit via a tail-call. Usually this is to the next threaded word (in this case, skipping the literal object to push) unless this threaded word is a return or a call/send. When going to the next threaded word, we also need to bump the `pc` past that address (in this case, also skipping the object). Note that the `sp` parameter that we pass is the `newSp` value because we just pushed something onto the stack. The `process.check` is an inline function that checks if we are in single-step mode, otherwise continuing to the next word.
## The stack and Contexts

The native stack is only used when calling non-Smalltalk functions. All Smalltalk stack frames (Contexts) are implemented in a Smalltalk linked list of `Context` objects. They are initially allocated on the Smalltalk stack which resides at the beginning of a Process. If the stack becomes too deep, the Contexts will be spilled to the Process' Nursery arena (and potentially to the Global arena). There are a few other conditions (such as returning `thisContext` from a method) that similarly cause the `Context` (and ones it links to) to be spilled to the heap.

There are several reasons for this, but the primary reasons are that: a) all the GC roots are on the stack or in the current Context which means that it is easy to do precise GC; and b) switching between interpreter and native implementation is seamless.

`self`, parameters, and locals are initially on the stack, and are accessed via offsets from `sp`. This means in a method that doesn't create a `Context` all references to these values will use stack offsets. Even in a method that does push a `Context`, there may be access to these values before the `Context` is created that will use stack offsets.

A `Context` must be created if a method sends any messages or calls any methods or if a block closure is created that either does a non-local return or references a shared local.

Once a `Context` has been created, `self`, parameters, and locals (and the caller's stack) are encapsulated in the `Context` and must be accessed via the `Context`.

#### Stack example

When m4 has called m3 has called m2 has called m1 has called m0, but we haven't created a Context for m0 yet, the stack looks like (lower addresses at the top of the  diagrams):

| Stack  | Description                 | Pointers                                               |
| ------ | --------------------------- | ------------------------------------------------------ |
| ...    |                             | space for stack growth                                 |
| object | m0 parameters               | <--- sp                                                |
| object | m0 self                     |                                                        |
| ...    | m1 stack                    |                                                        |
| header | m1 header                   | <--- aContext                                          |
| tpc    | m1 threaded pc to return to |                                                        |
| npc    | m1 native pc to return to   |                                                        |
| ctxt   | m1 ContextPtr               | ---> m2 header                                         |
| method | m1 method                   |                                                        |
| object | m1 locals                   | allocated by pushContext                               |
| object | m1 parameters               | pushed by m2                                           |
| object | m1 self                     | pushed by m2                                           |
| ...    | m2 stack                    |                                                        |
| header | m2 header                   | <--- m1 ctxt                                           |
| tpc    | m2 threaded pc to return to |                                                        |
| npc    | m2 native pc to return to   |                                                        |
| ctxt   | m2 ContextPtr               | ---> m3 Context header (see example below on the heap) |
| method | m2 method                   |                                                        |
| object | m2 locals                   |                                                        |
| object | m2 parameters               |                                                        |
| object | m2 self                     |                                                        |
| ...    | m3 stack                    |                                                        |


| m3 Context | Description | Pointers |
| ----  | ------------------------ | --------------------------------------------------------- |
| header | m3 header |
| tpc    | m3 threaded pc to return to       |                                                           |
| npc    | m3 native pc to return to       |                                                           |
| ctxt   | m3 ContextPtr            | ---> m4 header                                            |
| method | m3 method                |                                                           |
| object | m3  locals |                                                           |
| object | m3 parameters |                                                           |
| ...    | ...                         |                                                           |
| object | m3 self                  |                                                           |
| ...    | m4 stack                 |                                                           |
| footer  | m3 footer | needed for all heap objects |


Note that the Context headers/size are set lazily because while they are on the stack, they are chained and physically contiguous. The partial header is also at the front of the object, rather than the end. The context is only turned into a proper object if it is promoted to the heap (via a spill or explicit reference).

A method will only create a Context if `thisContext` is referenced, or if a non-tail message send will be performed. If there is a `<primitive>`, this is only done after the primitive is evaluated and fails. If the primitive succeeds, it adjusts the stack and returns to the current context. Primitives that fail proceed to the next threaded function, which will typically create a Context.

#### Non-local Return and Exceptions
Non-local return does a return from the target Context, making all the intervening Contexts inaccessible (and hence garbage). In most cases this is within a couple of instructions of as efficient as a normal return.
The complication is if there is an `on:do:` or an `ensure:` between the target Context and the current Context. If all the Contexts were on a stack, this could be checked with a simple range check. However Contexts can migrate to the heap, at which point there is no longer any guaranteed ordering of addresses. The solution is that every time a trapping context is created, a thread-local counter is incremented. The good news is that if the target and current trap-context number is equal, all further checking is avoided. If the trap-context number is different for the target and current Contexts then a trapping context has been created between them. That context may no longer be active, but a more expensive check will have to be performed. The first check is if the top trapping context on the queue has a trap-context smaller than the target, in which case it is not a problem for this non-local return. in this case, any intervening `on:do:` contexts need to be removed from the queue, and the first intervening `ensure:` context needs to be returned to.

### ???
When sending a message, the current `Context` will be updated with the return PC, and the address of the CPS next function. When that method returns to the CPS next function, we will continue in native execution mode. If there *is* no native code, it will point to the next threaded primitive. If we need to switch execution to threaded mode (for debugging or single-stepping), we simply replace the return CPS address with the address of the next threaded function.

When we dispatch to a method, we always treat it as threaded code. This may seem expensive, but it is only 1 extra indirect jump. There are four kinds of method:
1. If there is no native version but the method starts with a primitive, the first address will point to the primitive. The rest of the thread is from the Smalltalk in the rest of the method (which could be preceded by a word to schedule optimization). The primitive will verify the selector and perform the operation. If successful, it will return to the caller. On failure, it simply proceeds with the following word of the threaded code.
2. If there is no native version and the method doesn't start with a primitive, the first address will point to a primitive to verify the selector and possibly schedule JIT compilation of this method. The rest of the thread is from the Smalltalk in the method.
3. If it is a native method generated from the threaded version, the first threaded address will simply be the address of the first native (CPS) function implementing the method. This function will verify the selector and implement 1 or more of the threaded words. It knows how many of these it implements, so knows what threaded-pc that it should save as the continuation in the Context. The remaining functions all will know which threaded-pc corresponds to each function.
4. If it is a hand-written native method, the first threaded address will simply be the address of the first native function. The remaining words will just be pointers to the other CPS functions used to implement the method. The functions all will know which threaded-pc corresponds to each function.

## Sends and Polymorphic Inline Caches
When a message send is encountered a sequence of threaded words are output:
1. Either a `SetupReturn` for a normal send, or a `SetupNoReturn` for a tail send. These both get the class from the receiver and the selector from the following word. `SetupReturn` also saves the return address (the address after the `DispatchDynamic` - the offset of which is also in that argument word) in the `Context`. `SetupNoReturn` doesn't save the return, but does restructure the stack (the parameters of which are also in that argument word). There are versions of both of these for unary and binary selectors as well as the general case, and there are versions of `SetupNoReturn` for with/without `Context` for the current method.
2. A sequence of `DispatchDirect` words, each followed by a `nil`.
	1. `DispatchDirect` gets the class of the receiver from the signature, hashes into the appropriate dispatch table, finds the appropriate method (which may require probing through the table).
	2. It then replaces the `DispatchDirect` and the `nil` with disambiguator for the method and the address of the `CompiledMethod`.
	3. Therefore, the next time we send here, the word will point directly into the code. So, these become a PIC, and bypass the hashing.
3. Finally a `DispatchDynamic`. This is the fallback and is never replaced. It does the same lookup of the class and hashing, but rather than probing for a match, it jumps indirectly through the dispatch table which is laid out the same way: a code address to execute followed by the `CompiledMethod` address.
4. This sequence will also be directly used for dispatch by any JIT code (although step 1 will be done in the JIT code with the native PC pointing to the return point in the JIT code, and the threaded PC pointing to the word after the `DynamicDispatch`). This means that: a) the JIT code benefits from the same PIC as the threaded code; b) no code addresses (other than the current method) are embedded in native code and would have to be invalidated; c) JIT dispatch is a little more expensive than it would otherwise be.
If a send is to a known method of a known class, then only one `DirectDispatch` need be generated and no `DynamicDIspatch`. If the send is to this method (after inlining) steps 2-3 will be replaced by a `Branch`.
## Dispatch
To find a compiled method from a signature, we need to get the dispatch table for the class, and then hash into that to find the actual method.

The signature is an augmented [[Symbol]] that has the class number in the top 24 bits of the word (which are zero in the `Symbol` encoding). So the first step is to extract the class number and index through a static table to the `Dispatch` table for the class. The number of classes is a compile-time configuration parameter to the Zag runtime, but the individual `Dispatch` tables are dynamically allocated.

Each `Dispatch` object has an array of `DispatchElement` structs. Each has a signature and a pointer to a `CompiledMethod`.

We then calculate a 'random' offset into the array. That is calculated as follows. The low 32 bits of the signature are the 24-bit hashed index of the symbol number and the 8-bit tag for `Symbol`. Because of the way this hash is created, this can be considered a fairly uniformly distributed number between 0 and 2^32 which means that dividing it by 2^32 will give a number between 0 and 1. If we multiply that by the number of elements in the array, we end up with a number between 0 and the size of the array-1. So if we label the hash as h and the size of the array as n. we have `h/2^32*n` which we can reorder as `h*n/2^32` and we can do that division via a shift. So we take the low 32 bits, do a 64-bit multiplication by the size of the array and shift right 32 bits, we end up with the equivalent of a mod operation (an integer between 0 and the size of the array-1) using only a multiply and a shift.

Starting from that position we check each `DispatchElement` for a match with the signature we are looking for. If we find a match, we execute the referenced method. If not, if the `DispatchElement` has a non-`nil` signature, we look at the next one, etc. This loop is guaranteed to finish because we will either find a match, a gap (because the hash table is generated with a loading factor of about 70%), or we will get to the end and there is an extra `nil` value at the end of the array.

If the `DispatchElement` has a `nil` signature, then there is no `CompiledMethod` for that selector, so we need to install one. To do this we lock the `Dispatch` object (in case another process is trying to add to this `Dispatch`) and then either install a place-holder if this `nil` is a gap, or create a replacement `Dispatch` object with a larger array and then install the place-holder.

The place-holder method will block if another process is compiling the target signature, or call Smalltalk code to find and compile the method (or a `DoesNotUnderstand` if there isn't one) and install it in the `Dispatch` table.  Then it will dispatch to the target `CompiledMethod`.

## Method dispatch
One of the defining aspects of object-oriented programming is that methods are customized to the object (or class). This requires dispatching to various code, dependent on the class of the object^[This is for class-based OOP like Smalltalk, Java, C++, Python, Ruby, etc.; for the much less common prototype-based OOP like Javascript or Self, there is still dispatch, but based on the object, not its class.]. Since this happens so frequently, optimizing the message dispatch is critical to performance.

#### Classic Smalltalk dispatch
Logically, Smalltalk message dispatch follows these steps:
 1. set T to the class of the target (or the current class's superclass if it's a `super` send)
 2. set C to T
 3. look up the selector symbol in C's `methodDict`
 4. if found, the value of the lookup is the method code - call it
 5. if not found and C has a superclass, set C to the superclass and continue at 3
 6. the method is not found, so create a Message object, set C to T and go back to 3 with the selector set to `doesNotUnderstand:` - we're guaranteed to find something on this go-round because `Object` implements `doesNotUnderstand:`.

This is not the whole story for 2 reasons:
 1. Some messages such as `ifTrue:ifFalse:` and `whileTrue:` and related messages are recognized by the compiler, and are turned into conditional byte code sequences, as an optimization.
 2. After the lookup described above, the target method is cached in the calling code, so the next time we do the lookup we should be very fast. This gets complicated because there could be objects from a different class in a subsequent lookup, so somewhat complex mechanisms are used to save the multiple method targets.
 
 See:
 - [Inline caching](https://en.wikipedia.org/wiki/Inline_caching)
 - [from Dynamic Dispatch](https://en.wikipedia.org/wiki/Dynamic_dispatch)
 - [from Late binding](https://en.wikipedia.org/wiki/Late_binding)

#### Java dispatch
Java has five opcodes to invoke methods, but the one we're interested in is `invokevirtual` which does virtual dispatch the same as Smalltalk^[the other 4 are because of the impoverished nature of Java object structure].

The difference is that the Java compiler statically knows the index into the dispatch table, so there is no need for a dictionary lookup. The dispatch table for each class has a prefix of a copy of the dispatch table from its superclass, followed by the methods defined in this class. Any methods that override superclass methods replace the corresponding method in the prefix. Since the names of all of the legal methods are known, finding the method for a particular name requires a simple index (which doesn't even have to be range checked).

This *flat dispatch* is many times faster than the classic Smalltalk dispatch, and is only slower than direct calling by a couple memory accesses and and a complete pipeline stall.

The same thing could be done for Smalltalk, if we knew which class the object was an instance of. Failing that, for every class (there are over 20,000 classes in a recent [Pharo](https://pharo.org) image, we could have to have a dispatch table with an entry for all message names (there are over 62,000 method names in the same image). Consuming over 10GB of memory for dispatch tables is clearly excessive.

Even if we somehow knew the class of the object, the tables would still be excessive because of the size of the Smalltalk Object class compared with the Java Object class. The Java Object class only has 11 methods, whereas the Smalltalk Object class has over 460 methods. Because this approach to dispatch requires a superclass method prefix, this would lead to over 80MB of dispatch tables - still excessive (and would have horrible cache locality).

#### Our approach
We lazily build a single dispatch table for each class, which includes not just the methods of the class, but also all the inherited methods that have been invoked.

| Dispatch table entry for a class |                        |
| -------------------------------- | ---------------------- |
| Hash multiplier                  | a u32                  |
| method pointers                  | an array of pointers to threads |

The sequence to look up a method is:
1. use the selector hash (symbol id (prehashed by multiplying by 24 bit inverse Phi) and arity - 32 bits)
2. convert to a u64 and then multiply by the hash multiplier
3. shift right 32 bits(the multiply and shift replace a modulo)
4. use that as the index into the array
5. jump to the threaded code

This dispatch is near-optimal. The method will check that the selector matches, else call DNU

The tables are built and sized for low conflict for prime-sized tables, so there will be very few conflicts, but where there are conflicts the method pointer will point to a second-level lookup, so there is no check - just the jump.

The methods listed are from anywhere in the hierarchy, but only methods that have actually been sent to any instance of this class. Super methods will never appear, because they will all be inlined or called directly (without dispatch).

## Does Not Understand

DNU can happen either because the dispatch pointer points to DNU code or because the dispatching selector didn't match the selector for the method. But in either case, a DNU simply means that there is no CompiledMethod for the selector for this class. This may be because of a true DNU, or because an appropriate method exists, but hasn't been compiled yet.

The DNU code will look in the method list for the class and its superclasses for a method with the correct selector. If found, the method will be compiled for the target class, and inserted into the dispatch table. If not found, a Message object will be created and the object will be sent a `doesNotUnderstand:` message. This may also trigger a DNU, but this time we are guaranteed that an appropriate method exists, because there is an implementation of `doesNotUnderstand:` in Object.

## Compilation

Methods understand the message `compileForClass:withCodeGenerator:` which takes a target class and a code generator, and converts the AST of the method to a series of calls to the code generator. In the simplest case, the AST is directly converted to threaded code. More generally there are lots of [Optimizations](Optimizations.md).
## BlockClosures

BlockClosures are defined within a method or another block. A closure may:
1. contain a non-local return in which case it has to have a reference to the context in which it was created
2. contain values that are not modified in an ancestor after the closure is created and is either not modified in the block either, or is not referenced in an ancestor or a sibling block after the closure is created
3. reference or modify values that are also referenced or modified by the main method code or another block in which case it has to have a reference to a ClosureData object where the mutable values are stored

Consider the following method, defined in Integer
```Smalltalk
foo: p1 bar: p2
	| l1 l2 l3 |
	p1 < p2 ifTrue: [ ^ self ].
	l1 := p2.
	l2 := p1 \\ p2.
	l3 := p2 - l2.
	[ l1 < p1 ] whileTrue: [ 
		l1 := l1 + 1.
		l1 = l3 ifTrue: [ ^ 1 ] ].
	^ l1
```

The method would compile to:
```zig
// self-7 p1-6 p2-5 l2-4 closureData-3 BCself-2 BC1-1 BC2-0
	&e.verifySelector,
    &e.pushContext, "^",
	// define all blocks here
    &e.closureData, 3 + (1 << 12), // local:3 size:1 (offset 1 is l1)
    &e.nonlocalClosure_self, 2, // [^ self] local:2
    &e.blockClosure, "0foo:bar::1", 1 + (1 << 12) + (0 << 20)   + (3 << 32),
	    // local:1, 1 field, no includeContext, closureData at local3
    &e.blockClosure, "1foo:bar::2", 0 + (1 << 12) + (255 << 20) + (3 << 32),
		// local:0, 1 field, includeContext, closureData at local3
    // all blocks defined by now
    &e.pushLocal, 6, // p1
    &e.popLocalData, 1 + (3 << 12),
	    // p1 (read-only) copy offset 3 in local 1 (field in BC1)
    &e.pushLocal, 6, // p1
    &e.pushLocal, 5, // p2
    &e.send1,      Sym.@"<",
    &e.pushLocal, 2, // [^ self]
    &e.send1,      Sym.@"ifTrue:",
    &e.drop, // discard result from ifTrue: (if it returned)
    &e.pushLocal, 5, // p2
    &e.popLocalData, 3 + (1 << 12), // l1
    &e.pushLocal, 6, // p1
    &e.pushLocal, 5, // p2
    &e.send1,      Sym.@"\\",
    &e.popLocal, 4, // l2
    &e.pushLocal, 5, // p2
    &e.pushLocal, 4, // l2
    &e.send1,      Sym.@"-",
    &e.popLocalData, 0 + (4 << 12), // l3 offset 4 in local 0
    &e.pushLocal, 1, // BC1 [ l1 < p1 ]
    &e.pushLocal, 0, // BC2 [ l1 := ... ]
    &e.send1,      Sym.@"whileTrue:",
    &e.drop,
    &e.pushLocalData, 3 + (1 << 12), // l1
    &e.returnTop,
```

The first block would be:
```zig
// foo:bar::1    [ l1 < p1 ]
// self-0
    &e.verifySelector,
    &e.pushContext, "^",
    &e.pushLocalDataData, 0 + (2 << 12) + (1 << 24),
	    // l1 offset 1 in offset 2 in local 0
    &e.pushLocalData, 0 + (3 << 12),
	    // p1 offset 3 in local 0
    &e.send,          Sym.@"<",
    &e.returnTop,
```

and the second block would be:
```zig
// foo:bar::2"   [ l1 := l1 + 1.  l1 = l3 ifTrue: [ ^ 1 ] ]
// self-1 BCone-0
    &e.verifySelector,
    &e.pushContext, "^",
    &e.nonlocalClosure_one, 0 + (1 << 12) + (2 << 24),
	    // [^ 1] local:0 context at offset 2 in local 1
    &e.pushLocalDataData, 1 + (3 << 12) + (1 << 24),
	    // l1 offset 1 in offset 3 in local 1
    &e.pushLiteral,       Object.from(1),
    &e.send,              Sym.@"+",
    &e.popLocalDataData, 1 + (3 << 12) + (1 << 24),
	    // l1 offset 1 in offset 3 in local 1
    &e.pushLocalDataData, 1 + (3 << 12) + (1 << 24),
	    // l1 offset 1 in offset 3 in local 1
    &e.pushLocalData, 1 + (4 << 12),
	    // l3 offset 4 in local 1
    &e.send,          Sym.@"=",
    &e.pushLocal, 0, // [^ 1]
    &e.send,      Sym.@"ifTrue:",
    &e.returnTop,
```

The following stack contents reflect the state after the expression `3 foo: 7 bar: 2` has partially executed and is at the start of the first execution of BlockClosure 2

| | BC1 Context  | Description              | Pointers                                                  |
|  - | ------ | ------------------------ | --------------------------------------------------------- |
| | ...    |               | space for stack growth                                                   |
|0| header | BC1 Context header                |       <--- sp                                       |
|1| tpc    | whileTrue: threaded pc to return       |                                                           |
|2| npc    | whileTrue: native pc to return to       |                                                           |
|3| ctxt   | whileTrue: ContextPtr            | ---> whileTrue: Context header (8)|
|4| object | trapContextNumber | 0 |
|5| method | foo:bar:1                |                                                           |
|6| clsPtr | [^ 1] |                --> foo:bar: header   (27)                                        |
|7| object | self                  |    BC1                                                       |

| | whileTrue: Context | Description | Pointers |
| - | - | - | - |
|8| header | whileTrue: Context header                | <--- BC1 ctxt                                              |
|9| tpc    | foo:bar: threaded pc to return      |                                                           |
|10| npc    | foo:bar: native pc to return to       |                                                           |
|11| ctxt   | foo:bar: ContextPtr            | ---> foo:bar: Context (27) |
|12| object | trapContextNumber | 0 |
|13| method | whileTrue:                |                                                           |
|14| clsPtr | aBlock |       BC1                                                    |
|15| clsPtr | self                  | BC2                                                          |

| | BC2 | Description | Pointers|
| - | - | - | - |
|16| object   |     l3        |                                             |
|17| clsDataPtr |  |   -->  clsDataPtr       (26)                                             |
|18| contextPtr | foo:bar: ContextPtr                |                                                           |
|19| method | foo:bar:2  |                                                          |
|20| footer | BlockClosure - 4 words  |                                                          |

| | BC1| Description | Pointers |
| - | - | - | - |
|21| object   | p1            |                                             |
|22| clsDataPtr |  |   -->  clsDataPtr         (26)                                            |
|23| method | foo:bar:1  |                                                          |
|24| footer | BlockClosure - 3 words |                                                          |

| | ClosureData | Description | Pointers |
| - | - | - | - |
|25| object | l1                  |                                                           |
|26| footer | ClosureData - 1 word                |     <--- clsDataPtr                                                      |

| | foo:bar: Context | Description | Pointers|
| - | - | - | - |
|27| header | foo:bar: header                | <--- whileTrue: ctxt                                              |
|28| tpc    | caller threaded pc to return       |                                                           |
|29| npc    | caller native pc to return to   (?)    |                                                           |
|30| ctxt   | caller ContextPtr            | ---> caller Context (?) |
|31| object | trapContextNumber | 0 |
|32| method | foo:bar:                |                                                           |
|33| clsPtr | [ l1 := ... ] |  -->     BC1  footer (24)                                                 |
|34| clsPtr | [ l1<p2 ] |         -->   BC2 footer (20)                                             |
|35| clsPtr | [^ self] |                --> foo:bar: header (27)                                          |
|36| clsDataPtr |  |  -->    ClosureData footer      (26)                                               |
|37| object | l2 |                                                           |
|38| object | p2 |                                                           |
|39| object | p1 |                                                           |
|40| object | self                  |                                                           |
|41| ...    | caller stack                 |                                                           |



## Interpretation Classes

**This hasn't been updated in a while... so may not be accurate**

These are classes that the interpreter understands for execution. These are the names in Pharo - the AS prefix is removed in the ZagSmalltalk image.
### ASSend
Fields:
- whitespace - nil or a string that should follow the token in textual representation - ignored by interpreter
- target - an expression that will be sent the message
- selector - a symbol that is sent if target is self, then the selector can be a special symbol `super@message` which will be coded into the table for the class just like any other selector, but the code will be the appropriate method from the superclass of the current method's class.
- arguments - a sequence of expressions
### ASSelf
- whitespace - nil or a string that should follow the token in textual representation - ignored by interpreter
### ASLiteral
- whitespace - nil or a string that should follow the token in textual representation - ignored by interpreter
- value - some literal value
	- could be atomic like a number, boolean, character
	- could be an array
		-  if all the values are literals (or arrays of literals) it will be represented as a literal array `#()`
	- else it will be represented as a constructed array `{}`
	- could be an ASMethod for a block (name will be `value`, `value:`, etc.)
If whitespace is nil and value is literal or array of literal, the ASLiteral can be omitted, and the literal be the expression value.
### ASReturn
- whitespace - nil or a string that should follow the token in textual representation - ignored by interpreter
- expression - the value to be returned
- nonLocal - true if the return is a non-local return
### ASSequence
- whitespace - nil or a string that should follow the token in textual representation - ignored by interpreter
- sequence - an array of expressions
If whitespace is nil, the ASSequence can be omitted, and the array be the expression value.
### ASLoad
- whitespace - nil or a string that should follow the token in textual representation - ignored by interpreter
- target - value of the base for the load
- offset - field index - forced SmallInteger
### ASStore
- whitespace - nil or a string that should follow the token in textual representation - ignored by interpreter
- target - value of the base for the store
- offset - field index
- expression - the value to be stored
### ASMethod
- whitespace - nil or a string that should follow the token in textual representation - ignored by interpreter
- name - a symbol
- primitive - primitive number or 0 - forced SmallInteger
- an ASSequence for the body
- an array of parameter symbols
- an array of locals symbols
- class - the class this method is defined in
### Array
- Contains fields for ASSequence - always less than 63 fields so will be compact
Can often stand in for ASSequence if there is no whitespace to be included.


## Auxiliary Classes
These are not part of the main interpret loop, but are referenced by it:
### ASBehavior
### ASClass
- methods - an array of method definitions for the instances
- instVarNames - an array of instance variable names for the instances
- superclass - the superclass
- name - a symbol
- format - the format code to use when creating instances
- classVars - an array of class variables
- sharedPools - an array of shared pool classes
### ASMetaclass
- methods - an array of method definitions for the class
- instVarNames - an array of instance variable names for the class
- superclass - the superclass
- instance - instanceSide
### ASBlockClosure
- method code
- index of `self`
- block-local variables (including values shared with the method)

## Forced SmallInteger
- means that the field is optimized to be treated as a positive SmallInteger, even if it isn't
- the bit pattern is ANDed with an appropriate value (63 for field references, 2047 for primitives), then MINed with the number of legal values (fields of the object)
- doesn't break integrity, but will produce specious results if messed with, and could produce runtime errors if references a non-array


### Rust structure

## Image format
The image format is [described here](ImageFormat.md)
## Dependencies/Porting
[DebuggableASTInterpreter](https://github.com/carolahp/DebuggableASTInterpreter)
[PharoCandle](https://github.com/guillep/PharoCandle)
