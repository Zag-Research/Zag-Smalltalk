## Execution

This system uses a dual execution model.  For each method, there is a threaded implementation and possibly a native implementation. There is no classical "interpreter". The closest is the threaded implementation. 

### Threaded Method Implementation
The threaded implementation is a sequence of addresses of functions implementing embedded primitives  and control operations. Every method has a threaded implementation. One of the "registers" that is passed through the thread is a flag indicating whether the current thread needs to check for interruptions. Every control operation checks this flag before passing control along to the next function. This allows the threaded implementation to single step through the method. Control is passed using an indirect tail-call.

### Native Method Implementation
The native implementation is a sequence of functions implementing everything between actual message sends. After inlining, this can be a significant amount of code. Each function passes control to the next code via a tail-call, passing the same registers as the threaded implementation. This means that native code implements continuation-passing style, and no native activation records are created. One of the registers that is passed is the program counter... that is, the next threaded code to be executed. Because one native function can implement several threaded equivalents, these may be non-sequential.

When sending a message, the current Context will be updated with the return PC, and the address of the CPS next function. When that method returns to the CPS next function, we will continue in native execution mode. If there *is* no native code, it will point to the next threaded primitive. If we need to switch execution to threaded mode (for debugging or single-stepping), we simply replace the return CPS address with the address of the next threaded function.

When we dispatch to a method, we always treat it as threaded code. This may seem expensive, but it is only 1 extra indirect jump. If we are executing in native mode, the first threaded address will simply point to the address of the first native function. Or, if there is no native version, the first address will point to a primitive to verify the selector and possibly schedule JIT compilation of this method. If we are executing in threaded mode (single-stepping, for example) we will skip the first word and thereby execute the first real instruction of the threaded implementation of the method.

## The stack and Contexts

The native stack is only used when calling non-Smalltalk functions. All Smalltalk stack frames (Contexts) are implemented in a Smalltalk linked list of Contexts. They are initially allocated on the Smalltalk stack which resides at the beginning of a Process (and are actually not completely filled in as long as they reside in the stack). If the stack becomes too deep, the Contexts will be copied to the Process' Nursery arena (and potentially to the Global arena). Similarly, if `thisContext` is returned from a method, the Context (and ones it links to) will be copied to the heap.

There are several reasons for this, but the primary reasons are that: a) all the GC roots are on the stack or in the current Context; and b) switching between interpreter and native implementation is seamless.

#### Stack example

When m4 has called m3 has called m2 has called m1 has called m0, but we haven't created a Context for m0 yet, the stack looks like (lower addresses at the top of the  diagrams):

| Stack  | Description              | Pointers                                                  |
| ------ | ------------------------ | --------------------------------------------------------- |
| ...    |               | space for stack growth                                                   |
| object | m0 parameters            |  <--- sp                                                         |
| object | m0 self                  |                                                           |
| ...    | m1 stack                 |                                                           |
| header | m1 header                | <--- aContext                                             |
| tpc    | m1 threaded pc to return to       |                                                           |
| npc    | m1 native pc to return to       |                                                           |
| ctxt   | m1 ContextPtr            | ---> m2 header                                            |
| method | m1 method                |                                                           |
| object | m1 locals |     allocated by pushContext                                                      |
| object | m1 parameters  | pushed by m2                                                          |
| object | m1 self                  | pushed by m2                                                          |
| ...    | m2 stack                 |                                                           |
| header | m2 header                | <--- m1 ctxt                                              |
| tpc    | m2 threaded pc to return to       |                                                           |
| npc    | m2 native pc to return to       |                                                           |
| ctxt   | m2 ContextPtr            | ---> m3 Context header (see example below on the heap) |
| method | m2 method                |                                                           |
| object | m2 locals |                                                           |
| object | m2 parameters |                                                           |
| object | m2 self                  |                                                           |
| ...    | m3 stack                 |                                                           |


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

The difference is that the Java compiler statically knows the index into the dispatch table, so there is no need for a dictionary lookup. The dispatch table for each class has a prefix of a copy of the dispatch table from its superclass, followed by the methods defined in this class. Any methods that override superclass methods replace the corresponding method in the prefix. Since the names of all of the legal methods are known, finding the method for a particular name requires a simple index (which doesn't even have to be range checked)..

The same thing could be done for Smalltalk, if we knew which class the object was an instance of. Failing that, for every class (there are over 20,000 classes in a recent [Pharo](https://pharo.org) image, we could have to have a dispatch table with an entry for all message names (there are over 62,000 method names in the same image). Consuming over 10GB of memory for dispatch tables is clearly excessive.

Even if we somehow knew the class of the object, the tables would still be excessive because of the size of the Smalltalk Object class compared with the Java Object class. The Java Object class only has 11 methods, whereas the Smalltalk Object class has over 460 methods. Because this approach to dispatch requires a superclass method prefix, this would lead to over 80MB of dispatch tables - still excessive (and would have horrible cache locality).

#### Our approach
We lazily build a single dispatch table for each class, which includes not just the methods of the class, but also all the inherited methods that have been invoked.

| Dispatch table entry for a class |                        |
| -------------------------------- | ---------------------- |
| Hash multiplier                  | a u32                  |
| superclass index                 | a u16                  |
| method pointers                  | an array of pointers to threads |

The sequence to look up a method is:
1. use the selector hash (symbol id and arity)
2. 32-bit multiply with wrap by a constant hash
3. multiply by the hash multiplier
4. shift right (the 2 multiplies and shift replace a modulo)
5. use that as the index into the array
6. jump to the threaded code

This dispatch is near-optimal. The method will check that the selector matches, else call DNU

The tables are built and sized for low conflict for prime-sized tables, so there will be very few conflicts, but where there are conflicts the method pointer will point to a second-level lookup.

The methods listed are from anywhere in the hierarchy, but only methods that have actually been sent to any instance of this class. Super methods will never appear, because they will all be inlined - unless they are recursive.

Note that because the hierarchy is flattened, and not all super sends can be inlined, there may be multiple methods with the same selector. These are disambiguated with tags in the arity field.

## BlockClosures

BlockClosures are defined within a method or another block. A closure may:
1. contain a non-local return in which case it has to have a reference to the context in which it was created
2. contain immutable values that are not modified after the block is created
3. reference or modify values that are also referenced or modified by the main method code or another block in which case it has to have a reference to a ClosureData object where the mutable values are stored

Consider the following method, defined in Integer and called with `3 foo: 7 bar: 2` at the start of the first execution of BlockClosure 2
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

| | foo:bar: | Description | Pointers|
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
