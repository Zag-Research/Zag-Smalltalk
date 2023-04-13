## Execution

This system uses a dual execution model.  For each method, there is a threaded implementation and possibly a native implementation. There is no classical "interpreter". The closest is the threaded implementation. 

### Threaded Method Implementation
The threaded implementation is a sequence of addresses of functions implementing primitives  and control operations. Every method has a threaded implementation. One of the "registers" that is passed through the thread is a flag indicating whether the current thread needs to check for interruptions. Every control operation checks this flag before passing control along to the next function. This allows the threaded implementation to single step through the method. Control is passed using an indirect tail-call.

### Native Method Implementation
The native implementation is a sequence of functions implementing everything between actual message sends. After inlining, this can be a significant amount of code. Each function passes control to the next code via a tail-call, passing the same registers as the threaded implementation. This means that native code implements continuation-passing style, and no native activation records are created. One of the registers that is passed is the program counter... that is, the next threaded code to be executed. Because one native function can implement several threaded equivalents, these may be non-sequential.

When sending a message, the current Context will be updated with the return PC, and the address of the CPS next function. When that method returns to the CPS next function, we will continue in native execution mode. If there *is* no native code, it will point to a trampoline function that will execute the next threaded primitive. If we need to switch execution to threaded mode (for debugging or single-stepping), we simply replace the return CPS address with the trampoline function.

When we dispatch to a method, we always treat it as threaded code. This may seem expensive, but it is only 1 extra indirect jump. If we are executing in native mode, the first threaded address will simply point to the address of the first native function. Or, if there is no native version, the first address will point to a primitive to schedule compilation of this method. If we are executing in threaded mode (single-stepping, for example) we will skip the first word and thereby execute the first real instruction of the threaded implementation of the method.

## The stack and Contexts

The native stack is only used when calling non-Smalltalk functions. All Smalltalk stack frames (Contexts) are implemented in a Smalltalk linked list of Contexts. They are initially allocated on the Smalltalk stack which resides at the top of the Nursery for a Thread (and are actually not completely filled in as long as they reside in the stack). If the stack becomes too deep, the Contexts will be copied to the Thread's Nurseery arena (and potentially to the Teen and then Global arenas). Similarly, if `thisContext` is referenced, the Context (and ones it links to) will be copied to the heap.

There are several reasons for this, but the primary reasons are that: a) all the GC roots are on the stack or in the current Context; and b) switching between interpreter and native implementation is seamless.

#### Stack example

When m3 has called m2 has called m1 has called m0, but we haven't created a Context for m0 yet, the stack looks like:

| Stack  | Description              | Pointers                                                  |
| ------ | ------------------------ | --------------------------------------------------------- |
| ...    | m3 stack                 |                                                           |
| object | m2 self                  |                                                           |
| object | m2 parameters and locals |                                                           |
| ...    |                          |                                                           |
| method | m2 method                |                                                           |
| ctxt   | m2 ContextPtr            | ---> m3 header (which could be above this or on the heap) |
| npc    | m2 native pc to return to       |                                                           |
| tpc    | m2 threaded pc to return to       |                                                           |
| header | m2 header                | <--- m1 ctxt                                              |
| ...    | m2 stack                 |                                                           |
| object | m1 self                  |                                                           |
| object | m1 parameters and locals |                                                           |
| ...    |                          |                                                           |
| method | m1 method                |                                                           |
| ctxt   | m1 ContextPtr            | ---> m2 header                                            |
| npc    | m1 native pc to return to       |                                                           |
| tpc    | m1 threaded pc to return to       |                                                           |
| header | m1 header                | <--- aContext                                             |
| ...    | m1 stack                 |                                                           |
| object | m0 self                  |                                                           |
| object | m0 parameters            |                                                           |
| ...    | m0 temps                 | <--- sp                                                   |
Note that the Context headers/size are set lazily because while they are on the stack, they are chained and physically contiguous. The partial header is also at the front of the object, rather than the end. The context is only turned into a proper object if it is promoted to the heap (via a spill or explicit reference).

A method will only create a Context if `thisContext` is referenced, or if a non-tail message send will be performed. If there is a `<primitive>`, this is only done after the primitive is evaluated and fails. Primitives that can fail are followed by an offset to branch to if successful. If the primitive can do the whole job of the method if it succeeds will have a -1 offset (which is otherwise meaningless) which says to return from the method on success. This will be followed by the thread-code to create the context.

#### Non-local Return and Exceptions
Non-local return does a return from the target Context, making all the intervening Contexts inaccessible (and hence garbage). In most cases this is within a couple of instructions of as efficient as a normal return.
The complication is if there is an `on:do:` or an `ensure:` between the target Context and the current Context. If all the Contexts were on a stack, this could be checked with a simple range check. However Contexts can migrate to the heap, at which point there is no longer any guaranteed ordering of addresses. The solution is that every time a trapping context is created, a thread-local counter is incremented. The good news is that if the target and current trap-context number is equal, all further checking is avoided. If the trap-context number is different for the target and current Contexts then a trapping context has been created between them. That context may no longer be active, but a more expensive check will have to be performed. The first check is if the top trapping context on the queue has a trap-context smaller than the target, in which case it is not a problem for this non-local return. in this case, any intervening `on:do:` contexts need to be removed from the queue, and the first intervening `ensure:` context needs to be returned to.

## Method dispatch
One of the defining aspects of object-oriented programming is that methods are customized to the object (or class). This requires dispatching to various code, dependent on the class of the object^[This is for class-based OOP like Smalltalk, Java, C++, Python, Ruby, etc.; for the much less common prototype-based OOP like Javascript or Self, there is still dispatch, but based on the object, not its class.]. Since this happens so frequently, optimizing the message dispatch is critical to performance.

#### Classic Smalltalk dispatch
Logically, Smalltalk message dispatch follows these steps:
 1. set C to the class of the target (or the current class's superclass if it's a `super` send)
 2. look up the selector symbol in C's `methodDict`
 3. if found, the value of the lookup is the method code - call it
 4. if not found and C has a superclass, set C to the superclass and continue at 2
 5. the method is not found, so create a Message object, set C to the class of the target and go back to 2 with the selector set to `doesNotUnderstand:` - we're guaranteed to find something on this go-round because `Object` implements `doesNotUnderstand:`.

This is not the whole story for 2 reasons:
 1. Some messages such as `ifTrue:ifFalse:` and `whileTrue:` and related messages are recognized by the compiler, and are turned into conditional byte code sequences.
 2. After the lookup described above, the target method is cached in the calling code, so the next time we do the lookup we should be very fast. This gets complicated because there could be objects from another class in a subsequent lookup, so somewhat complex mechanisms are used to save the multiple method targets.
 
 See:
 - [Inline caching](https://en.wikipedia.org/wiki/Inline_caching)
 - [from Dynamic Dispatch](https://en.wikipedia.org/wiki/Dynamic_dispatch)
 - [from Late binding](https://en.wikipedia.org/wiki/Late_binding)

#### Java dispatch
Java has five opcodes to invoke methods, but the one we're interested in is `invokevirtual` which does virtual dispatch the same as Smalltalk^[the other 4 are because of the impoverished nature of Java object structure].

The difference is that the Java compiler statically knows the index into the dispatch table, so there is no need for a dictionary lookup. The same thing could be done for Smalltalk, if we knew which class the object was an instance of. Failing that, for every class (there are over 20,000 classes in a recent [Pharo](https://pharo.org) image, we would have to have a dispatch table with an entry for all message names (there are over 62,000 method names in the same image). Consuming over 10GB of memory for dispatch tables is clearly excessive.

Even if we somehow knew the class of the object, the tables would still be excessive because of the size of the Smalltalk Object class compared with the Java Object class. The Java Object class only has 11 methods, whereas the Smalltalk Object class has over 460 methods, leading to 80MB of dispatch tables - still excessive (and would have horrible cache locality).

#### Our approach
We lazily build a single dispatch table for each class, which includes not just the methods of the class, but also all the inherited methods that have been invoked.

| Dispatch table entry for a class |                        |
| -------------------------------- | ---------------------- |
| Hash multiplier                  | a u32                  |
| second hash multiplier | a u16                                 |                        |
| superclass index                 | a u16                  |
| method pointers                  | an array of MethodType |

The sequence to look up a method is:
1. use the selector hash
2. multiply with wrap by the hash multiplier
3. shift right by the low 5 bits of the multiplier
4. use that as the index into the array
5. call the method

This dispatch is near-optimal. The method will check that the selector matches, else call DNU

The tables are built as a near-perfect for power-of-2 tables, so there will be very few conflicts, but where there are conflicts the method pointer will point to some kind of second-level lookup - could be an and, could be linear scan with symbols - for any, the data will follow the first-level method pointers in the table.

The methods listed are from anywhere in the hierarchy, but only methods that have actually been sent to any instance of this class. Super methods will never appear, because they will all be inlined - unless they are recursive

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
	- - could be atomic like a number, boolean, character
	- - could be an array
	- - - if all the values are literals (or arrays of literals) it will be represented as a literal array `#()`
	- - - else it will be represented as a constructed array `{}`
	- - could be an ASMethod for a block (name will be `value`, `value:`, etc.)
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
- target - value of the base for the load
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
