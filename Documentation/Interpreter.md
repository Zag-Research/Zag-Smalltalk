## The Interpreter

Ideally the interpreter (and [the JIT](JIT.md)) are defined within [AST Classes](AST_Classes.md). The trick is to get enough bootstrap in place that the interpreter can run.

### Method dispatch
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

### The table below **is wrong** (from a previous design).

| Hash table for a class                   |                                      |
| ---------------------------------------- | ------------------------------------ |
| Pointer to the class                     |                                      |
| hash table mask (2^n-1)*8                |                                      |
| hash entry 0 - points to 2nd level below | `<--` object has pointer to here     |
| hash entry 1 - ditto                     |                                      |
| ...                                      |                                      |
| hash entry 2^n-1 - ditto                     |                                      |
| symbol hash                              | `<--` pointed to by one of the above |
| method address                           |                                      |
| symbol hash                              |                                      |
| method address                           |                                      |
| symbol hash                              | `<--` pointed to by one of the above |
| method address                           |                                      |
| symbol hash                              |                                      |
| ...                                      |                                      |
| 0                                        |  designates end of table                                     |
| 1                                        |                                      |

The sequence to look up a method is:
1. get the symbol hash value - the offset from the start of the symbol table
2. bit-and it with the hash table mask
3. offset into the table to get a pointer to the 2nd level table
4. the 2nd level table is a sequence of symbol hash value/method address pairs
5. scan linearly through the 2nd level table looking for a match
	1. if found, use the next word as an address for the method code
	2. if 0, then this symbol isn't in the table, fire off a DNU

Although we're doing a linear scan for the second level, the size of the first-level table will mean that only very rarely will we have to search beyond 2 entries. There will be zero entries in the original table, but they may be filled in over time. This will mean that DNUs may have to search a little longed in some circumstances, but the cost of this search will be swamped by the other requirements of a DNU.

The methods listed are from anywhere in the hierarchy, but only methods that have actually been sent to any instance of this class.

## Interpretation Classes
These are classes that the interpreter understands for execution. These are the names in Pharo - the AS prefix is removed in the ASTSmalltalk image.
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
