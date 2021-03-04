## The Interpreter

Ideally the interpreter (and [the JIT](Jit.md)) are defined within [AST Classes](AST_Classes.md). The trick is to get enough bootstrap in place that the interpreter can run.

### Method dispatch
One of the defining aspects of object-oriented programming is that methods are customized to the object (or class). This requires dispatching to various code, dependent on the class of the object^[This is for class-based OOP like Smalltalk, Java, C++, Python, Ruby, etc.; for the much less common prototype-based OOP like Javascript or Self, there is still dispatch, but based on the object, not its class.]. Since this happens so frequently, optimizing the message dispatch is critical to performance.

#### Classic Smalltalk dispatch
Logically, Smalltalk message dispatch follows these steps:
 1. set C to the class of the target (or its superclass if it's a `super` send)
 2. look up the selector symbol in C's `methodDict`
 3. if found, the value of the lookup is the method code - call it
 4. if not found and C has a superclass, set C to the superclass and continue at 2
 5. the method is not found, so create a Message object and go back to 1 with the selector set to `doesNotUnderstand:` - we're guaranteed to find something on this go-round because `Object` implements `doesNotUnderstand:`.

This is not the whole story for 2 reasons:
 1. Some messages such as `ifTrue:ifFalse:` and `whileTrue:` and related messages are recognized by the compiler, and are turned into conditional byte code sequences.
 2. After the lookup described above, the target method is cached in the calling code, so the next time we do the lookup we should be very fast. This gets complicated because there could be objects from another class in a subsequent lookup, so somewhat complex mechanisms are used to save the multiple method targets.

#### Java dispatch
Java has five opcodes to invoke methods, but the one we're interested in is `invokevirtual` which does virtual dispatch the same as Smalltalk^[the other 4 are because of the impoverished nature of Java object structure].

The difference is that the Java compiler statically knows the index into the dispatch table, so there is no need for a dictionary lookup. The same thing could be done for Smalltalk, if we knew which class the object was an instance of. Failing that, for every class (there are over 20,000 classes in a recent [Pharo](https://pharo.org) image, we would have to have a dispatch table with an entry for all message names (there are over 62,000 method names in the same image). Consuming over 10GB of memory for dispatch tables is clearly excessive.

Even if we somehow knew the class of the object, the tables would still be excessive because of the size of the Smalltalk Object class compared with the Java Object class. The Java Object class only has 11 methods, whereas the Smalltalk Object class has over 460 methods, leading to 80MB of dispatch tables - still excessive (and would have horrible cache locality).

#### Our approach
We build a single dispatch table for each class, which includes not just the methods of the class, but also all the inherited methods.