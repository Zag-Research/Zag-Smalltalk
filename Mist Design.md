Martin McClure

Copyright © 2014-2015 Martin McClure

This work is licensed under the Creative Commons Attribution-ShareAlike 4.0 International License. To view a copy of this license, visit http://creativecommons.org/licenses/by-sa/4.0/ or send a letter to Creative Commons, 444 Castro Street, Suite 900, Mountain View, California, 94041, USA.

The date of the latest revision is:

**2015-10-10**

This document is occasionally updated. The most recent version is available in the Mist project git repository, the link to which can be found at http://mist-project.org.

This is in largely random order for now...

But it _is_ information about the design of Mist. Primarily the design of Mist’s _implementation_, as opposed to its language design.

  

**Project Values and Strategies/Tactics**

The Mist project is guided by a set of values. The values are:

-   **Simplicity**
-   **Consistency**
-   **Self-sufficiency**
-   **Speed**
-   **Craziness**

We have strategies and tactics to help promote some of these values. Let’s look at each value in turn.

**Simplicity**

Simplicity is very difficult to achieve, but is very valuable. A major goal of the Mist project is to build a system that is significantly less complex than other Smalltalk and Smalltalk-like systems of similar power. Tactics:

-   Implement the **simplest thing first**.
-   **Increase complexity only when needed** for increased functionality, or to promote the other Mist values.
-   When possible, **measure the payoff** of increased functionality. If the payoff is insufficient relative to the complexity cost, go back to the simple way. This is especially relevant for trading complexity for speed – speed is measurable, and so is complexity (up to a point).
-   **Spend memory freely**. Memory is not free, but it is orders of magnitude less expensive than it was when most Smalltalks were designed. We do not want to _waste_ memory, but if we can _buy_ simplicity (or another value, such as speed) by _spending_ a reasonable amount of memory, we will do so.

**Consistency**

The desire for simplicity leads us to have a small number of design rules. The desire for consistency leads us to try to apply those rules uniformly across the entireMistImplementationDesign.MITE.odt system, avoiding special cases in which the usual rules don’t apply. In particular:

-   Mist will **not have reserved selectors**. Most Smalltalk implementations optimize sends of #==, #ifTrue:, #to:by:do:, and others. Mist will not do this. This conceptual simplicity comes at the cost of some implementation complexity, which is covered later in this document.

Self-sufficiency

The project slogan, “There is no ‘C’ in ‘Smalltalk’” refers to this value. Most Smalltalk implementations use a virtual machine that is either written in C or uses a C compiler to build it. Mist will not. On Mist’s initial platform (X_64 Linux) the only external software dependency is the Linux kernel. For core functions, Mist does not even require libc.

Note that not _requiring_ anything from the user-space C world does not mean that Mist should not inter-operate with that world. In fact, we want Mist to be very friendly with that world. Mist should eventually be able to easily call out to foreign functions and use foreign data, accept call-ins, and be packaged as a shared library.

**Speed**

We want Mist to be usable, so it **should be fast**, at least as fast as popular Smalltalk implementations. Since we want to keep complexity to a minimum, this means concentrating on optimizations that yield the biggest improvements relative to the added complexity. Tactics:

-   **Accept some complexity if the speed reward is big enough.** We want to keep the entire system considerably simpler than systems of comparable functionality and speed, but do not want it to be as slow as it would be if it was limited to the simplest possible implementation. Therefore, we try to use  optimization techniques that we suspect will yield the **biggest gains for the least complexity**. And once again, **measure the payoff** and back it out if the gains are not large relative to the complexity.
-   Push complexity **out of runtime into compile time**. An example of this is method lookup, where at runtime Mist’s worst case to find the method for a message send is a lookup in a single dictionary, as compared to Smalltalk’s traditional looking up the chain of superclasses, one dictionary lookup per class in that chain. This runtime cost lets traditional Smalltalk implementations update only a single entry in a single dictionary when one method is compiled. Mist may have to update a number of entries in a dictionary, or even more than one dictionary in some cases.

**Craziness**

Larry Page has said, “If you aren’t doing some things that are crazy, you’re doing the wrong things.” We interpret this as meaning that if no one thinks anything you’re doing is crazy, then you aren’t being innovative enough. However, if you get so crazy that _everyone_ thinks _everything_ you’re doing is crazy, you’re probably too far from the current state of the art to succeed. Tactics:

-   **Do the unconventional thing first**. In selected areas of the design (_not_ everywhere), if we see an unconventional way to do something, and it looks like it might have some advantage over the conventional way, we’ll try the unconventional way. We won’t always know how that will turn out, but if it doesn’t work out well we can always retreat to the conventional path, since we have a very good idea of how well that works.

**General Notes**

Mist initially runs only on x86_64 Linux. Anything that looks platform-specific in this document probably is. The architecture of Mist includes the ability to support other platforms, but that will come later.

**Language Differences**

Mist is mostly Smalltalk. Right now, this document is almost completely about implementation, so I'm not going to go into detail about the differences at the language level, except as they affect implementation. But there are a few things that affect the implementation:

-   Single-threaded. Mist (initially) has no concurrency mechanism. There will be something later, likely a variant of the Actor model based on asynchronous message sending (see, for instance, E or Erlang).
-   Super/self. Mist has no super send, but there are somewhat similar special rules for self sends. In some cases, this allows self sends to be early-bound – that is, the invoked method can be determined at compile time.

**Deployment Models**

Mist runs directly on the X86_64 architecture, without a virtual machine. Thus, the traditional Smalltalk model of an executable VM file plus a data file containing the image of all objects in the system does not apply to Mist. Mist aims to support a variety of different deployment models:

-   Standalone executable image. This is the most basic deployment model, and first to be implemented. The image is a single file, containing all of the system's objects, that is also executable directly by the host operating system. Once the OS kernel loads a Mist image and starts it running, Mist can be largely self-sufficient. The core functionality of Mist makes calls into the OS kernel, but loads no libraries into its memory space, not even libc.
-   Executable image with libraries. This works similarly to the standalone image, but it does make use of userspace libraries, which may be written in another language. This requires that Mist be able to load libraries, call functions in those libraries, handle data defined by the libraries, and be called back from libraries. 
-   Mist as a library. A Mist program defines some entry points, and is compiled as a .so shared object file. There is no “main” here, there are only entry points that can be called from a program written in any language.
-   Mist scripts. A Mist executable image is created that can be the interpreter for a #! script. The script contains Mist source code which is compiled and executed by the Mist image.

The executable image is the first model to be implemented, and the one covered in the most detail in this document.

**Image Format**

A Mist executable image is (for X86_64 Linux) a file in legal ELF64 format. The ELF header specifies that the entire file be loaded into memory at a specific address, and that control then be transferred to the machine code of an initialization method (see the _Image Startup_ section). Every byte of the entire file is part of some object. The first object in the file is an instance of ElfHeader. Instances of ElfHeader have an atypical memory layout that allows the object pointer to an instance to point to an address in the middle of the object's allocated memory. The beginning of the object contains byte-indexed instance variables, allowing the object to construct a valid ELF header without restriction.

**Methods**

In a traditional Smalltalk implementation, most of the code of the system exists as bytecodes in instances of CompiledMethod. The virtual machine provides the rest of the code: 

-   A bytecode interpreter implementing language-level features like assigning to variables, sending messages, and returning from methods
-   Memory management code, including object allocation and garbage collection
-   Implementations of the primitive methods

In Mist, there is no VM. All of the code in a standalone image resides in Mist methods. A method retains its information in several forms, including its source code string, its intermediate code (Fog) tree, and its machine code. All methods have all of these forms at all times. In particular, all methods always have their native machine code – machine code is not just a “cache” containing the machine code for the most recently-executed methods, as it is in some Smalltalk implementations. Since all methods are compiled, Mist does not use the term CompiledMethod – they're just Methods. Perhaps Smalltalk uses the term CompiledMethod to emphasize that they do not contain the source code.

Mist methods are first compiled from source code to an intermediate code known as Fog. The Fog code is then compiled to machine code, which can then be run directly by the machine's CPU. Fog is a tree-structured low-level architecture-independent code form. At least it's intended to be architecture-independent; at this writing it's only been implemented for x86_64, but it should be reasonably compatible with other major CPU architectures such as ARM or SPARC. It's certainly not going to be compatible with every imaginable architecture. Most compiler optimizations are done by manipulation of the Fog tree before translation to machine code, so much of the code that does this should be reusable with other architectures.

**Primitive Methods**

Mist, like Smalltalk, only supports a very few basic operations at the language level. All the language lets you do is:

-   Specify a limited set of objects literally (numbers, strings, symbols, arrays, blocks...)
-   Reference a few special constant objects (nil, true, false)
-   Reference the implicit temporary variable “self”
-   Declare formal parameters and temporary variables
-   Reference variables (parameters, temporaries, instance, shared)
-   Assign to assignable variables
-   Send messages
-   Return from methods

This list is, I believe, exhaustive, since almost everything is done by message sending, including control structures such as conditionals and loops.

You can't actually get much done with only these operations. Thus, Mist, like Smalltalk, relies on the presence of a set of _primitive methods_ that perform other necessary low-level operations, such as adding two integers together, or making a system call to the OS kernel. 

In Smalltalk, primitive methods are traditionally implemented within the virtual machine, and are written in the language that the VM is written in, which is usually C. In Mist, primitives are written directly in Fog, and are then compiled to machine code using the same Fog-to-machine-code compiler that is used for Mist methods. To make this possible, Fog defines node types (such as intAdd, syscall, or if-then-else) that cannot be generated directly from Mist source code. Primitives make use of such low-level nodes.

Fog does not have a syntax. Primitives are assembled by writing Mist code that builds a Fog tree at compile time, and then passes this tree to the back end of the compiler. This is a somewhat awkward way to write primitives, but the intent is that there be relatively few primitives in a Mist system (quite a bit fewer than in most Smalltalks) and that each be very low-level and thus quite small.

In Mist, there is no notion equivalent to the “primitive failure” of Smalltalk-80 and most Smalltalk implementations since. A primitive is just a method that happens to contain some Fog nodes that can't be generated from Mist source code. If a primitive detects a condition that it cannot handle, it can send messages just as easily as any other method can send messages, so no special mechanism is required.

>>><insert example primitive here>

**Memory Management**

Details of Mist's memory management – object allocation and garbage collection – can be gleaned from the slides and video of the Mist talk at the November 2012 Smalltalks conference, which are available at the mist-project.org website. For now, this document only has the highlights.

Mist's garbage collector is a non-compacting multi-pool allocator. Once an object is allocated, its address stays the same for the life of the process. This makes quite a few things quite a bit simpler to implement. To limit memory fragmentation, objects are only allocated on the heap in a limited number of physical sizes – 64, 128, 256, 512, 1024, 2048, 4096, and 8192 bytes. A free list for each size of object is maintained. Objects that require more than 8192 bytes are mmaped directly from the kernel, and munmapped upon garbage collection. Large objects are expected to be relatively rare.

Object allocation and garbage collection are handled by sending messages to the objects involved. When an object realizes that it is garbage, its response is to change its class to the FreeSpace class for its physical size and send a message to the memory manager singleton asking to be linked into the appropriate free list. Thus, a garbage-collected object does not cease being an object, it just becomes a free object awaiting re-allocation. FreeSpace instances respond to messages sensibly, having behavior appropriate for free objects. This behavior is pretty much limited to participating in being linked to and unlinked from free lists, strategically ignoring requests to participate in garbage collection, splitting themselves into two smaller FreeSpace instances, and changing their class to a non-free-space class during object allocation. Free objects don't really need to do much more than that. 

**No-Virtual-Machine Gotchas**

Mist does not run on a software virtual machine, but on a hardware machine (initially, just X86_64). The only external software requirement is an operating system (initially, the Linux kernel), though some applications will want to load external user-space libraries. Mist has some primitive methods, but they are small and simple. Everything else is written in Mist. This conceptual self-recursion can, if care is not taken, lead to actual runtime infinite recursion. For instance:

-   Method lookup is done by executing Mist code. Therefore, to avoid infinite recursion, the Mist code that is used to look up methods in dictionaries must _not_ do any method lookups, even though it does (of course) send messages. See the Method Lookup section.
-   Blocks are Mist’s only mechanism for control structures such as conditionals and loops. In the general case, block execution requires allocation of a closure object at runtime of the method that contains the block literal. Object allocation is done by Mist code, which must contain blocks in order to evaluate conditional expressions. To avoid infinite recursion, the Mist code for object allocation must not itself require any object allocation to execute. Therefore, some way must be provided for the object allocation code to execute without creating closure instances. See the section on Blocks.

These requirements can get a little tricky, but there are solutions. See the sections referenced above.

**Blocks**

Starting with Smalltalk-80, messages to code _blocks_ have been used to implement the flow-of-control operations that in most other languages are part of the language itself. Mist follows in this tradition. In Smalltalk-80, blocks were not closures. Most modern Smalltalk implementations implement blocks as closures, as does Mist. A _closure_, in general, is a function that captures the variables in its environment. To see what that means in the context of Mist, let’s look at an example.

Block example 1

gcMark

   isGcMarked ifFalse: [isGcMarked := true.

                        self allReferencesDo: [:each | each gcMark]].

This, incidentally, is the heart of the “mark” part of a mark-sweep garbage collector such as Mist’s. The method sends one message, #ifFalse:, to an instance variable of the receiver, isGcMarked. The argument to the message is given as a _block constructor_, some code delimited with square brackets. The block constructor specifies what kind of block (a closure instance) should actually be passed to the message send at runtime. This particular block constructor contains another block constructor. The inner block takes a single argument, each, and sends that argument one message, gcMark. It does not make reference to any other variables. The outer block, however, does reference variables from the environment in which it is defined. It references self, and an instance variable of self, isGcMarked. In general, code within a block constructor may refer to any of the following variables and constants in its environment:

-   Its own formal parameters
-   Its own declared temporary variables
-   Formal parameters and temporary variables declared in any block constructor in which it appears. Block constructors may contain block constructors, to an arbitrarily deep level. Each block constructor has access to variables from any level which is “outer” relative to them.
-   Formal parameters and temporary variables declared in the method in which it appears. 
-   The pseudo-variable self, indicating the receiver of the message which caused the invocation of the method in which the block constructor appears.
-   Any instance variable of self.
-   Any module constant of the module in which the method’s class is defined.

A reference to a variable var may consist of an assignment to another variable (a := var), use as a receiver of a message (var size), use as an argument to a message (a includes: var). If var is an instance variable or declared temporary variable, it is also legal to assign to it (var := a). It is not legal to assign to module constants, formal parameters, or self.

Since a block constructor specifies the creation of a block at runtime, all of these variables in its environment must be available to a block. To complicate matters, the variables in the environment must remain available after the enclosing method activation has returned.

Block example 2

newCounter

  | count |

  count := 0.

  ^[count := count + 1].

When you send #newCounter, what is answered is a block which closes over the variable count, with the value of count initialized to zero. Subsequently, each time you send #value to this block, it increments count and answers the incremented value. Each time you send #newCounter, you get back a new block with its own value of count. The variable count, although a temporary variable of #newCounter, has its life extended through its use in a block closure. It must survive until the block is garbage collected.

Most Smalltalk implementations have special bytecodes and virtual machine support for implementing blocks. Mist does not have these options, so it relies on mechanisms it already has – classes, objects, and messages. And it turns out that these constructs are quite sufficient to do what must be done. The compiler must do some extra work to specify those objects and messages. In the general case, the compiler must do the following to compile each method or block constructor which contains a block constructor:

-   For each block constructor in a method (or in a block constructor), the compiler creates an anonymous block closure class, extending a base closure class with a single method which implements the code in the body of the block constructor.
-   The compiler also creates an anonymous shared variable class with an instance variable for self, and instance variables for each formal parameter or temporary variable declared in the method or block constructor.
-   The compiler inserts code at the beginning of the method or block to create an instance of the shared variable class and initialize it with the values of the formal parameters.
-   After that, the compiler inserts code to create an instance of each block closure class (one for each block constructor that is directly contained), initializing each with the shared variable instance. Since block constructors can be nested to an arbitrary level, and the lifetime of formal parameters and temporary variables varies with each level, the number of shared variable instances needed to initialize a block instance is equal to its nesting level – the more deeply it’s nested, the more shared variable objects it needs, since in the general case it needs one from each level of outer contexts.
-   In the code of the method or enclosing block, all references to temporary variables must be replaced with getter or setter message sends directed to the shared variable instance.
-   In the code of the enclosed block, all references to variables from the outer context must be replaced with getter or setter message sends directed to the shared variable instance.
-   In the code of the enclosed block, the pseudo-variable self must be compiled as the send of a getter message to the shared variable instance. The code within the block constructor has no way to reference the block instance itself, unless it is passed in as an argument when it is invoked.
-   In the code of the enclosed block, references to instance variables must be handled specially. It can get a reference to self from the shared variable instance, but to retrieve or modify instance variables of that object it must send getter and setter messages to that stored self. If the setter and getter methods do not exist, the compiler must create private ones for this use.

Block example 2, then, would be transformed by the compiler into something like this:

Methods in the <sharedVariableClass> for #newCounter. This class has at least two instance variables: slf and count. (self from the outer context is represented as slf since self is one of the few reserved words in Mist and therefore cannot be used to name an instance variable.)

slf

   ^slf.

  

slf: anObject

   slf := anObject.

   ^slf.

  

count

   ^count.

  

count: anObject

   count := anObject.

   ^count.

Methods in the <closureClass> for the block in #newCounter. This class has one instance variable, sharedContext.

sharedContext: anObject

   sharedContext := anObject.

  

value

   ^sharedContext count: sharedContext count + 1.

And #newCounter itself ends up looking something like this (though it never takes this form in source code – the transformation is done in Fog):

newCounter

   | sharedContext block1 |

   sharedContext := <sharedVariableClass> new.

   sharedContext slf: self.

   block1 := <closureClass> new.

   block1 sharedContext: sharedContext.

   sharedContext count: 0.

   ^block1.

The class references here are represented as references in angle brackets <>. Since these classes have no name, there is no way to directly refer to them in source code. The compiler inserts references to the class objects directly into the method as it compiles it.

Note that the block answered by newCounter responds to #value because its body (after transformation) is compiled into the closure class with #value as its selector, so normal message dispatch can be used. This contrasts with Smalltalk implementations which use a primitive to invoke a closure’s behavior.

**Block optimizations**

Blocks, as described above, are rather expensive. (And we haven’t even talked about non-local returns yet!) However, their use is mandatory in all control-flow constructs. Thus, blocks are very widely used. Various Smalltalk resources contain advice on how to avoid using blocks, or to use ones that are less expensive. Since one of the Mist project’s values is “speed,” it’s worth taking a look at what optimizations could be done to reduce the cost of block usage.

**Block elimination**

The most effective way to reduce the cost of a block might be to eliminate the block altogether, as long as what you replace it with is less costly. 

Most (if not all) Smalltalk implementations do static block elimination, eliminating some blocks at compile time when certain “restricted selectors” are used. For instance, #ifTrue: is typically restricted, so when the compiler is presented with code like this:

   isActive ifTrue: [self deactivate].

it does not compile a block, but assumes that it knows what #ifTrue: means and compiles the body of the block in-line with the containing block or method, conditionally executed using whatever low-level conditional mechanism the VM provides (typically a conditional jump bytecode).

This approach has several drawbacks

-   If #ifTrue: is sent to some object other than true or false, the response is typically not the doesNotUnderstand: one would expect, but some other error such as mustBeBoolean.
-   Worse, if you define #ifTrue: in some other class, this definition may be ignored, and you find that sending #ifTrue: to an instance results in a mustBeBoolean error instead of invocation of your #ifTrue: method. Even though the circumstances where it would be good programming practice to implement your own #ifTrue: method are vanishingly small, this exception to the normal language rules can be confusing, and clearly conflicts with the Mist project value of consistency.
-   More pragmatically, this approach does not apply to all of the cases in which blocks can be eliminated. It only works for specific selectors that the compiler has been designed to optimize. It provides no help at all for user-defined selectors. For instance, if you were to add a new “short circuit” logical operation:
-   nand: aBlock
-     ^(self and: aBlock) not.
-   The compiler would not be able to eliminate the block, since the block is not a block constructor, but a block passed in as an argument.

Because there are both philosophical and pragmatic drawbacks to this scheme, and because we have a better scheme in mind, Mist will not initially use static block elimination. Mist will be able to eliminate blocks in more situations through the use of dynamic inlining and subsequent code analysis. See the (alas, not yet written) major section on code optimizations.

**Blocks and the object allocation conundrum**

Even when you can’t eliminate blocks, there are techniques available to reduce the cost of some blocks. In keeping with Mist’s “do the simple thing first” strategy, we normally wouldn’t be looking at ways of reducing the cost of non-eliminated blocks until we were able to measure the cost of the blocks that remained after block elimination. However, it turns out that block implementation poses a problem for Mist that requires some kind of solution in the first running version of Mist.

The heart of the problem is this: executing a method that contains blocks requires, as seen above, that objects be allocated by sending #new to a closure class and to a shared variable class. Object allocation, in the absence of a VM, is implemented in Mist code. Object allocation can, if no free objects are available, trigger garbage collection. The object allocation code and garbage collection code (collectively, memory management code) need to use flow-of-control constructs. Those constructs are implemented using blocks. And executing a method that contains blocks requires that objects be allocated. This is a recipe for infinite recursion. So _something_ must be done to break this cycle. One possibility is the...

**Clean block optimization**

One fairly obvious change that seems likely to improve the performance of the block implementation scheme shown above is to only share through a shared variable object those temporary variables that are actually _referenced_ within a block. Method temporaries that are only referenced by the method itself can remain ordinary temporaries. This optimization is fairly easy for the compiler to do, though it does make the compiler more complex. Since the compiler must examine each variable name used to determine if it is in scope, it can mark each variable that must be shared.

Of particular interest is the case where a block references _no_ variables that must be accessed through a shared variable object. This is the case if the block references only module constants, its own formal parameters, and the temporary variables declared within the block itself. Such blocks are sometimes called _clean blocks_. Any reference to self,  or a formal parameter or temporary variable from an outer context, disqualifies a block from being considered clean.

A block closure created from a clean block constructor has no shared state. Thus, all blocks created from a clean block constructor are equivalent. And if they’re all equivalent, we can get away with only having one instance of that closure class – a singleton. And instead of compiling into the outer context of the block a message send of #new to the closure class, we can compile in a reference to the singleton  block instance of that class.

This gives us a clue to a possible solution to the object allocation conundrum. If all memory management code was written to use only clean blocks, the “we’re trying to allocate an object while allocating an object” recursion could be avoided. 

However, the traditional flow-of-control messages like #ifTrue: take a nilary (zero-argument) block. With no arguments, such a block is very limited in what it can do. One solution to this is to implement a very small set of rather inelegant messages like #ifTrue:arg1:arg2:arg3:, which would be implemented in class True like this:

-   ifTrue: aBlock arg1: arg1 arg2: arg2 arg3: arg3
-     ^aBlock

      value: arg1

      value: arg2

      value: arg3.

And whose implementation in class False would be just:

-   ifTrue: aBlock arg1: arg1 arg2: arg2 arg3: arg3
-     ^nil.

This would work, although it’s a bit clunky. The above methods might be all that are required. We’d have to be careful to write all memory management code using only clean blocks.

We could also reconsider static inlining of blocks that are arguments to certain messages, such as #ifTrue:. The philosophical objections to this could be addressed by changing how the inlined code reacts to a non-Boolean receiver. Instead of a mustBeBoolean error, it could actually create a block and send #ifTrue: to the receiver with the block as the argument. This may be a better long-term solution, but I think I want to wait until dynamic inlining of blocks is implemented to see how static inlining might fit into that.

Therefore, the initial plan is to go with the clunky clean blocks in memory management code.

**Copying block optimization**

Some Smalltalks implement another block optimization, the _copying block_ optimization. (See [http://www.esug.org/data/Articles/misc/oopsla99-contexts.pdf](http://www.esug.org/data/Articles/misc/oopsla99-contexts.pdf) for one such implementation.) In this optimization, if the compiler can statically prove that a variable from an outer scope cannot be modified after the creation of a closure instance that references that variable, then the value of the variable can be copied into the closure at its creation. This avoids the creation and use of some shared variable objects. This optimization could be applied to Mist. However, it does not appear to be necessary at the outset, like the clean block optimization is. Also, eliminating blocks through dynamic inlining seems like it may be a bigger win, so I want to wait until that’s working, then look to see how many frequently-evaluated blocks remain that could benefit from a copying block optimization.

**Compiler phases**

  

In Smalltalk, the unit of compilation is the method. In Mist, methods may be individually compiled during interactive development, but because there can be interdependencies between the classes and methods in a module the unit of compilation is more accurately described as the module. However, all code resides in methods which are independent of each other in much the same way as Smalltalk's methods, but which are compiled in the context of the Module and its imports.

The code of a method can exist in four forms:

-   Source code  
    Human-readable (and writeable) linear text
-   Macro Fog  
    An abstract syntax tree whose nodes are Fog macros.
-   Micro Fog  
    A tree of lower-level nodes resulting from expansion of the macro nodes; more or less an instruction-set-architecture-independent structured assembler
-   Machine code  
    Machine-executable linear binary code

Compilation thus has three primary phases, the front end, the middle end, and the back end, each taking one form as input and producing output of the next form. Each phase is pluggable.

**Front end – source to Macro Fog (and back again)**

Parses the source code into a tree, then does semantic analysis on the tree to verify such things as variable references and compile-time self sends. The output contains enough information to allow a pretty-printer to produce code that resembles the original input reformatted. Thus, this phase is more or less reversible. 

More than one source code grammar is possible. Each grammar is used by a parser / pretty-printer pair. For instance, one programmer may choose to code in a Smalltalk-like syntax, and another programmer may choose to later edit that same code in a Java-like syntax. The latter programmer would view the code as pretty-printed by the Java-like grammar, and use that grammar to compile the code once edited.

Besides the grammar being pluggable, the pretty-printers have pluggable formatting rules – different programmers might choose to view and edit the same code using the same grammar but with different formatting rules.

One of the things that Macro Fog must preserve is code comments, so that source pretty-printed from the Macro Fog includes comments placed more or less where the programmer intended them to be. Mist grammars cannot get by with the typical trick of discarding comments during lexing. Newspeak does something similar – allowing a comment to precede any expression, and associating the comment with that expression. Mist may adopt this practice.

Passes in the front end:

The first pass parses the source code of a method into a the basic form of a Macro Fog tree. At the end of the parse, the method is known to be syntactically valid. However, although each variable reference is known to be a valid identifier, it is not yet known whether each identifier is a legal one to use in that context. Node types created are:

-   Method node – the root node of the method's compilation. This node specifies the selector of the method, and declares the names of its formal parameters. It has one child, and returns the result of evaluating that child as the method's return value.
-   Variable scope node – defines zero or more temporary variable names, whose scope is the single child of the variable scope node. The result of evaluating this node is the result of its child.
-   Sequence node – defines one or more children, and evaluates them in order. The result of the sequence node is the result of the last child.
-   Variable reference node – indicates a mention of a variable. Initially contains just a variable name, but in the second pass acquires a reference to an object representing the variable itself. That object is the same object for all references. Has no children. Its value is the value of the variable. 
-   Assignment node – writes a value to a variable. It has two children. The first child is a variable reference node indicating the variable to be assigned to. The second child may be any Macro Fog node except a Method node, and the result of evaluating that child is assigned to the variable.
-   Message send node – causes a message to be sent. Each message send node internally contains its selector. It has n + 1 child nodes, where n is the arity of its selector. The result of the first child node is the receiver of the message, and the results of the other child nodes are the arguments to the message send. The result of the message send node is the value returned from the message send.
-   Block node – defines a block contained within the method. This node specifies the arity and formal parameter names of the block. The result of this node is a block instance whose value (or value: or value:value:, etc.) method is defined by the node's single child.
-   Non-local return node – represents an explicit return from within a block. This node has one child, and returns the result of that 

The second pass checks each declaration or use of a variable name for validity, and binds each to an object representing the variable. Within a method, there are five name spaces in which an identifier can be bound:

-   Global namespace – an immutable namespace containing only the bindings for 'nil', 'true', and 'false'. These names are available to all methods and blocks in Mist, no matter where they appear. These names represent constants and may not be the target of assignment.
-   Module namespace – the identifiers defined by the module into which the method is being compiled. These are the names of classes and modules. They will include at least the names of classes defined in the module itself, along with the module's own name. The available names may also include names of other modules and/or names of classes defined in other modules, if the current module's definition specifies that those be included. These names are available to any method compiled within the module, and to any block within such a method. By convention, these names start with an upper-case letter. Names found here may not be the target of assignment.
-   Instance variables – The instance variables of the class that the method is being compiled into form a namespace. By convention these names begin with a lower-case letter. In general, instance variables may be assigned to. Note that instances of a class will usually contain instance variables that cannot be accessed from every method; a method may only access instance variables defined in its own class, not instance variables gained by composition with other classes. These names are available to any method compiled in this class, and to any blocks within such a method.
-   Formal parameters – The parameter names given in the method pattern, plus 'self'. These names are available only in this method and any blocks within it, and may not be assigned to. These names, by convention, begin with a lower-case letter.
-   Temporary variables – The temporary variable names declared between vertical bars at the beginning of the method. These names are available only in this method and any blocks within it. These names, by convention, begin with a lower-case letter. They may be assigned to.

Names in these five namespaces must be distinct. No name may appear in more than one of these namespaces. Mist does not support overriding of names from an outer scope by names from an inner scope; all scopes are equal here.

Code in blocks within a method has access to all of the names that code in the method may access, plus:

-   The block's own formal parameters, which may not be assigned to.
-   The block's own temporary variables, which may be assigned to
-   Formal parameters and temporary variables from the block's enclosing block, if any

  

**Middle end – Macro Fog to Micro Fog**

The middle end is a process of macro expansion, expanding each Macro Fog node to zero or more Micro Fog nodes. Comment nodes are an example of macro nodes that expand to zero micro nodes. Since this compilation step is not expected to be reversible, the comments are no longer needed after this step.

The macro expansions are pluggable by specifying an implementation strategy object. For instance, a macro node for “instance variable assign” would expand to a single memory write if using the initial Mist mark-sweep garbage-collector, but would expand to also include a reference count update if using a reference-counting collector. Similarly, the macro expansion for “send a message” includes the code for method lookup and in-line caching if it is used by the implementation strategy.

**Back end – Micro Fog to machine code**

Micro Fog nodes are simple operations that typically take one or two instructions on popular CISC architectures, and simple control structures such as if-then-else, basic loops, and call/return. This phase is pluggable by the instruction set architecture of the target processor.

Vocabularies

Each code form has some vocabulary in which it is expressed.

Source code's vocabulary is linear text, though the content of that text will vary based on the grammer being used.

Macro Fog's vocabulary is chosen to represent operations with semantic meaning in Mist. Two methods that do the same thing using the same algorithm will tend to have very similar Macro Fog representations, regardless of the syntax, implementation strategy, or target instruction set architecture.

Micro Fog has a vocabulary of simple operations and control structures. The content will vary based on implementation strategy, but the vocabulary is the same regardless of implementation strategy or instruction set architecture.

Machine code's vocabulary is dictated by the instruction set architecture being used.

  

Storage

During compilation, all four code forms must exist at one time or another. What code forms does a method object keep for its entire lifetime? The machine code must be kept, for without that the method is not executable. For debugging and possible recompilation, we'd like to keep something source-like. Since Macro Fog can be used to generate pretty-printed source code (as long as the grammar is also around), keeping the Macro Fog is sufficient. Micro Fog can be derived from the Macro Fog, as long as the implementation strategy is available, so keeping the Micro Fog is not necessary. 

Mist methods contain references to the Macro Fog of the method, the implementation strategy, the default grammar, and the machine code (which knows its instruction set architecture).

  

Optimizations

In the future, various optimizations are planned. Most of these are applied to one of the two tree-structured forms, Macro Fog or Micro Fog. Thus, some optimizations will be applied after the front end, others after the middle end. At this point, we don't anticipate adding optimizations after the back end, but if it makes sense that is possible as well.

  

  

  

The Mist compiler, like most, has a front end and a back end. The front end translates the Mist source code of a method (in the context of the complete system) into intermediate code (Fog). The back end translates Fog into machine code. Fog is (at least largely) architecture-independent, so the front end should also be mostly architecture-independent. The back end is, of course, somewhat specific to the X86_64 instruction set architecture.

Mist primitives are written directly in Fog, by writing Mist code that assembles the Fog tree.

Various optimizations may be added to the compiler in the future. Some of these may manipulate Fog between the front end and the back end (forming what is sometimes called a “middle end”). Initially, in keeping with Mist’s “do the simple thing first” philosophy, only the optimizations actually needed to make things work will be included. But the first version may look something like this:

**Front end**

-   Parse the source into an AST. This pass just checks for legal syntax. The AST is essentially Fog, but not yet in a form that can be accepted by the back end. Each remaining pass transforms the Fog tree into something more closely resembling the final machine code.
-   Replace textual names with references to actual variables (each variable is represented by an object). This pass checks that every variable name references a legally reachable variable.
-   Process block literals. This might actually take more than one pass. A class needs to be created for each block literal, and a method compiled into that class, and you might need a shared variable class for the method and/or blocks, and accessor methods for those, and possibly private accessors on the method’s class so the block(s) can access instance variables...
-   Early-bind self sends. In Mist, syntactic self-sends (those with a receiver that is the pseudo-variable self) of a selector that is implemented in the class of a method can be early-bound, that is, compiled as a simple call rather than as a method lookup with in-line cache. This pass marks each qualifying self-send node as early-bound, and adds a reference to the method it is bound to. During a module load, this pass is done across all methods in the module after the previous passes are complete for all methods, so that all possible target methods exist before the pass starts.

**Back end**

-   Expand Fog macro nodes. The main anticipated use of macro nodes is for message sends. Each message send node expands into a tree of roughly a dozen lower-level Fog nodes. These nodes remember which macro they came from, largely to help in debugging.
-   Traverse the Fog tree from right to left, assigning locations to temporary and implicit variables. Locations for these variables can be registers or stack frame locations. Instance variables are always in memory, within the receiver object.
-   Traverse the Fog tree from left to right, emitting machine code. This, finally, translates the tree structure of Fog into the linear structure of machine code.
-   Populate each inline cache with a reasonable guess as to which receiver class and method will be used at that send site. If this method is being compiled in isolation, this will be done immediately. If a module is being loaded, this step is done in a final pass across all methods in the module, at which time the machine code address of all methods in the module is known.

**Method lookup**

Whenever a message is sent, the system must determine which method to execute. As in Smalltalk, which method to execute is a function of the class of the receiver and the message’s selector. At each _send site_ (place in code where a message is sent) the selector is known, since it is a constant. (I’m ignoring #perform: here, but it’s a much less common case that can be handled differently.) The class of the receiver, on the other hand, cannot usually be determined at compile time. Note that “compile time” for a dynamically-changeable language like Mist includes the addition, modification, or removal of any method in the system.

In Mist, syntactic self sends for which a method with that selector exists in the defining class always invoke that method, ignoring the class of the receiver. In this case, the receiver is early-bound, compiled to a simple call of the target method, with no lookup at runtime required. This places a _dependency_ on the target method. For instance, if the target method is recompiled, the address of its machine code may change, and the machine code of the call at the send site will have to change to match. Or if the target method is deleted, the simple call will have to be converted to a method lookup.

For send sites that cannot determine the target method at compile time, we rely on the selector, since it _is_ known at compile time. In Mist, a Selector is not just a Symbol, but a first-class object with quite a bit of associated information. One of the things that a Selector has is a method dictionary. This is a variant of an identity dictionary, where each key is a class and each value is the address of the machine code of the method to invoke if an instance of that class is sent a message with the method dictionary’s selector. Each send site has a reference to the appropriate method dictionary compiled into it. At runtime, it sends a lookup message to the dictionary, which answers the address to call. So message sending works.

But _wait_, you might say! If you’re sending a message to a method dictionary every time you need to send a message, then how do you send the message to the method dictionary itself? Well, _that_ message send is hard-coded at the send site. But the lookup code of the method dictionary is itself written in Mist, so it must also send messages in order to do its job. 

In order to avoid infinite recursion, we must be careful to write all message sends that can be executed during the dictionary lookup in such a way that they are early-bound self sends, or that their receivers are SmallIntegers. The reason why SmallIntegers work is covered in the section on inline caching.

**Inline caching**

Mist uses inline caching. Inline caching is a long-known technique (see “Efficient Implementation of the Smalltalk-80 System” L. Peter Deutsch and Allan M. Schiffman, POPL 1984) for avoiding method lookup on every method send. The technique is based on the observation that 90%-95% of send sites in Smalltalk _always_ have the same receiver class. It is hard, however, to statically predict which sites must actually handle multiple receiver classes. The idea, therefore, is to cache the result of the most recent method lookup, along with the receiver class for which it is known to be correct. In Smalltalk implementations that use this technique, at each send site native code is compiled that is more or less the logical equivalent of this Smalltalk pseudocode:

actualReceiverClass := receiver class.

expectedReceiverClass == actualRecieverClass 

  ifFalse: [expectedReceiverClass := actualReceiverClass.

            methodAddress := expectedReceiverClass 

                              methodAddressFor: selector].

methodAddress call.

In this fragment, selector is a compile-time constant. The variables expectedReceiverClass and methodAddress are traditionally stored directly into the machine code as immediate values in a compare instruction (expectedReceiverClass) and the call instruction (methodAddress). Due to the cache-invalidation penalties paid for self-modifying code in modern x86 processors, this may no longer be the highest-performing place to store these values. In Mist, they could be stored either in the code as is traditional, or alternately in instance variables of the method, and the address of those instvars compiled into the machine code.

There’s one other wrinkle in Mist’s implementation of inline caching – the handling of SmallInteger receivers. For all other classes, the receiver’s behavior token (usually the class, but could be any other object, as long as it uniquely identifies the behavior of the receiver) is obtained by fetching the 64-bit word that is pointed to by the object pointer. SmallIntegers, however, encode both their class and state into the object pointer itself, which is then not a memory address. So before we attempt to fetch the behavior token of an object we must first check to see whether it’s a SmallInteger. Since we must determine this, we go ahead and cache the method address for SmallInteger receivers as well as a method address for non-SmallInteger receivers. The SmallInteger method’s value can be determined at compile time. Recall that “compile time” includes the addition, removal, or modification of any method in the system. Adding, removing, or modifying a method in SmallInteger requires updating all send sites of that selector in the entire system to update this cache. Changes to SmallInteger methods are rare, so for most purposes the cached SmallInteger method address is static. 

**Messages that are not understood**

As in Smalltalk, a Mist message that is sent to an object that does not understand that message (because the selector of the message is not implemented in the class of the receiver) is made the argument of a #doesNotUnderstand message, which is then sent to the receiver of the original message. All objects must understand #doesNotUnderstand, which sounds a bit like an oxymoron.

Mist’s message-sending machinery expects to find a method in a dictionary and execute it. Rather than have an exceptional path for what happens if no such method exists, we make sure that an appropriate method is always found. Recall that a message send is implemented by a method dictionary answering the address to call, and that there is a method dictionary for each selector. Each dictionary has a default answer which is answered if the lookup key is a class in which there is no method with that selector. That default answer is the address of a special method that handles not-understood messages that use that selector. There is one of these methods for each selector sent in normal code anywhere in the system. (#perform:, as usual, is a special case that is handled altogether differently.) The method for #at:put: would look something like this:

-   at: offset put: value
-     ^self doesNotUnderstand: 
-       (Message selector: #at:put: arguments: {offset. Value}).

Because this method is invoked as if it was defined in the class of the receiver, self is bound to the object that received the original message.

There’s also the question of what to do if the receiver does not understand #doesNotUnderstand:. According to the language definition, it is an error condition to not understand #doesNotUnderstand:, so the method dictionary for #doesNotUnderstand: has a special method for its default answer. This method signals an exception, which is handled (or not) by the normal mechanisms.

**Private methods**

Mist allows methods to be declared private to the class in which the method is defined. Such methods can only be invoked through a _self-send_ (a send to the pseudo-variable self) within a method defined within that class. Note that a send that is syntactically sent to another variable does not count as a self-send for this purpose, even if that variable happens to refer to the receiver. 

When composing classes into a new class, a method defined in one of the component classes (but not declared private to its defining class) may also be declared to be private to that composite class. It is then only invocable through a self-send in any method in the composite class or any of its component classes.

Like other self-sends in Mist where the selector is locally defined, private self-sends can be (and are) bound to a method at compile time, so no method lookup or inline caching needs to be compiled into the send site. Since the address of the method is known at compile time, the message send can be compiled more simply as a procedure call.

**Teams**

>>>Needs to be written

**Fog**

Fog is the intermediate representation of the Mist compiler, and is also the form in which Mist primitive methods are written. Fog is an object structure, a tree of nodes.  Mist primitive methods are written by writing Mist code that assembles a Fog tree for that method. Non-primitive methods are written in textual Mist, which the Mist compiler front-end parses and compiles into Macro Fog. Macro Fog is macro-expanded into Micro Fog. The back end of the Mist compiler then compiles the Micro Fog tree into an executable machine code representation of the method.

Macro Fog represents the language semantics fairly directly, with such nodes as “assign instance variable” or “send message.” Micro Fog is fairly low-level. It is intended to be a structured  assembler, general enough to be independent of processor instruction architecture, at least for major architectures such as X64 and ARM.

Typing in Fog

As of this writing, Fog is untyped. From a data-typing standpoint, Fog is equivalent to assembly language. I anticipate that Micro Fog will need to have some static typing. Not very many static types, though – int, double, and object pointer may be sufficient. The distinction between floating-point and integer primitive types is needed to comply with the ABI for passing arguments in float or general-purpose registers, and the distinction between object pointers and raw ints seems useful for low-level optimizations.

Operation nodes

The simplest type of Fog nodes are operation nodes. An operation node has some number of child nodes (two children is typical) and take an input from each child, then perform some operation dependent on the specific node type, then pass the result of that to its parent node. For example, an integer add node has two children. It evaluates its left subtree, then its right subtree, each of which produces a 64-bit integer value. It then adds the two integers and passes the sum to its parent as a 64-bit integer.

Constant nodes

Each constant node has no children, and passes a constant result to its parent. What constant is passed is specified at the time the node is created. This constant may be a raw int, or an object pointer. When Fog becomes typed, there will likely be two different classes of constant node for this. Constant nodes are used to implement Mist literals.

Sequence nodes

A sequence node has one or more children (the number fixed at the time the node is created during compilation). It evaluates its children in order. Its runtime value is the value of its last child.

Variable scope nodes

Each variable scope node declares one or more temporary variables, and has one child. It evaluates its child. The variables declared by the node are in scope during evaluation of its child. The runtime value of a variable scope node is the value of its child.

Variable fetch and store nodes

Temporary variables are accessed through variable fetch and store nodes. Each variable fetch or store node refers to one temporary variable specified at the time the node is created. A variable fetch node has no children. Its runtime value is the current value of the variable to which it refers. A variable store node has one child, and stores the runtime value of that child into its variable. Its runtime value is the runtime value of its child.

Byte and word fetch and store nodes

Instance variables and other data stored in memory are accessed through byte and word fetch and store nodes. A fetch node has two children which provide a base address and an offset. Its runtime value is the byte or word fetched from memory.  A store node has three children – a base address, an offset, and the value to be stored. Its runtime value is the value stored.

Conditional nodes

If-then-else constructs are implemented with conditional nodes. Each conditional node has three children. The first child is evaluated, and is expected to leave a condition code in the CPU's condition code register. If the condition specified in the conditional node at the time of its creation is met at runtime, the second child is evaluated, otherwise the third child is evaluated. The runtime value of the conditional node is the value of whichever of the second or third child that is evaluated.

Loop and loop exit nodes

A loop node has one child, which it evaluates repeatedly, forming an infinite loop. Unless infinite execution is actually desired, a loop exit node appears somewhere in the subtree rooted by the loop node. This loop exit node refers to the loop node. A loop exit node has one child. It evaluates that child, then exits the loop. The runtime value of the loop exit node's child becomes the runtime value of th e loop node. Note that the reference from a loop exit node to its loop node is not part of the node tree – the loop node is not a child of the loop exit node, even though the loop exit node has a reference to the loop node. 

Syscall nodes

A syscall node makes a system call to the Linux kernel. It has one to seven children. The first child provides the call number, and the remainder provide the arguments to the call. The result of the kernel call is the runtime result of the syscall node.

Method nodes

A method node is the root node of a method. The method node declares how many arguments the method takes. It has one child, which it evaluates when the method is invoked. The return value of the method is the runtime value of the method node's child.

Macro Fog

Macro nodes

A Fog macro node is an architecture-independent node that is expanded in a possibly architecture-dependent way to a tree of lower-level Fog nodes. These are used for frequently-used complex structures, such as message sends.

Message send macro node

The sending of messages is compiled by the Mist compiler's front end into a message send macro node. The selector of the message is compiled into the node at the time of its creation. This node has one or more children, which are evaluated in order. The first child's runtime value becomes the receiver of the message. The remaining children's runtime values become the arguments to the message. Once all children are evaluated, the message is sent, and the value returned by the message send is the runtime value of the message send node.

In the initial x64 implementation of Mist, the message send macro node is expanded into a number of lower-level nodes. Here’s the tree of nodes, represented as in indented list. When reading, recall that Fog is executed depth-first, so the first thing executed here is “receiver subtree.”

  

*** This tree needs updating to current design, which is less optimized but requires fewer special Fog node classes. ***

sequence (special?)

if c

sequence

marshal arguments (via required locations)

receiver subtree

arg1 subtree

…

argn subtree

rdiIsSmallInt

call expectedSmallInt method

temp var scope (receiverBehavior)

if =

cmp

store temp receiverBehavior

fetch indirect rdi

constant expectedBehavior

call address

constant expectedNonSmallInt method

sequence

push N arguments

call indirect

sequence

callWithArguments \ cacheMissExpectedBehaviorAt:actualBehavior:methodAt:andAt:

constant <selector-specific dictionary>

address of constant (expectedBehavior)

fetch temp receiverBehavior

address of constant (expectedNonSmallInt method)

address of constant (lookedUpMethod)

pop N arguments

call address

constant lookedUpMethod

  

One key to understanding how this works is to realize that #cacheMissExpectedBehaviorAt:actualBehavior:methodAt:andAt: both looks up the method to be executed and patches the three constants in the send site to form a (monomorphic) inline cache. It is, of course, vital that the cache miss lookup and patching code use only self sends or sends to SmallIntegers, or otherwise ensure that there is never a cache miss during cache miss handling.

**Message send generated code**

When Fog generates a message send, this is an approximate disassembly of the generated machine code. The code shown here is actually a bit more optimized than Fog will probably be able to produce from the nodes above.

  <move arguments to registers and stack>

  bt   rdi, 0

  jnc  NotSmallInt

  call <Constant, offset to method>

  jmp Continue

NotSmallInt

  mov r11, [rdi]

  mov rax, <Constant, address of expected class>

  cmp rax, r11

  jnz CacheMiss

  call <Constant, offset to method>

  jmp Continue

CacheMiss

  <push message send receiver and register arguments>

  mov rdi, <constant address of selector-specific 

            method dictionary>

  mov rsi, rip

  lea rsi, [rsi + rsi + (1 – 2n)] 

    <1-2n constant to reach constant above, plus one for smallInt conversion>

  mov rdx, r11

  call <Constant, address of

        MethodDictionary>>cacheMissAt:actualBehavior:>

  <pop message send receiver and register arguments>

  call [rax + 0xnn] <offset to start of machine code within method>

Continue

  add rsp, 16r10

  ret

  

**Register Allocation**

  

Micro Fog-to-machine-code translation uses a (hopefully) simple algorithm. Here I'll explain the simple part, then go over the complicating factors.

Each edge in a Fog tree defines an _implicit variable_, a variable by which a child node transfers its result to its parent. There are also the explicitly-defined temporary variables, and the arguments to a method. Each set of variables must be assigned one or more locations where it can reside. A location is either a specific register or a specific memory address relative to the stack pointer.  Temporary variables are each assigned a single location for their entire lifetime. Method arguments occupy a specific register or stack location (defined by the ABI) upon entry to the method, but each is typically moved to another assigned location and kept there for the life of the method.

An implicit variable’s value is set by one node, and is consumed by that node’s parent. This defines the life of the variable. An implicit variable can have up to three distinct locations:

-   The _generation location_ is the location where the child node initially places the variable’s value.
-   The _resting location_ is the location where a variable’s value is held while its right-hand siblings execute. 
-   The _consumption location_ is the location through which the parent node accesses the value.

To minimize moves, it is desirable that all three locations be the same location. This is often not possible, so there may be two or three locations involved.

  

**Calling convention**

Mist does not use context objects as described in the Smalltalk blue book. Mist uses the hardware stack, as pointed to by the x86_64 rsp register. Mist does manage its own stack memory, rather than use the stack area that the OS gives it at launch. In Very Early Mist (that which exists today) the stack lives at the end of memory allocated at start. Later, Mist will most likely mmap an area of memory to use for the stack. 

Mist message sends, once the method to be invoked has been determined, are implemented as function calls. self is sent as an implicit first argument to the called method.

Mist conforms (mostly) to the x86_64 calling convention documented in the _System V Application Binary Interface AMD64 Architecture Processor Supplement_, available at 

[http://www.x86-64.org/documentation/abi.pdf](http://www.x86-64.org/documentation/abi.pdf)

This makes it easier to implement calls out to functions written in other languages, and calls in from other languages. Mist uses no frame pointer or base pointer, freeing rbx and rbp for general use. Currently, Mist treats all of the general-purpose registers as caller-saved. This is currently the only deviation from the ABI. Later, we may want to fully conform, or to deviate further. The current deviation does not affect call-out to external functions, but it does add some requirements to call-ins to Mist. General-purpose registers that the ABI specifies as callee-saved would have to be saved on initial entry into Mist from an external caller.

**Non-local returns**

When a message is sent, the evaluation of the method containing the message send is suspended and evaluation of another method is started. A normal, or _local_, return from a method continues the suspended evaluation of the method that sent the message that invoked the method that is returning, which is always the most-recently suspended evaluation. A _non-local return_ may return to a method activation that is deeper on the stack, without completing the intervening suspended method activations.

In Mist, a non-local return is specified by an explicit return in a block. When a non-local return is executed, any active unwind blocks (those that are arguments to ensure: or ifCurtailed:) must be evaluated, those closer to the top of the stack first. To implement this, some special stack handling is required.

For normal message sends and returns, Mist follows the ABI layout for the hardware stack. For non-local returns, additional stack-related information is needed. This information does not fit handily into the ABI format, and is also not needed for most stack frames. Mist puts this information into a separate _non-local-return stack_ (NLR stack). This is a singly-linked list of objects. A class instance variable in the class NlrStack points to the object that is the current top of stack, and each stack element points to the next deeper element. The order of NLR stack elements is consistent with the order of frames on the hardware stack, but since not every hardware stack frame needs extra information there are generally fewer elements in the NLR stack than there are frames on the hardware stack.

The elements of the NLR stack are of two classes of objects, ReturnPoints and UnwindBlocks.

A ReturnPoint contains an address within the hardware stack. To perform a non-local return, this value is stored in the hardware stack pointer, and the next normal return will return to that point on the stack. Details below.

An UnwindBlock specifies a block that must be evaluated during a non-local return that will discard its point on the stack. An UnwindBlock is put on the NLR stack during the evaluation of the specified code.

Implementation of #ensure: and #ifCurtailed: is fairly simple. Approximate implementations:

  ifCurtailed: terminationBlock

    | stackElement result |

    stackElement := UnwindBlock for: terminationBlock.

    NlrStack push: stackElement.

    result := self value.

    "We only get here if I returned normally"

    NlrStack pop: stackElement.

    ^ result.

  

  ensure: terminationBlock

    | result |

    result := self ifCurtailed: terminationBlock.

    "We only get here if I returned normally."

    terminationBlock value.

    ^ result.

Implementation of a method and a block containing a non-local return is more interesting. On entry to any method that contains a block with a non-local return, a ReturnPoint must be pushed on the NLR stack. That ReturnPoint must then be given to the block upon its creation, and the non-local return in the block translates to a message sent to the ReturnPoint. Consider this simple method:

  foo

    self isBar ifTrue: [^ 'yep'].

    self beBar.

    ^ 'converted'.

This would be translated by the compiler to the Fog equivalent of something like:

  foo

    | stackPointerValue returnPoint |

    stackPointerValue := Stack pointerValueLess: 3.

    returnPoint := ReturnPoint returningTo: stackPointerValue.

    self isBar ifTrue: [returnPoint return: 'yep'].

    self beBar.

    returnPoint invalidateAndPop.

    ^ 'converted'.

The #pointerValueLess: method is a primitive that answers the address of the stack pointer as it was upon entry to the calling method. It needs the constant argument (3 in this case, though that may well not be the true value for this example method) since the #foo method has decremented the stack pointer on entry to make room for some number of stack-based temporary variables, and #pointerValueLess: must correct for that, as well as for the procedure call to #pointerValueLess:. The compiler knows how many stack-based temporaries it has allocated space for, so it can use that value as the constant argument.

The #returningTo: message creates a ReturnPoint instance with the given value for the stack pointer, and pushes it onto the NLR stack. Sending #invalidateAndPop to the ReturnPoint lets it know that it should signal an error on any attempt to return through it, since the context it would return from is no longer valid, and pops it from the NLR stack.

The remaining piece of the non-local-return puzzle is the implementation of #return: in ReturnPoint. It might look something like this:

  return: anObject

    isValid 

      ifFalse: [self error]

      ifTrue: [NlrStack unwindTo: self.

               Stack setStackPointerTo: savedStackPointer 

                    andReturn: anObject].

    "The send above does not return to here."

 Here,  #setStackPointerTo:andReturn: is a primitive that sets the hardware stack pointer to the given integer, puts the given return value into the correct register for a returned value, and returns. This truncates the stack and actually accomplishes the non-local return.

Some more approximate code to help visualize how this all fits together...

In NlrStack class:

  unwindTo: anNlrStackElement

    topElement unwindTo: anNlrStackElement.

In ReturnPoint:

  unwindTo: anNlrStackElement

    self invalidateAndPop.

    ^anNlrStackElement == self

      ifFalse: [nextElement unwindTo: anNlrStackElement].

In UnwindBlock:

  unwindTo: anNlrStackElement

    self invalidateAndPop.

    terminationBlock value.

    ^anNlrStackElement == self

      ifFalse: [nextElement unwindTo: anNlrStackElement].

**Exceptions**

>>>Note: I think this section is _almost_ complete and _almost_ correct. Hopefully it is close enough to communicate the implementation strategy. Bug-finding is left as an exercise for the reader. :-)

Exceptions in Mist are intended to have the same semantics as ANSI Smalltalk exceptions, though minor deviations might become necessary due to differences between Mist and Smalltalk.

Most exception functionality is provided by the class ExceptionHandler. This class defines at least the following instance variables:

-   **exceptionSelector**  
    An exception class or exception set specifying which exceptions I am willing to handle.
-   **nextHandler**  
    When searching for an exception handler, the next handler to be searched. This is what ANSI Smalltalk calls the “handler environment.” Since handlers, in the general case, form a tree, there may be more than one handler with the same nextHandler.
-   **exception**  
    While handling an exception, this is the exception being handled.
-   **exceptionToResignal**  
    Nil, or a new exception to resignal.
-   **isOuter**  
    Boolean, true if I am handling an exception and I was chosen to handle the exception directly or indirectly due to another handler sending #outer to the exception.
-   **needsRetry**  
    Boolean, true if my protected block needs to be evaluated. Initially true, set to false upon the first evaluation of the protected block, set to true upon the exception being sent #retry or #retryUsing:
-   **actionBlock**  
    The block that is the argument to the do: keyword of #on:do:. I evaluate this block if I am called upon to handle an exception. 
-   **returnBlock**  
    This block, if evaluated, performs a non-local return from the protected block. It is evaluated during processing of #return:, #retry, and #retryUsing to truncate and unwind the stack.
-   **protectedBlock**  
    The block to be evaluated while being prepared to handle some set of exceptions.
-   **actionUnwindBlock**  
    This block, if evaluated, performs a non-local return from the action block. It is evaluated during processing of #resume: and #resignalAs:.
-   **signalingEnvironment**  
    The handler that was the exception environment (first handler to be searched) when the exception I am handling was signaled.

An instance of DefaultActionHandler is always at the root of the handler tree, and handles the case where no handler is found and #defaultAction should be sent to an exception. This avoids complicating the handler code with checks for nil.

**Establishing handlers**

Exception handlers are established by sending #on:do: to a _protected block_. For the duration of the evaluation of the protected block, the exceptions specified by the argument to on: will be handled by the block sent as an argument to do:. This would be implemented something like this:

In closure classes: 

on: exceptionSelector do: actionBlock

    | handler |

    handler := ExceptionHandler

                 withExceptionSelector: exceptionSelector

                 actionBlock: actionBlock

                 protectedBlock: self.

    handler push.

    ^[handler evaluateProtectedBlock] ensure: [handler pop].

In ExceptionHandler:

evaluateProtectedBlock

  | result |

  needsRetry := true.

  [needsRetry] whileTrue: [result := self innerEvaluate].

  ^result.

  

innerEvaluate

   needsRetry := false.

   returnBlock := [:returnValue | ^ returnValue].

   ^ protectedBlock value.

  

push

nextHandler := ExceptionHandler exceptionEnvironment.

ExceptionHandler exceptionEnvironment: self.

  

pop

ExceptionHandler exceptionEnvironment: nextHandler.

During the evaluation of the protected block (the receiver of #on:do:) the ExceptionHandler is in the handler search tree, with an action block to evaluate in case one of the exceptions in the exceptionSelector is signaled. It also has a returnBlock which the handler can use to terminate evaluation of the protected block if it likes.

**Signaling an exception**

An exception is signaled by sending it #signal. Its response is to search up the handler tree for a handler, and to evaluate that handler’s action block. If no handler for this exception’s class is found, the exception is sent #defaultAction. It could be implemented something like this:

In exceptions:

  signal

    | result |

    handler := ExceptionHandler findHandlerFor: self.

    result := [handler handle: self] ensure: [handler := nil].

    “If we reach here, the handler has resumed.”

    self isResumable ifFalse: [self error].

    ^ result.

In ExceptionHandler:

  handle: anException

| returnValue |

    returnValue := self innerHandle: anException.

exceptionToResignal ifNotNil: 

[:ex | | newHandler | newHandler := 

  ExceptionHandler exceptionEnvironment: signalingEnvironment;   findHandlerFor: ex.

  exceptionToResignal := nil.

newHandler handle: ex ].

  

  innerHandle: anException

    exception := anException.

    signalingEnvironment := ExceptionHandler exceptionEnvironment.

    self pop.

    actionUnwindBlock := [:returnValue | ^ returnValue].

    “The actionBlock may not return. If it does, the value 

     it answers must be answered from the #on:do: that established

     my actionBlock.”

    [self return: (actionBlock value: anException)] ensure: [self reset].

    “Flow of control never gets here.”

reset needs to set isOuter to false, exception to nil, etc.

In DefaultActionHandler:

handle: anException

^ anException defaultAction.

“This may or may not return, depending on the defaultAction”

**Finding a handler**

To find a handler for a signaled exception, the ExceptionHandler class sends #findHandlerFor:ifNone: to the handler that is the first handler to search in the current exception environment. In the general case handlers form a tree. In simple cases, the tree has no branches and degenerates to a stack. In either case, each handler has a nextHandler instance variable that points to the next handler to search, which is one level closer to the root of the tree. Each element asks itself whether it handles the given exception. If so, it answers itself; if not it sends the message on to the next element. If no handler in the path from current exception environment to the root handles the exception, this processing gets to the root of the handler tree, the unhandled exception handler. This handler will agree to handle anything, but will send #defaultAction to the exception instead of having an action block to evaluate.

In ExceptionHandler class:

  findHandlerFor: anException

    ^ exceptionEnvironment findHandlerFor: anException.

In ExceptionHandler:

  findHandlerFor: anException

    ^ self handles: anException 

        ifTrue: [self]

        ifFalse: [nextHandler findHandlerFor: anException].

  

  handles: anException

^ exceptionSelector handles: anException.

In DefaultActionHandler:

  handles: anException

^ true.

**Handler actions**

These are messages that can be sent to exceptions from within an action block. They are not valid to send at other times.

**return**

When the message #return or #return: is sent to an exception from within a handler's action block, the stack is unwound to the point where #on:do: was sent, and the specified value (or nil) is answered from the #on:do: send. 

In exceptions:

  return

    self return: nil.

  

  return: returnValue

    “Can only send this from within an action block.”

    self validHandler return: returnValue.

  

  isHandled

    ^ handler ~~ nil.

  

  validHandler

    ^ self isHandled ifTrue: [handler] ifFalse: [self error]

In ExceptionHandler:

  return: returnValue

    returnBlock value: returnValue.

    “The send above does not return to here.”

**resume**

When the message #resume or #resume: is sent to an exception from within a handler's action block, the specified resumption value (or nil) is answered from the #signal, #signal:, or #outer message that caused this handler to be invoked, after unwinding the stack to that point. If the exception is not resumable, it is only valid to send #resume or #resume: within a handler block that is invoked as the result of #outer.

In exceptions:

  resume

    self resume: nil.

  

  resume: resumptionValue

    “Can only send this from within an action block.”

    self validHandler resume: resumptionValue.

In ExceptionHandler:

  resume: resumptionValue

    ^ (isOuter or: [exception isResumable]) 

        ifTrue: [actionUnwindBlock value: resumptionValue]

        ifFalse: [self error]

Here, we once again use non-local return in order to unwind the stack. In this case, we're only unwinding any unwind blocks invoked by the exception action block.

**pass**

When the message #pass is sent to an exception from within a handler's action block, we act as though that handler had never been found, and search again starting with the next handler in the search order.

In exceptions:

  pass

    ^ self validHandler pass.

In ExceptionHandler:

  pass

    | newHandler |

    newHandler := nextHandler findHandlerFor: exception.

    newHandler isOuter: isOuter.

    newHandler handle: exception.

**outer**

In exceptions:

  outer

    ^ self validHandler outer.

In ExceptionHandler:

  outer

    | newHandler |

    newHandler := nextHandler findHandlerFor: exception ifNone: [nil].

    newHandler isOuter: true.

    newHandler handle: exception.

**retry**

In exceptions:

  retry

    self validHandler retry.

In ExceptionHandler:

  retry

    needsRetry := true.

    returnBlock value: nil.

    “Execution does not reach here.”

**retryUsing:**

In exceptions:

  retryUsing: newProtectedBlock

    self validHandler retryUsing: newProtectedBlock.

In ExceptionHandler:

  retryUsing: newProtectedBlock

    protectedBlock := newProtectedBlock.

    ^ self retry.

**resignalAs:**

The message resignalAs:, which has an exception as its argument, effectively does a non-local return from the handler block (which may run unwind blocks) then signals the argument in place of the receiver.

In exceptions: 

  resignalAs: otherException

self validHandler resignalAs: otherException.

In ExceptionHandler:

  resignalAs: otherException

exceptionToResignal := otherException.

actionUnwindBlock value.

“The above never returns to here.”

**isNested**

In exceptions:

  isNested

^ self validHandler isNested.

In ExceptionHandler

  isNested

^ (nextHandler findHandlerFor: exception) 

isDefaultActionHandler not. 

**Tail-call elimination**

>>>Needs to be written

**Debugging**

>>>Needs to be written

**Optimization opportunities**

>>>Needs to be written

**Image Startup**

The Mist image is an ELF64 executable. Its header object specifies that the entire file be loaded into memory at address 16r400000 and execution should start at an _initialization method_.

**Initial State**

**Allocated memory pages**

At the point the kernel passes control to the initialization method, the world as seen from the Mist process is pretty simple. The process has a few memory regions allocated to it:

martin@mite /proc/23090 $ cat maps

00400000-00402000 rwxp 00000000 08:03 12329631                          <path to image>

00402000-00502000 rwxp 00000000 00:00 0                                  [heap]

7ffff7ffe000-7ffff7fff000 r-xp 00000000 00:00 0                          [vdso]

7ffffffdd000-7ffffffff000 rwxp 00000000 00:00 0                          [stack]

ffffffffff600000-ffffffffff601000 r-xp 00000000 00:00 0                  [vsyscall]

The first line shows the image file itself. In this case, the image is only 8K long, being a short test image. The ‘rwxp’ indicates that the image’s area of memory is readable, writeable, executable, and private. The most unusual of these is ‘writeable.’ The traditional text area of an executable file is not writeable, but since Mist modifies its own objects as it runs this is necessary, and is specified in the ELF header object.

[heap] is additional memory requested by the ELF header object, since at this writing Mist unconditionally requests 1MB. This additional memory may or may not be present in later Mist versions – Mist might just allocate exactly the size of the image file, then use mmap if it needs more after startup.

[stack] is the stack area given us by the kernel. It’s initially fairly small (136K in this case) and Mist does not put its stack here, it manages its own stack memory. But this area does contain some useful information. More on that below.

[vdso] is a single page used by glibc to make certain system calls, such as gettimeofday(). Mist does not currently use this, but it might offer a faster way to make certain calls in the future.

[vsyscall] is a single allocated page that gives a way to call kernel routines. This way is now obsolete, replaced by vDSO, so we ignore the presence of this page.

**Registers**

The contents of the general-purpose registers at startup looks like this:

(gdb) info registers

rax            0x0      0

rbx            0x0      0

rcx            0x0      0

rdx            0x0      0

rsi            0x0      0

rdi            0x0      0

rbp            0x0      0x0

rsp            0x7fffffffdac0   0x7fffffffdac0

r8             0x0      0

r9             0x0      0

r10            0x0      0

r11            0x0      0

r12            0x0      0

r13            0x0      0

r14            0x0      0

r15            0x0      0

rip            0x4001b1 0x4001b1

eflags         0x202    [ IF ]

cs             0x33     51

ss             0x2b     43

ds             0x0      0

es             0x0      0

fs             0x0      0

gs             0x0      0

Most are initialized to zero, though we don’t really care about that since Mist should always initialize a register before depending on its value. The instruction pointer rip is set to the address of the initialization method’s first instruction, which was specified in the ELF header object.

  

The stack pointer is the interesting one. In this case, it’s set to an address more than 4K from the end of the stack area. So what’s in that area after the stack pointer?

**Stack**

The kernel puts the arguments and environment variables of the process into the area after the stack pointer. The 64-bit word pointed to by rsp is the argument count.

(gdb) x/g 0x7fffffffdac0

0x7fffffffdac0: 3

So three arguments. But I invoked this test program with the arguments ‘foo’ and ‘bar,’ so why the third argument? There’s also always arg(0), the name of the program itself.

So let’s take a look at the contents of these arguments.

(gdb) x/4g 0x7fffffffdac0

0x7fffffffdac0: 0x3     0x7fffffffde6b

0x7fffffffdad0: 0x7fffffffde9d  0x7fffffffdea1

They’re pointers into farther into the stuff in the stack area. They should point to null-terminated strings.

(gdb) x/s 0x7fffffffde6b

0x7fffffffde6b: "/home/martin/Projects/Mist/pharo3.0/shared/test42"

(gdb) x/s 0x7fffffffde9d

0x7fffffffde9d: "foo"

(gdb) x/s 0x7fffffffdea1

0x7fffffffdea1: "bar"

And indeed they do. So that’s how the arguments work, what about the environment? The environment is supposed to follow the arguments, after a zero word. Let’s see:

0x7fffffffdac0: 0x0000000000000003      0x00007fffffffde6b

0x7fffffffdad0: 0x00007fffffffde9d      0x00007fffffffdea1

0x7fffffffdae0: 0x0000000000000000      0x00007fffffffdea5

Yep, the last argument pointer is followed by a null pointer, then another pointer that points to right after the string ‘bar’. No count of environment pointers, apparently. The environment pointers must go until a null pointer. And they do. And each pointer points to a null-terminated string like

(gdb) x/s 0x00007fffffffefb0

0x7fffffffefb0: "OPENCL_PROFILE=nvidia"

_But_ – We still have some unaccounted space. What is this?

0x7fffffffdd00: 0x00007fffffffefb0      0x0000000000000000

0x7fffffffdd10: 0x0000000000000021

The first pointer is the pointer to the environment string for OPENCL_PROFILE. Next comes the null pointer. And that’s the end of what I’ve seen documented. 

But there’s clearly more, let’s see if it’s anything obvious. That first word is 16r21. What’s after that?

0x7fffffffdd00: 0x00007fffffffefb0      0x0000000000000000

0x7fffffffdd10: 0x0000000000000021      0x00007ffff7ffe000

0x7fffffffdd20: 0x0000000000000010      0x00000000bfebfbff

0x7fffffffdd30: 0x0000000000000006      0x0000000000001000

0x7fffffffdd40: 0x0000000000000011      0x0000000000000064

0x7fffffffdd50: 0x0000000000000003      0x0000000000400040

0x7fffffffdd60: 0x0000000000000004      0x0000000000000038

0x7fffffffdd70: 0x0000000000000005      0x0000000000000001

0x7fffffffdd80: 0x0000000000000007      0x0000000000000000

0x7fffffffdd90: 0x0000000000000008      0x0000000000000000

0x7fffffffdda0: 0x0000000000000009      0x00000000004001b0

0x7fffffffddb0: 0x000000000000000b      0x00000000000003e8

0x7fffffffddc0: 0x000000000000000c      0x00000000000003e8

0x7fffffffddd0: 0x000000000000000d      0x00000000000003e8

0x7fffffffdde0: 0x000000000000000e      0x00000000000003e8

0x7fffffffddf0: 0x0000000000000017      0x0000000000000000

0x7fffffffde00: 0x0000000000000019      0x00007fffffffde49

0x7fffffffde10: 0x000000000000001f      0x00007fffffffefc6

0x7fffffffde20: 0x000000000000000f      0x00007fffffffde59

0x7fffffffde30: 0x0000000000000000      0x0000000000000000

0x7fffffffde40: 0x0000000000000000      0x001d41f5d83fea00

0x7fffffffde50: 0xa7b63b8fdfd1d49b      0x0034365f36387868

0x7fffffffde60: 0x0000000000000000      0x656d6f682f000000

So we have a bunch of small values, a few things that appear to be pointers, and some other data towards the end. The last word above contains the beginning of our arg0 string.

The second word points to the VDSO area. This is probably how the program is supposed to find the VDSO, since its address is not fixed. Mist might use that someday, but for now we’re just going to leave the area after the environment pointers alone.

So that’s the state of our little universe when the kernel hands control over to us. It’s up to us to make things work after that.

**Startup Sequence**

The initialization method is special in that it never returns, since it has no sender. 

>>> needs completion