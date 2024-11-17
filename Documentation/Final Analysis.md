## Inlining
Inlining is the primary target-independent optimization for the Zag Smalltalk compiler. This is similar to [SELF](papers-others/An_Efficient_Implementation_of_SELF_a_Dy.pdf); it is available because methods are compiled for each target (class in Smalltalk, prototype in SELF).

When the Zag compiler is asked to compile a particular selector for a target class, it looks at the class and its superclasses for the corresponding method, stored as an abstract syntax tree (AST). It is translated to an intermediate form of a series of basic blocks - each ending in either a send or a return. This is perfectly executable and, in fact, is the initial implementation for a method (output as either threaded or native code). If it is executed frequently enough, it will get re-compiled with some level of inlining.

When inlining, the compiler looks at each block that end with a send, and tries to find a way to replace the send with a semantically equivalent operation - i.e. to inline the send. This is repeated as long as there is a send that could be replaced.

In some cases all sends can be replaced, in which case there isn't even a reason to create a `Context` for the method (because the `Context` is where we would save the return address for the send), saving a considerable number of instructions.

There are several patterns that can be inlined, based on what we can determine about the receiver. In the following, "known type" is clearly safe for a literal or literal parameter, but harder if it is some kind of calculation or uses a local variable.
#### Send to `self`, `super` or known type
These are easy to do, we simply walk the AST of the designated method. The only restriction is that it can't be a recursive send (a call to a method we're currently inlining (any enclosing one)). In the inlined method we simply rename the top elements on the stack to the names from the parameters, and instead of returning, we branch to the block that would have been returned to in the original send.
#### Send to `self`, `super` or known type when can't inline
If we know exactly the code that should be executed, but can't inline (because we're not inlining, or because we've already inlined too much), we can save the return addresses as if it were a normal call, but instead of sending, we can do a call of the exact method.
#### Recursive send to `self`, `super` or known type
The previous case can't inline a recursive send. However, we know exactly the code that should be executed, so we can save the return addresses as if it were a normal call, and then branch directly to the target method.
#### Recursive tail-call send to `self`, `super` or known type
If the previous case is a tail-call, then there is no need to save the return addresses, but rather a simple branch to the target method. This creates a loop.
#### Send to `self`, `super` or known type where the method is primitive
These are easy to do, we simply replace the send with a primitive operation and pass control to the original return block. This may require that the receiver and parameters provably have certain properties (like they are both `SmallInteger`). If we have weaker knowledge we may be able to use a primitive, but it may dispatch a send if the appropriate properties don't hold at runtime (like the result of an add of two `SmallInteger`s produces a `LargeInteger`). In this case we will have to force a `Context` to have been created in this method.
#### Send `value`, `value:`, etc. to a literal `BlockClosure` - safe
This is a special case of a primitive method send. This is very similar to the first case, with two notable exceptions:
1. any explicit return is a non-local return, so it will need to branch to the return point that the method in which the block is defined would use;
2. when doing name resolution, when we get to the position of the receiver on the stack, we skip to the scope where this block is defined.
#### Send where there are few implementations of a method - not safe
If there are only a few classes that have access to an implementation of a particular message (i.e they have it themselves or a superclass has an implementation), we emit a class-case instruction and inline the method for each of them, with each of them branching to the original return block on "return". To have the correct/conservative semantics, we need to retain a fall-back of doing the original send, unless we can prove that the list is exhaustive.
#### Send where the target is the result of a comparison primitive - safe
As a special case of the previous case, we know that comparison primitives always return `true` or `false` (or an error if the values are incomparable), so we have an exhaustive list. But more, we now know something about the relationship of these values. So, for example, we might know that a value is in the range of 1 to the size of an array, which means that we can safely use that value to index into the array.
## Removal of redundant `BlockClosure`s
After all inlining is completed there will typically be pushes of `BlockClosure`s that are subsequently inlined so that the block itself need never be created. These are turned into pushes of `nil`.
## Compiling required `BlockClosure`s
Any `BlockClosure`s that remain after the previous step must be compiled, and the above inlining operations performed.
## Removal of redundant operations
After all inlining is completed there will typically be push operations that are unnecessary, such as pushing an integer constant where the value is propagated so the push is no longer required. There may also be cleanup operations (typically dropping values off the stack) that are simplified or eliminated. Some sends will be required even if the result is unused because sends to unknown methods may have side-effects. Note that `self` values may need at least nil pushed into the location so there is space to return a value.
## Optimizing local variable locations
If we have block closures, we now determine the optimum location for each method-scoped variable. There are several possibilities:
1. If a variable is only referenced in the method, it will be put in the `Context` (or just on the stack if no context is created).
2. If the variable is only referenced in one `BlockClosure` then it will be created as a local variable there.
3. If a variable isn't modified after a `BlockClosure` (in which it's referenced) is created, it can be copied to that closure as a read-only value and only exist as a local variable in the creating unit. Alternately, it could be only in that closure.
4. For values referenced in two or more places, modified in at least one, the default would be to put them in the `Context`.  However, if a `BlockClosure` has a reference to the `Context` and the closure gets moved to the heap, it will drag the entire stack with it. Therefore the only closures that reference the context will be ones with non-local returns (or that create closures that need a context reference). Variables referenced in non-local-return closures will be placed in the context.
5. All other variables will be placed in a closure that modifies the variable.
## Non-structural inlining
The next stage is to inline primitives that can't affect the control-flow graph, but can use type information that is not available until the data-flow graph is complete. Current examples are `at:` and `at:put:` where the receiver is known to be an `Array`, `String` or one of the other special array types.
## Creating `Context` and stack offsets
Creating a `Context` is fairly expensive (dozens of instructions) and they are only required if we send a message or create a `BlockClosure` that does a non-local return. After inlining, some methods may not require a context along certain paths (see `fibonacci`). Therefore we want to delay creating a `Context` as long as we can. The size of a `Context` is variable, because it contains: `self`, the parameters, the locals, references to any `BlockClosures` we need to reference, as well as space for all the `BlockClosure`s that we create in the method.

Each block has a context status:
 - `nil` means it hasn't been established yet
 - 0 means it is inherited - all the blocks that come to this block created or inherited a context
 - MAXINT means there is no context
 - n>0 is the index in the block of the context-create operation  - any references before that calculate offsets without the context, any after calculate with the context

The algorithm to set all the context statuses is:
1. Tell the first basic block that it has no context.
2. If we haven't established yet and we're told we have not context, go through all the operations in the block
	1. If we don't have a context yet and the current instruction needs a context, insert one (ideally before any pushes in this block) and then resume scanning from that point.
	2. If the current instruction references a local, calculate the offset (with or without the closure size) for that instruction
3. If we still have no context and any of our next blocks  have an inherited context, then we must create one, and then tell all our next blocks that they have inherited, otherwise tell all our next blocks that they have no context.
4. If we now have a context, tell each of our next blocks that they have inherited a context
5. If we haven't established yet and we're told we have inherited a context, go through all the operations in the block:
	1. If the current instruction references a local, calculate the offset with context
6. If we had no context and we're being told to inherit...
7. If we had inherited a context
8. If we creates a context in this block and we're being told we inherit...