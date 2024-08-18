Inlining is the primary target-independent optimization for the Zag Smalltalk compiler. This is similar to [SELF](papers-others/An_Efficient_Implementation_of_SELF_a_Dy.pdf); it is available because methods are compiled for each target (class in Smalltalk, prototype in SELF).

When Zag compiler is asked to compile a particular selector for a target class, it looks at the class and its superclasses for the corresponding method, stored as an abstract syntax tree (AST). It is translated to an intermediate form of a series of basic blocks - each ending in either a send or a return. This is perfectly executable and, in fact, is the initial implementation for a method. If it is executed frequently enough, it will get re-compiled with some level of inlining.

When inlining, the compiler looks at all the blocks that end with a send, and tries to find a way to replace the send with a semantically equivalent operation - i.e. to inline the send. This is repeated as long as there is a send that could be replaced.

In some cases all sends can be replaced, in which case there isn't even a reason to create a `Context`, saving a considerable number of instructions.

There are several patterns that can be inlined, based on what we can determine about the receiver.
#### Send to `self`, `super` or known type
These are easy to do, we simply walk the AST for the designated method. The only restriction is that it can't be a recursive send (a call to a method we're currently inlining (any enclosing one)). In the inlined method we simply rename the top elements on the stack to the names from the parameters, and instead of returning, we branch to the block that would have been returned to in the original send.
#### Recursive send to `self`, `super` or known type
The previous case can't inline a recursive send. However, we know exactly the code that should be executed, so we can save the return addresses as if it were a normal call, and then branch directly to the target method.
#### Recursive tail-call send to `self`, `super` or known type
If the previous case is a tail-call, then there is no need to save the return addresses, but rather a simple branch to the target method. This creates a loop.
#### Send to `self`, `super` or known type where the method is primitive
These are easy to do, we simply replace the send with a primitive operation and pass control to the original return block. This may require that the receiver and parameters provably have certain properties (like they are both `SmallInteger`). If we have weaker knowledge we may be able to use a primitive, but it may dispatch a send if the appropriate properties don't hold at runtime (like the result of an add of two `SmallInteger`s produces a `LargeInteger`). In this case we will have to force a `Context` to have been created in this method.
#### Send `value`, `value:`, etc. to a literal `BlockClosure`
This is a special case of a primitive method send. This is very similar to the first case, with two notable exceptions:
1. any explicit return is a non-local return, so it will need to branch to the return point that the method in which the block is defined would use;
2. when doing name resolution, when we get to the position of the receiver on the stack, we skip to the scope where this block is defined.
#### Send where there are few implementations of a method


## Removal of redundant operations
After all inlining is completed there will typically be push operations that are unnecessary, such as pushing an integer constant where the value is propagated so the push is no longer required, or pushing a `BlockClosure` that is subsequently inlined so that the block itself need never be created. There may also be cleanup operations (typically dropping values off the stack) that are simplified or eliminated. Some sends will be required even if the result is unused because sends to unknown methods may have side-effects