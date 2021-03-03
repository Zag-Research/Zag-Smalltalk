## Optimization Opportunities

- self messages where the method is not overridden can be inlined
	- doesn't matter if the method is from a superclass, as long as it's not overridden below the current class
	- can be a bit tricky if the inlined method is from a superclass, because self messages there refer to both messages defined there and (since we're inlining) anything down to the current method as long as not overridden below there
- if we have recognized the other argument to be the same class as self, then ditto
- tail call elimination
- closures for blocks that send no messages can be allocated on the stack
- would like to not special-case ifTrue: and friends
- maybe have `value` as a special case in the dispatch table so that blocks can be evaluated faster
- alternately (slightly less efficiently), blocks have a single-entry table, and if the symbol index isn't the desired one, they do a `super` send
- we will recognize methods that are sent to particular classes (or messages that have limited numbers of implementations (like 1 or 2) e.g. `yourself`, `ifTrue:ifFalse:`, `whileTrue:`) and be able to inline them