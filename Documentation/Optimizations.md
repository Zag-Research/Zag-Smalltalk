## Optimization Opportunities

- self messages where the method is not overridden can be inlined
- if we have recognized the other argument to be the same class as self, then ditto
- tail call elimination
- closures for blocks that send no messages can be allocated on the stack
- would like to not special-case ifTrue: and friends
- maybe have `value` as a special case in the dispatch table so that blocks can be evaluated faster
- alternately, blocks have a single-entry table, and if the symbol index isn't the desired one, they do a `super` send