## Optimization Opportunities

- self messages where the method is not overridden can be inlined
	- doesn't matter if the method is from a superclass, as long as it's not overridden below the current class
	- can be a bit tricky if the inlined method is from a superclass, because self messages there refer to both messages defined there and (since we're inlining) anything down to the current method as long as not overridden below there
- if we have recognized the other argument to be the same class as self, then ditto
- tail call elimination
- closures for blocks that send no messages can be allocated on the stack
- would like to not special-case ifTrue: and friends
-  we will recognize methods that are sent to particular classes (or messages that have limited numbers of implementations (like 1 or 2) e.g. `yourself`, `ifTrue:ifFalse:`, `whileTrue:`, `ifNil:`) and be able to inline them
- the Object value for Symbols will have the index into the Symbol table and also the arity. Then the dispatch keys will have the same information encoded for each method so we don't have to check arity separately
- the object header for BlockClosure will have the symbol hash (one of `#value`, `#value:`, etc.) as the hash value and BlockClosure as the class, This will be followed by code address (which would look like a double, if anyone could see it), then `self`, a reference to the indirect vector if any, and the closure fields. If the Symbol matches, we dispatch directly, else we dispatch through the class table. The same bit described below is set if the code is source code.
- when code is generated for a method, any messages that are inlined must be noted so that if any of them are replaced, the generated code can be invalidated so that it can be regenerated - likely do this conservatively
- dispatch handles special symbol values that can't come from Smalltalk code
	1. I thought of having a bit that says this is a primitive failure and it encodes the Smalltalk code in a primitive method. To get the Symbol value, just AND the low bits. However, all we actually need is to code the same Symbol, but with an extra parameter number for the error code. (This would not correspond to any actual code, because the arity is encoded in the extended hash for Symbols). This would only exist in the dispatch table for the class.
	2. I thought of having a bit that says this is looking for the interpretive code - this would have been set by the dispatch code itself when it doesn't find a compiled version. However, I realized that if the pointer is below the Smalltalk heap, then it's compiled code, and if it's above, it's source code -so we don't have to hash twice
- there is a global lock guarding any class updates:
	- updating dispatch tables
		1. resizing
		2. compiling a method
		3. filling in an entry
		4. compiling a block closure
	- user-code-driven changes
- exporter has map of Pharo primitives to AST primitive numbers - signals error for any unmapped ones
- if my class is <= 8 I can check with an AND and comparison. Use for `nil`, `true`, and `false` rather than compare the value. Can also check for the special return value
- `at:`, `at:put:` and `instVarAt:`,`instVarAt:put:` are opposite for most classes; e.g. Array has the former, but the latter signal an error, whereas Point has the opposite behavior. Therefore, when looking up one of these on a failed dispatch, we can choose the appropriate implementations of these depending on the format of the class. Ditto for ByteArray/String, Symbol, format 9.