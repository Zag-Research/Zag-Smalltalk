"
I document the API that any translator from threaded representation of a CompiledMethod must implement.

Currently this is only used by the LLVM jitter

The methods defined in this trait must exactly correspond with the `references` array in controlPrimitives.zig
They are used to automatically generate the shared variables in `ZagConstants`
"
Trait {
	#name : 'ZagImageOutputAPI',
	#category : 'Zag-Core-Output',
	#package : 'Zag-Core',
	#tag : 'Output'
}

{ #category : 'threaded words' }
ZagImageOutputAPI >> branch [
	" branch to the address in the next field "

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> call [
	" call to the method at the address in the next field.
	Exactly a send without the dispatch, so set up the return addresses appropriately.
	Because there is no dispatch, there is no PIC, and no selector.
	Used for self-recursive calls and calls to know methods (because the target is a single class)  "

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> classCase [
	" jump to one of a series of addresses based on the class of the TOS.
	Followed by a series of jump tables. Each table looks like:
	an integer with a sequence of up to 4 classes (14 bits each, first in the bottom 14 bits);
	followed by the same number of addresses,
	It the class of the TOS matches any of them, jump to the corresponding address.
	If none, continue with the next word.
	There can be a series of `classCase` operations to handle more than 4 classes "

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> classCase24 [
	" jump to one of a series of addresses based on the class of the TOS.
	reserved in case we have a system with more than 16K classes
	- uses 28 bits to encode a class, so works with millions of classes "

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> cullColon [
	" `cull:` recognizes special cases
	Falls back to fast-dispatch through the reserve location in the target's `Dispatch` table. "

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> drop [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> dropNext [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> dup [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> label [
	" start of a runtime basic block.
	Somebody branches to me other than in a fall-through way.
	No functional requirement.
	Used as part of conversion from threaded to e.g. LLVM JIT code. "

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> makeImmediateClosure [
	" TOS is an address. Shift that left 16, or with next integer value, replace TOS. This has the effect of creating an immediate BlockClosure "

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> over [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> popAssociationValue [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> popIndirect [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> popIndirectLocal [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> popInstVar [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> popLocal [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> popLocalData [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> popLocalField [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> primitive [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> primitiveError [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> primitiveModule [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> primitiveModuleError [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> pushAssociationValue [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> pushContext [
	" create context with parameters in the next value "

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> pushIndirect [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> pushIndirectLocal [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> pushInstVar [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> pushLiteral [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> pushLocal [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> pushLocalData [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> pushLocalField [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> pushStack [
	" like pushLocal, except stack offset rather than context index "

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> pushThisContext [
	" push the address of the current context "

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> pushThisProcess [
	" push the address of the current process "

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> returnSelf [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> returnSelfNoContext [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> returnTop [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> returnTopNoContext [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> returnTopNonLocal [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> returnTopNonLocalNoContext [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> send [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> storeLocal [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> swap [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> tailCallMethod [
	" call to the method at the address in the next field.
	Exactly a tailSend without the dispatch.
	Because there is no dispatch, there is no PIC, and no selector.
	Used for self-recursive calls and calls to known methods (where the target is a single class)  "

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> tailSend [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> tailSendNoContext [

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> value [
	" `value` recognizes special cases
	Falls back to fast-dispatch through the reserve location in the target's `Dispatch` table. "

	^ self explicitRequirement
]

{ #category : 'threaded words' }
ZagImageOutputAPI >> valueColon [
	" `value:` recognizes special cases
	Falls back to fast-dispatch through the reserve location in the target's `Dispatch` table. "

	^ self explicitRequirement
]
