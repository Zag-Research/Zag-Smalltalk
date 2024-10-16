"
I am an abstract class for all the compilable unitss that are dispatchable as opposed to inlined.
"
Class {
	#name : 'ASCDispatchableBlock',
	#superclass : 'ASCCompilableUnitBlock',
	#instVars : [
		'declares',
		'owns',
		'modifies',
		'references'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'categories' }
ASCDispatchableBlock class >> recategorizeSelector: selector from: oldProtocol to: newProtocol [ 
	^ self subclassResponsibility
]

{ #category : 'accessing' }
ASCDispatchableBlock >> declares [

	^ declares
]

{ #category : 'accessing' }
ASCDispatchableBlock >> declares: anObject [

	declares := anObject
]

{ #category : 'analyzing' }
ASCDispatchableBlock >> deriveOwnership: variables [

	| doBlock absentBlock |
	"
	1. If a variable is only referenced in the method, it will be put in the Context (or just on the stack if no context is created).
   2. If the variable is only referenced in one BlockClosure then it will be created as a local variable there.
   3. For values referenced in two or more places, modified in at least one, the default would be to put them in the Context. However, if a BlockClosure has a reference to the Context and the closure gets moved to the heap, it will drag the entire stack with it. Therefore the only closures that reference the context will be ones with non-local returns (or that create closures that need a context reference). Variables referenced in non-local-return closures will be placed in the context.
   4. All other variables will be placed in a closure that modifies the variable.
"
	doBlock := [ :var | variables at: var ifPresent: [ :collection | collection add: self ] ifAbsentPut: [ | set | set := Set new. set add: self. set ] ].
	references do: doBlock.
	modifies do: doBlock .
]

{ #category : 'accessing' }
ASCDispatchableBlock >> initialize [
	super initialize.
	references := Set new.
	modifies := Set new.
	owns := Set new.
	declares := Set new
]

{ #category : 'accessing' }
ASCDispatchableBlock >> modified [

	^ modifies
]

{ #category : 'accessing' }
ASCDispatchableBlock >> modified: anObject [

	modifies := anObject
]

{ #category : 'accessing' }
ASCDispatchableBlock >> owns [

	^ owns
]

{ #category : 'accessing' }
ASCDispatchableBlock >> owns: anObject [

	owns := anObject
]

{ #category : 'accessing' }
ASCDispatchableBlock >> referenced [

	^ references
]

{ #category : 'accessing' }
ASCDispatchableBlock >> referenced: anObject [

	references := anObject
]
