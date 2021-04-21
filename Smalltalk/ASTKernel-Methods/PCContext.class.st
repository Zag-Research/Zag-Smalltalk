Class {
	#name : #PCContext,
	#superclass : #PCObject,
	#instVars : [
		'sender',
		'pc',
		'stackp'
	],
	#category : #'ASTKernel-Methods'
}

{ #category : #'as yet unclassified' }
PCContext class >> newForMethod: aMethod [
	"This is the only method for creating new contexts, other than by using the clone primitive. Any other attempts, such as inherited methods like shallowCopy, should be avoided or must at least be rewritten to determine the proper size for the method that will use this context. This is because asking a context its size (even basicSize!) will not return the actual object size but only the number of fields currently accessible, as determined by stackp."

	^ super basicNew: aMethod frameSize
]

{ #category : #'as yet unclassified' }
PCContext >> blockCopy: numArgs [
	"Primitive. Distinguish a block of code from its enclosing method by creating a new BlockContext for that block. The compiler inserts into all methods that contain blocks the bytecodes to send the message blockCopy:. Do not use blockCopy: in code that you write! Only the compiler can decide to send the message blockCopy:. Fail if numArgs is not a SmallInteger. Optional. No Lookup. See Object documentation whatIsAPrimitive."

	<primitive: 80>
	^ (PCBlock newForMethod: self home method)
		home: self home
		startpc: pc + 2
		nargs: numArgs
]

{ #category : #'as yet unclassified' }
PCContext >> isContextPart [
	^ true
]

{ #category : #'as yet unclassified' }
PCContext >> sender [
	"Answer the context that sent the message that created the receiver."

	^ sender
]
