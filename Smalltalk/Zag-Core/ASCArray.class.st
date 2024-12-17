"
I create an array and push the address on the stack
"
Class {
	#name : 'ASCArray',
	#superclass : 'ASCNonStackReferencingOperations',
	#instVars : [
		'size'
	],
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'printing' }
ASCArray >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream << $(.
	aStream print: size.
	aStream << $)
]

{ #category : 'code generation' }
ASCArray >> outputTo: output [

	^ output visitPushArray: size
]

{ #category : 'stack' }
ASCArray >> resultStack: anASStack basicBlock: aBasicBlock [

	| resultStack |
	resultStack := stack := anASStack.
	size timesRepeat: [ resultStack := resultStack next ].
	^ self creates: (resultStack pushClass: Array)
]

{ #category : 'instance creation' }
ASCArray >> with: anInteger [

	size := anInteger
]
