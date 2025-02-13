"
I create an array and push the address on the stack
"
Class {
	#name : 'ASCArray',
	#superclass : 'ASCStackReferencingOperations',
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
ASCArray >> resultStack: aStack [

	^ self creates: ((self consumes: size at: aStack) pushClass: Array)
]

{ #category : 'instance creation' }
ASCArray >> with: anInteger [

	size := anInteger
]
