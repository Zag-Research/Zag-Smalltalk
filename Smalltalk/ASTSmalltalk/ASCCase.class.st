"
I am used for inlining.

I have a map of class, ??? pairs which will translate into a class-dispatch instruction in threaded code.

Originally there was an ASCSend here but the compiler recognized that there were a limited number of implementations, so we could inline the methods.
"
Class {
	#name : 'ASCCase',
	#superclass : 'ASCStackReferencingOperations',
	#instVars : [
		'map',
		'numArgs'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASCCase >> at: key [

	^ map at: key
]

{ #category : 'accessing' }
ASCCase >> at: key put: value [

	^ map at: key put: value
]

{ #category : 'accessing' }
ASCCase >> blocks [

	^ map values
]

{ #category : 'initialization' }
ASCCase >> initialize [

	super initialize.
	map := Dictionary new
]

{ #category : 'printing' }
ASCCase >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream space.
	map printElementsOn: aStream
]

{ #category : 'data flow instructions' }
ASCCase >> markReferences [

	(stack at: numArgs) markReferenced
]

{ #category : 'accessing' }
ASCCase >> nextBasicBlocks [

	^ map values
]

{ #category : 'code generation' }
ASCCase >> outputTo: anOutput [
	^ anOutput
		  visitCase: map
]

{ #category : 'stack' }
ASCCase >> resultStack: anASStack basicBlock: anObject [

	stack := anASStack.
	^ anASStack
]

{ #category : 'instance creation' }
ASCCase >> with: anInteger [

	numArgs := anInteger
]
