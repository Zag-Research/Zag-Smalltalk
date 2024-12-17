"
I implement very simple operations described by a symbol
"
Class {
	#name : 'ASCDup',
	#superclass : 'ASCStackReferencingOperations',
	#instVars : [
		'symbol'
	],
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'printing' }
ASCDup >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream << $(.
	aStream print: symbol.
	aStream << $)
]

{ #category : 'data flow instructions' }
ASCDup >> markReferences [

	^ stack markReferenced
]

{ #category : 'code generation' }
ASCDup >> outputTo: aCodeEmitter [

	^ aCodeEmitter visitSimple: symbol
]

{ #category : 'stack' }
ASCDup >> resultStack: anASStack basicBlock: aBasicBlock [

	stack := anASStack.
	^ self
		  consumes: anASStack;
		  creates: (anASStack replicate: anASStack)
]

{ #category : 'as yet unclassified' }
ASCDup >> with: anObject [

	 symbol := anObject
]
