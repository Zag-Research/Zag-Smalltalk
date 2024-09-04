"
I implement very simple operations described by a symbol
"
Class {
	#name : 'ASCDup',
	#superclass : 'ASCStackReferencingOperations',
	#instVars : [
		'symbol'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
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

{ #category : 'accessing' }
ASCDup >> next [
	^ nil
]

{ #category : 'code generation' }
ASCDup >> outputTo: aCodeEmitter [

	^ aCodeEmitter visitSimple: symbol
]

{ #category : 'stack' }
ASCDup >> resultStack: anASStack basicBlock: anObject [

	stack := anASStack.
	^ self created: (anASStack replicate: anASStack)
]

{ #category : 'as yet unclassified' }
ASCDup >> with: anObject [

	 symbol := anObject
]
