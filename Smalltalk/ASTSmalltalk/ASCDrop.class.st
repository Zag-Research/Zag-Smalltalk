"
I implement very simple operations described by a symbol
"
Class {
	#name : 'ASCDrop',
	#superclass : 'ASCNonStackReferencingOperations',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'callgraph' }
ASCDrop >> dotStringOn: aStream [

	^ aStream << 'drop'
]

{ #category : 'code generation' }
ASCDrop >> outputTo: aCodeEmitter [

	^ aCodeEmitter visitDrop
]

{ #category : 'stack' }
ASCDrop >> resultStack: anASStack basicBlock: anObject [

	stack := anASStack.
	^ anASStack drop
]

{ #category : 'as yet unclassified' }
ASCDrop >> shrinksStack [

	^ true
]

{ #category : 'instance creation' }
ASCDrop >> with: ignored [
]
