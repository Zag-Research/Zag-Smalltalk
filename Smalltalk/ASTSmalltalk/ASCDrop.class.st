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

{ #category : 'API' }
ASCDrop >> dotStringOn: aStream [

	^ aStream << 'drop'
]

{ #category : 'code generation' }
ASCDrop >> outputTo: aCodeEmitter [

	^ aCodeEmitter visitDrop
]

{ #category : 'stack' }
ASCDrop >> resultStack: anASStack basicBlock: anObject [

	^ anASStack drop
]

{ #category : 'instance creation' }
ASCDrop >> with: ignored [
]
