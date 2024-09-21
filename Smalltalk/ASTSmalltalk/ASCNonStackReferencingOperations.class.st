"
I am the superclass of all the operations that don't referencce the top elements of the stack
"
Class {
	#name : 'ASCNonStackReferencingOperations',
	#superclass : 'ASCOperations',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'data flow instructions' }
ASCNonStackReferencingOperations >> markReferences [

]

{ #category : 'stack' }
ASCNonStackReferencingOperations >> resultStack: anASStack basicBlock: anObject [

	^ self subclassResponsibility
]
