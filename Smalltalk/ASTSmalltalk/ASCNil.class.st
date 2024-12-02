"
I represent a Nil placeholder do nothing operation.
"
Class {
	#name : 'ASCNil',
	#superclass : 'ASCNonStackReferencingOperations',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'instance creation' }
ASCNil >> fromOperation: anASCBlock [

	stack := anASCBlock stack
]
