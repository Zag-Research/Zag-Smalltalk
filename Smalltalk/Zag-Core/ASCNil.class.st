"
I represent a Nil placeholder do nothing operation.
"
Class {
	#name : 'ASCNil',
	#superclass : 'ASCNonStackReferencingOperations',
	#category : 'Zag-Core-CodeGen',
	#package : 'Zag-Core',
	#tag : 'CodeGen'
}

{ #category : 'instance creation' }
ASCNil >> fromOperation: anASCBlock [

	stack := anASCBlock stack
]
