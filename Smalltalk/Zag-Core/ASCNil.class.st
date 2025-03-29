"
I represent a Nil placeholder do nothing operation.
"
Class {
	#name : 'ASCNil',
	#superclass : 'ASCNonStackReferencingOperations',
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'instance creation' }
ASCNil >> fromOperation: anASCBlock [

	stack := anASCBlock stack
]

{ #category : 'as yet unclassified' }
ASCNil >> interpretFor: anASemanticInterpreter [ 
	^ true
]
