"
I represent an encoding of a reference to `thisContext`
"
Class {
	#name : 'ASCPushThisContext',
	#superclass : 'ASCNonStackReferencingOperations',
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'testing' }
ASCPushThisContext >> needsContext: aStackContext [

	^ true
]
