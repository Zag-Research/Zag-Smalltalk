"
I remember stores to local variable
"
Class {
	#name : 'ASCStoreVariable',
	#superclass : 'ASCVariableOperations',
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'code generation' }
ASCStoreVariable >> outputTo: anOutput [

	^ variableLocation storeTheVariable: anOutput stackOffset: stack offset
]
