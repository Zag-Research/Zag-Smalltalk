"
I remember stores to local variable
"
Class {
	#name : 'ASCStoreVariable',
	#superclass : 'ASCVariableOperations',
	#category : 'Zag-Core-CodeGen',
	#package : 'Zag-Core',
	#tag : 'CodeGen'
}

{ #category : 'code generation' }
ASCStoreVariable >> outputTo: anOutput [

	^ variableLocation storeTheVariable: anOutput stackOffset: stack offset
]
