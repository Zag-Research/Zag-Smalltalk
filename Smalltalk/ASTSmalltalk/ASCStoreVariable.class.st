"
I remember stores to local variable
"
Class {
	#name : 'ASCStoreVariable',
	#superclass : 'ASCVariableOperations',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'code generation' }
ASCStoreVariable >> outputTo: anOutput [

	^ variableLocation storeTheVariable: anOutput stackOffset: stack offset
]
