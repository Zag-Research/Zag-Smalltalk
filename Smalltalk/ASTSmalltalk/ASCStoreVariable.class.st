"
I remember stores to local variable
"
Class {
	#name : #ASCStoreVariable,
	#superclass : #ASCVariableOperations,
	#category : 'ASTSmalltalk-CodeGen'
}

{ #category : #'code generation' }
ASCStoreVariable >> outputTo: anOutput [

	^ variable store: anOutput
]
