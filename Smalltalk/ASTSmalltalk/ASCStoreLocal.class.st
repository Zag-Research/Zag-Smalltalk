"
I remember stores to local variable
"
Class {
	#name : #ASCStoreLocal,
	#superclass : #ASCLocalOperations,
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'code generation' }
ASCStoreLocal >> outputTo: anOutput [

	anOutput visitStoreLocal: local
]
