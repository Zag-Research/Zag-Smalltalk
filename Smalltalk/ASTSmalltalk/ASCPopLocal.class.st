"
I remember local pops
"
Class {
	#name : #ASCPopLocal,
	#superclass : #ASCLocalOperations,
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'code generation' }
ASCPopLocal >> outputTo: anOutput [

	anOutput visitPopLocal: local 
]
