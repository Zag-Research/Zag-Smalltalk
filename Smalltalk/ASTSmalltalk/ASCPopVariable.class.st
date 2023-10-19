"
I remember local pops
"
Class {
	#name : #ASCPopVariable,
	#superclass : #ASCVariableOperations,
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'code generation' }
ASCPopVariable >> outputTo: anOutput [

	^ variable pop: anOutput
]
