"
I remember to push local variable
"
Class {
	#name : #ASCPushVariable,
	#superclass : #ASCVariableOperations,
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'code generation' }
ASCPushVariable >> outputTo: anOutput [

	^ variable push: anOutput
]
