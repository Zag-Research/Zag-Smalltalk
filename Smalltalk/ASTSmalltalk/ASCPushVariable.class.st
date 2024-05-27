"
I remember to push local variable
"
Class {
	#name : 'ASCPushVariable',
	#superclass : 'ASCVariableOperations',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'code generation' }
ASCPushVariable >> outputTo: anOutput [
	^ variable push: anOutput
]

{ #category : 'stack analysis' }
ASCPushVariable >> stackOp: output [ 

	^ output stackPush: self
]
