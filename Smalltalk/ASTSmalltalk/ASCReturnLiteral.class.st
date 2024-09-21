"
Clean up stack and return nil
"
Class {
	#name : 'ASCReturnLiteral',
	#superclass : 'ASCReturnSelf',
	#instVars : [
		'literal'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'accessing' }
ASCReturnLiteral class >> literal: literal [

	^ self new literal: literal
]

{ #category : 'accessing' }
ASCReturnLiteral >> literal: anObject [

	literal := anObject
]

{ #category : 'code generation' }
ASCReturnLiteral >> outputTo: output [

	^ output visitReturnLiteral: literal
]

{ #category : 'stack' }
ASCReturnLiteral >> resultStack: aStack basicBlock: aBasicBlock [

	stack := aStack.
	^ self created: ((aStack at: #self) next pushDataAsTemporary:
		  ((ASFlowNew value: literal) basicBlock: aBasicBlock))
]
