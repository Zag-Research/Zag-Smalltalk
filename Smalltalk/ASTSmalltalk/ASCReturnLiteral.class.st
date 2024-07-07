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

{ #category : 'as yet unclassified' }
ASCReturnLiteral >> resultStack: anASStack [

	stack := anASStack.
	^ (super resultStack: anASStack) next pushValue: literal
]
