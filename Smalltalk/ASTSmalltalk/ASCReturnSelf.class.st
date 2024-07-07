"
Clean up stack and return self
"
Class {
	#name : 'ASCReturnSelf',
	#superclass : 'ASCReturn',
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'code generation' }
ASCReturnSelf >> outputTo: output [

	^ output visitReturnSelf: self
]

{ #category : 'stack' }
ASCReturnSelf >> resultStack: anASStack [

	stack := anASStack.
	^ anASStack at: #self
]
