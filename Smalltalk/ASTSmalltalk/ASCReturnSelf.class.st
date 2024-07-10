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
ASCReturnSelf >> resultStack: aStack basicBlock: aBasicBlock [

	| stackElement |
	stack := aStack.
	stackElement := aStack at: #self.
	^ (ASStackTemporary fromStack: stackElement next) data: ((ASFlowNew from: stackElement data) basicBlock: aBasicBlock)
]
