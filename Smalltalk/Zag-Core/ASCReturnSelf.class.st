"
Clean up stack and return self
"
Class {
	#name : 'ASCReturnSelf',
	#superclass : 'ASCReturn',
	#category : 'Zag-Core-CodeGen',
	#package : 'Zag-Core',
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
	^ self createsTemporary: stackElement next data: stackElement data
]
