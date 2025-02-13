"
Clean up stack and return self
"
Class {
	#name : 'ASCReturnSelf',
	#superclass : 'ASCReturn',
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'Collection API' }
ASCReturnSelf >> inlineWithFixedCFG: anASCodeGenerator [ 
]

{ #category : 'code generation' }
ASCReturnSelf >> outputTo: output [

	^ output visitReturnSelf: self
]

{ #category : 'stack' }
ASCReturnSelf >> resultStack: aStack [

	| stackElement |
	stack := aStack.
	stackElement := aStack at: #self.
	^ self
		  consumes: stackElement;
		  createsTemporary: stackElement next data: stackElement data
]
