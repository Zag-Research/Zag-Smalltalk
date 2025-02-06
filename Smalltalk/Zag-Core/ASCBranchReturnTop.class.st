"
I parallel ASCReturnTop
"
Class {
	#name : 'ASCBranchReturnTop',
	#superclass : 'ASCBranchReturn',
	#traits : 'ASCContextDependentOperations',
	#classTraits : 'ASCContextDependentOperations classTrait',
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'stack' }
ASCBranchReturnTop >> resultStack: aStack [

	| stackElement |
	stack := aStack.
	stackElement := aStack indirectElement.
	^ self createsTemporary: stackElement next data: aStack data
]
