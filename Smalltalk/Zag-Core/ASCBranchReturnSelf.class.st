"
I parallel ASCReturnSelf
"
Class {
	#name : 'ASCBranchReturnSelf',
	#superclass : 'ASCBranchReturn',
	#traits : 'ASCContextDependentOperations',
	#classTraits : 'ASCContextDependentOperations classTrait',
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'stack' }
ASCBranchReturnSelf >> resultStack: aStack [

	| stackElement |
	stack := aStack.
	stackElement := aStack indirectElement.
	^ self createsTemporary: stackElement next data: stackElement data
]
