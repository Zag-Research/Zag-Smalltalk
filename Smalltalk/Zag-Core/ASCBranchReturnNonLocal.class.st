"
I parallel ASCReturnNonLocal
"
Class {
	#name : 'ASCBranchReturnNonLocal',
	#superclass : 'ASCBranchReturn',
	#traits : 'ASCContextDependentOperations',
	#classTraits : 'ASCContextDependentOperations classTrait',
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'initialization' }
ASCBranchReturnNonLocal >> resultStack: anASStack [

	self flag:
		'needs to know where self is and cleanup stack to that point'.
	stack := anASStack.
	^ anASStack drop
]
