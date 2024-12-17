"
I represent a nonLocal return usually from a block. I must clean up stack and return to calling context.
"
Class {
	#name : 'ASCReturnNonLocal',
	#superclass : 'ASCReturn',
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'as yet unclassified' }
ASCReturnNonLocal >> outputTo: output [

	^ output visitReturnNonLocalTop
]

{ #category : 'stack' }
ASCReturnNonLocal >> resultStack: anASStack basicBlock: aBasicBlock [

	self flag:
		'needs to know where self is and cleanup stack to that point'.
	stack := anASStack.
	^ anASStack drop
]

{ #category : 'instance creation' }
ASCReturnNonLocal >> with: ignored [

]
