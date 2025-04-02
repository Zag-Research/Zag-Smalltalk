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
ASCReturnNonLocal >> interpretFor: anInterpreter [ 

"need to adjust stack by some offset"
"need to return to defining blocks method, or terminate"

"(inContext and: [ stackElement isInContext ]) ifTrue: [
		^ anInterpreter
			  return: anInterpreter top
			  selfContext: stackElement offset ]."
	^ anInterpreter
		  returnNonLocal: anInterpreter top
]

{ #category : 'as yet unclassified' }
ASCReturnNonLocal >> outputTo: output [

	^ output visitReturnNonLocalTop
]

{ #category : 'stack' }
ASCReturnNonLocal >> resultStack: anASStack [

	self flag:
		'needs to know where self is and cleanup stack to that point'.
	stack := anASStack.
	^ anASStack drop
]

{ #category : 'instance creation' }
ASCReturnNonLocal >> with: ignored [

]
