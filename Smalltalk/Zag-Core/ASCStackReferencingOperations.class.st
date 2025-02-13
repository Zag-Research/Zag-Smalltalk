"
I am the superclass of all the operations that referencce the top elements of the stack
"
Class {
	#name : 'ASCStackReferencingOperations',
	#superclass : 'ZagOperations',
	#category : 'Zag-Core-Operations',
	#package : 'Zag-Core',
	#tag : 'Operations'
}

{ #category : 'accessing' }
ASCStackReferencingOperations >> consumes: aStackElement [

	^ aStackElement location consumer: self
]

{ #category : 'stack' }
ASCStackReferencingOperations >> consumes: n at: anASStack [

	| resultStack |
	resultStack := stack := anASStack.
	n timesRepeat: [
		self consumes: resultStack.
		resultStack := resultStack next ].
	^ resultStack
]
