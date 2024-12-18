"
I am a flow source placeholder. I am the flow value of everything at the start of an ASCReturnBlock. If referenced, I will be loaded from the stack or context.
"
Class {
	#name : 'ASFlowOnDemand',
	#superclass : 'ASFlowNonNil',
	#category : 'Zag-Core-Stack',
	#package : 'Zag-Core',
	#tag : 'Stack'
}

{ #category : 'output' }
ASFlowOnDemand >> getNameFor: aStackElement on: anOutput [

	^ outputName ifNil: [ outputName := anOutput loadFromStack: aStackElement ]
]

{ #category : 'testing' }
ASFlowOnDemand >> needsToBeSpilled [

	^ false
]

{ #category : 'printing' }
ASFlowOnDemand >> printOn: aStream [

	aStream nextPutAll: 'on demand'.
]
