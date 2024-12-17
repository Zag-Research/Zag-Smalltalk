"
I am a flow source in a ASReturnBlock or a ASMethodBlock so will be loaded from the stack globally for the block and any successor inline blocks
"
Class {
	#name : 'ASFlowParameter',
	#superclass : 'ASFlowOnDemand',
	#category : 'Zag-Core-Stack',
	#package : 'Zag-Core',
	#tag : 'Stack'
}

{ #category : 'output' }
ASFlowParameter >> getNameFor: aStackElement on: anOutput [

	^ outputName ifNil: [ outputName := anOutput methodScopeName: aStackElement name ]
]

{ #category : 'printing' }
ASFlowParameter >> printOn: aStream [

	aStream
		nextPutAll: 'on demand';
		nextPutAll: (outputName ifNil: [ '' ])
]

{ #category : 'output' }
ASFlowParameter >> pushTheVariable: aStackElement with: anASLLMethodOutput [ 
	self halt.
]
