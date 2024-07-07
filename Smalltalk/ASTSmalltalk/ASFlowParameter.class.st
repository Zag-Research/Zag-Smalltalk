"
I am a flow source in a ASReturnBlock or a ASMethodBlock so will be loaded from the stack globally for the block and any successor inline blocks
"
Class {
	#name : 'ASFlowParameter',
	#superclass : 'ASFlowNonNil',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'output' }
ASFlowParameter >> getNameFor: aStack on: anOutput [

	^ anOutput methodScopeName: aStack name
]

{ #category : 'testing' }
ASFlowParameter >> needsToBeSpilled [

	^ false
]

{ #category : 'printing' }
ASFlowParameter >> printOn: aStream [

	aStream nextPutAll: (id ifNil: ['???'])
]
