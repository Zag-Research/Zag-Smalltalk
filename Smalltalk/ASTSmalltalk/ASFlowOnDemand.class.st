"
I am a flow source placeholder. If referenced, I will be loaded from the stack or context.
"
Class {
	#name : 'ASFlowOnDemand',
	#superclass : 'ASFlowNew',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'printing' }
ASFlowOnDemand >> printOn: aStream [

	^ aStream nextPutAll: 'on demand'
]
