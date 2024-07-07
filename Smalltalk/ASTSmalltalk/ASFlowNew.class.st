"
I am a flow source, with a value.
That value will have to either be passed along to the next block through a phi node, or stored on the stack.
"
Class {
	#name : 'ASFlowNew',
	#superclass : 'ASFlowNonNil',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'output' }
ASFlowNew >> getNameFor: aCollection on: anOutput [

	^ anOutput generateTemp
]

{ #category : 'testing' }
ASFlowNew >> needsToBeSpilled [

	^ true
]

{ #category : 'printing' }
ASFlowNew >> printOn: aStream [

	^ aStream nextPutAll: 'new ';print: value
]
