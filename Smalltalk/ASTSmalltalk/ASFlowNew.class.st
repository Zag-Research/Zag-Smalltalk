"
I am a flow source, with a value.
That value will have to either be passed along to the next block through a phi node, or stored on the stack.
"
Class {
	#name : 'ASFlowNew',
	#superclass : 'ASFlowDefine',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'printing' }
ASFlowNew >> printOn: aStream [

	^ aStream nextPutAll: 'new'
]
