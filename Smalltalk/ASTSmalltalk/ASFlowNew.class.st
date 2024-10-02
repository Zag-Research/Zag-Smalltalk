"
I am a flow source, with a value.
That value will have to either be passed along to the next block through a phi node, or stored on the stack.
"
Class {
	#name : 'ASFlowNew',
	#superclass : 'ASFlowNonNil',
	#instVars : [
		'basicBlock'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'accessing' }
ASFlowNew >> basicBlock: aBasicBlock [ 
	basicBlock := aBasicBlock
]

{ #category : 'output' }
ASFlowNew >> getNameFor: aStackElement on: anOutput [

	^ outputName ifNil: [ outputName := anOutput generateTemp ]
]

{ #category : 'testing' }
ASFlowNew >> needsToBeSpilled [

	^ true
]

{ #category : 'printing' }
ASFlowNew >> printOn: aStream [

	aStream
		nextPutAll: 'new ';
		nextPutAll: (outputName ifNil: [ '' ]).
	value ifNotNil: [
		aStream
			space;
			print: value ].
	^ self printClassOn: aStream
]

{ #category : 'accessing' }
ASFlowNew >> value: anObject [

	self halt.
	value := anObject.
	self class: anObject flowClass
]
