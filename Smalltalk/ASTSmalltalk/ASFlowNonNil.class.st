"
I am an abstract flow source
"
Class {
	#name : 'ASFlowNonNil',
	#superclass : 'ASFlowNil',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'accessing' }
ASFlowNonNil class >> from: aFlow [

	^ self new from: aFlow
]

{ #category : 'accessing' }
ASFlowNonNil class >> value: anObject [

	^ self new value: anObject
]

{ #category : 'output' }
ASFlowNonNil >> getNameFor: aCollection on: anOutput [

	^ self subclassResponsibility
]

{ #category : 'testing' }
ASFlowNonNil >> isNilReference [

	^ false
]

{ #category : 'testing' }
ASFlowNonNil >> needsToBeSpilled [

	^ self subclassResponsibility
]

{ #category : 'printing' }
ASFlowNonNil >> printOn: aStream [

	^ self subclassResponsibility
]

{ #category : 'output' }
ASFlowNonNil >> pushFromContext: anInteger with: anASLLMethodOutput [ 

	^ anASLLMethodOutput emitPushFromContext: anInteger flow: self
]
