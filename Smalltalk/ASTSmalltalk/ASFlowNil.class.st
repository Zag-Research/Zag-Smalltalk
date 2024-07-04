"
I am a flow source, but I'm `nil` right now.
I am the initial source for a local variable.
"
Class {
	#name : 'ASFlowNil',
	#superclass : 'Object',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'instance creation' }
ASFlowNil >> from [

	^ self
]

{ #category : 'as yet unclassified' }
ASFlowNil >> getNameFor: aCollection on: anASLLMethodOutput [

	^ anASLLMethodOutput generateNil
]

{ #category : 'as yet unclassified' }
ASFlowNil >> getTempNameFor: aCollection on: anASLLMethodOutput [

	^ anASLLMethodOutput generateNil
]

{ #category : 'accessing' }
ASFlowNil >> id [

	^ '@.nil'
]

{ #category : 'testing' }
ASFlowNil >> isFlowSource [

	^ true
]

{ #category : 'testing' }
ASFlowNil >> isNilReference [

	^ true
]

{ #category : 'testing' }
ASFlowNil >> isPhiReference [

	^ false
]

{ #category : 'data flow instructions' }
ASFlowNil >> isReferenced [

	^ false
]

{ #category : 'data flow instructions' }
ASFlowNil >> makeStackTheSource [

	^ ASFlowFromStack new from: self from
]

{ #category : 'data flow instructions' }
ASFlowNil >> markReferenced [

	^ false
]

{ #category : 'printing' }
ASFlowNil >> printOn: aStream [

	^ aStream nextPutAll: 'nil'
]
