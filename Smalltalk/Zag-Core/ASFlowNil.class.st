"
I am a flow source, but I'm `nil` right now.
I am the initial source for a local variable.
"
Class {
	#name : 'ASFlowNil',
	#superclass : 'Object',
	#instVars : [
		'outputName'
	],
	#classInstVars : [
		'singleton'
	],
	#category : 'Zag-Core-Stack',
	#package : 'Zag-Core',
	#tag : 'Stack'
}

{ #category : 'printing' }
ASFlowNil class >> canonicalArgumentName [

	^ 'aFlow'
]

{ #category : 'instance creation' }
ASFlowNil class >> singleton [

	^ singleton ifNil: [ singleton := self new ]
]

{ #category : 'accessing' }
ASFlowNil class >> value: ignored [

	^ self new
]

{ #category : 'accessing' }
ASFlowNil >> basicBlock: aBasicBlock [
]

{ #category : 'instance creation' }
ASFlowNil >> class: ignored [
]

{ #category : 'output' }
ASFlowNil >> getNameFor: aCollection on: anOutput [

	^ outputName ifNil: [ outputName := anOutput generateNil ]
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
ASFlowNil >> markReferenced [

	^ false
]

{ #category : 'testing' }
ASFlowNil >> needsToBeSpilled [

	^ false
]

{ #category : 'printing' }
ASFlowNil >> printOn: aStream [

	^ aStream nextPutAll: 'nil'
]

{ #category : 'accessing' }
ASFlowNil >> value: anObject [

self halt.
	^ ASFlowNew value: anObject
]
