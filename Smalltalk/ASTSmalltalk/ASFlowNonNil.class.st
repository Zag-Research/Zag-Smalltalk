"
I am an abstract flow source
"
Class {
	#name : 'ASFlowNonNil',
	#superclass : 'ASFlowNil',
	#instVars : [
		'id',
		'referenced'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'as yet unclassified' }
ASFlowNonNil >> getNameFor: aCollection on: anASLLMethodOutput [

	^ anASLLMethodOutput generateTemp
]

{ #category : 'output' }
ASFlowNonNil >> getTempNameFor: aCollection on: anASLLMethodOutput [

	^ anASLLMethodOutput generateTemp
]

{ #category : 'accessing' }
ASFlowNonNil >> id [

	^ id
]

{ #category : 'accessing' }
ASFlowNonNil >> id: aString [ 
	id := aString
]

{ #category : 'initialization' }
ASFlowNonNil >> initialize [

	super initialize.
	referenced := false
]

{ #category : 'testing' }
ASFlowNonNil >> isNilReference [

	^ false
]

{ #category : 'data flow instructions' }
ASFlowNonNil >> isReferenced [

	^ referenced
]

{ #category : 'data flow instructions' }
ASFlowNonNil >> markReferenced [
	" return true if we just made it true "

	referenced ifTrue: [ ^ false ].
	^ referenced := true
]

{ #category : 'printing' }
ASFlowNonNil >> printOn: aStream [

	^ aStream nextPutAll: '???'
]
