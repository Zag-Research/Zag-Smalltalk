"
I am an abstract flow source
"
Class {
	#name : 'ASFlowNonNil',
	#superclass : 'ASFlowNil',
	#instVars : [
		'referenced',
		'value',
		'class'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'accessing' }
ASFlowNonNil class >> value: anObject [

	^ self new value: anObject
]

{ #category : 'accessing' }
ASFlowNonNil >> class: aClass [ 
	class := aClass
]

{ #category : 'output' }
ASFlowNonNil >> getNameFor: aCollection on: anOutput [

	^ self subclassResponsibility
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

{ #category : 'testing' }
ASFlowNonNil >> needsToBeSpilled [

	^ self subclassResponsibility
]

{ #category : 'printing' }
ASFlowNonNil >> printOn: aStream [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ASFlowNonNil >> value [

	^ value
]

{ #category : 'accessing' }
ASFlowNonNil >> value: anObject [

	value := anObject.
	class := anObject class
]

{ #category : 'accessing' }
ASFlowNonNil >> valueClass [

	^ value ifNotNil: [value valueClass]
]
