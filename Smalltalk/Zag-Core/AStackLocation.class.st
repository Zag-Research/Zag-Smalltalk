"
I represent a location of a value pushed on the stack, either as the result of a message send, or a push operation.

I know nothing about the name of the location (captured by the ASStack subclasses).
I also know nothing about the value that may be stored in the location (captured by the `stackValue` field).
"
Class {
	#name : 'AStackLocation',
	#superclass : 'Object',
	#instVars : [
		'creator',
		'stackValueOrFlow',
		'consumers',
		'flowValue'
	],
	#category : 'Zag-Core-Stack',
	#package : 'Zag-Core',
	#tag : 'Stack'
}

{ #category : 'accessing' }
AStackLocation >> consumer: anOperation [

	^ consumers add: anOperation
]

{ #category : 'accessing' }
AStackLocation >> consumers [

	^ consumers
]

{ #category : 'accessing' }
AStackLocation >> creator: anOperation [

	creator := anOperation
]

{ #category : 'accessing' }
AStackLocation >> dataClass [

	^ stackValueOrFlow dataClass
]

{ #category : 'flow analysis' }
AStackLocation >> flowValue [

	^ flowValue
]

{ #category : 'accessing' }
AStackLocation >> flowValue: aFlow [

	flowValue := aFlow
]

{ #category : 'initialization' }
AStackLocation >> initialize [

	super initialize.
	consumers := OrderedCollection new
]

{ #category : 'data flow instructions' }
AStackLocation >> markReferenced [

	^ stackValueOrFlow markReferenced
]

{ #category : 'printing' }
AStackLocation >> printOn: stream [

	stream nextPutAll: flowValue asString.
]

{ #category : 'accessing' }
AStackLocation >> pushFromStack: anInteger with: anOutput [

		^ anOutput visitPushFromStack: anInteger
]

{ #category : 'accessing' }
AStackLocation >> stackValue [

	^ stackValueOrFlow
]

{ #category : 'accessing' }
AStackLocation >> stackValue: aStackValue [

	stackValueOrFlow := aStackValue
]
