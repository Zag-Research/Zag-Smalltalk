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
		'consumers'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'accessing' }
AStackLocation >> creator: anASCLiteral [

	creator := anASCLiteral
]

{ #category : 'accessing' }
AStackLocation >> dataClass [

	^ stackValueOrFlow dataClass
]

{ #category : 'data flow instructions' }
AStackLocation >> markReferenced [

	stackValueOrFlow markReferenced
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
