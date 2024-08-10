"
I represent a location of a value pushed on the stack, either as the result of a message send, or a push operation.

I know nothing about the name of the location (captured by the ASStack subclasses).
I also know nothing about the value that may be stored in the location (captured by the `stackValue` field).
"
Class {
	#name : 'AStackLocation',
	#superclass : 'Object',
	#instVars : [
		'stackValue',
		'isReferenced'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'accessing' }
AStackLocation >> dataClass [

	^ stackValue dataClass
]

{ #category : 'data flow instructions' }
AStackLocation >> markReferenced [

	stackValue markReferenced
]

{ #category : 'accessing' }
AStackLocation >> stackValue [

	^ stackValue
]

{ #category : 'accessing' }
AStackLocation >> stackValue: aStackValue [

	stackValue := aStackValue
]
