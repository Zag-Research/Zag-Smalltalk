"
I reperesent a stack of values within a method.

Initially this will simply retain the values.

Eventually it will implement Phi nodes for SSA evaluation, supporting some level of type inference

Code generators can inquire as to values and types
"
Class {
	#name : #ASStack,
	#superclass : #Object,
	#instVars : [
		'top',
		'rest'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'instance creation' }
ASStack class >> from: anASStack [
	^ self basicNew from: anASStack 
]

{ #category : #accessing }
ASStack >> depth [
	^ rest ifNil: [ 0 ] ifNotNil: [ 1 + rest depth ]
]

{ #category : #accessing }
ASStack >> drop [
	^ rest
]

{ #category : #removing }
ASStack >> drop: count [ 
	count = 0 ifTrue: [ ^self ].
	^ rest drop: count-1
]

{ #category : #'instance creation' }
ASStack >> from: anASStack [

	rest := anASStack 
]

{ #category : #testing }
ASStack >> isEmpty [

	^ rest isNil 
]

{ #category : #adding }
ASStack >> push: anObject [

	| result |
	result := self class from: self.
	result top: anObject.
	^ result
]

{ #category : #accessing }
ASStack >> top [

	^ top
]

{ #category : #private }
ASStack >> top: anObject [

	top := anObject
]
