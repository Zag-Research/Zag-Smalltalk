"
I reperesent an element of a stack within a method.

Initially this will simply retain the values.

Eventually it will implement Phi nodes for SSA evaluation, supporting some level of type inference

Code generators can inquire as to values and types
"
Class {
	#name : 'ASStackPlain',
	#superclass : 'ASStackNonEmpty',
	#instVars : [
		'source',
		'value'
	],
	#classVars : [
		'LLVMNumber'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'initialization' }
ASStackPlain class >> reset [
	LLVMNumber := 0
]

{ #category : 'accessing' }
ASStackPlain >> at: nameOrIndex [
	0 = nameOrIndex ifTrue: [ ^ self ].
	nameOrIndex isInteger ifTrue: [ ^ rest at: nameOrIndex - 1 ].
	^ rest at: nameOrIndex
]

{ #category : 'accessing' }
ASStackPlain >> drop [

	^ rest
]

{ #category : 'removing' }
ASStackPlain >> drop: count [ 
	count = 0 ifTrue: [ ^self ].
	^ rest drop: count-1
]

{ #category : 'API' }
ASStackPlain >> makeContextRequired: aBlock [

	^ (self class fromStack: (rest makeContextRequired: aBlock))
		  value: value;
		  source: source
]

{ #category : 'adding' }
ASStackPlain >> nameStack: parameterNamesLeftToRight offset: anInteger forBlock: basicBlock [

	| name |
	name := parameterNamesLeftToRight removeLast.
	^ (parameterNamesLeftToRight isEmpty
		   ifTrue: [ ASStackSelf fromStack: rest ]
		   ifFalse: [
			   (ASStackNamed fromStack: (rest
						     nameStack: parameterNamesLeftToRight
						     offset: anInteger + 1
						     forBlock: basicBlock)) name: name ])
		  value: value;
		  source: source
]

{ #category : 'accessing' }
ASStackPlain >> offset [

	^ offset
]

{ #category : 'initialization' }
ASStackPlain >> reset [

	^ rest reset
]

{ #category : 'accessing' }
ASStackPlain >> source [

	^ source
]

{ #category : 'accessing' }
ASStackPlain >> source: anObject [

	source := anObject
]

{ #category : 'accessing' }
ASStackPlain >> value [

	^ value
]

{ #category : 'accessing' }
ASStackPlain >> value: v [

	value := v
]
