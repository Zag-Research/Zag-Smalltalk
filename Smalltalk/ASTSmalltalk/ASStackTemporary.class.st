"
I reperesent an element of a stack within a method.

Initially this will simply retain the values.

Eventually it will implement Phi nodes for SSA evaluation, supporting some level of type inference

Code generators can inquire as to values and types
"
Class {
	#name : 'ASStackTemporary',
	#superclass : 'ASStackNonEmpty',
	#instVars : [
		'source',
		'value'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'accessing' }
ASStackTemporary >> at: nameOrIndex [
	0 = nameOrIndex ifTrue: [ ^ self ].
	nameOrIndex isInteger ifTrue: [ ^ rest at: nameOrIndex - 1 ].
	^ rest at: nameOrIndex
]

{ #category : 'accessing' }
ASStackTemporary >> drop [

	^ rest
]

{ #category : 'removing' }
ASStackTemporary >> drop: count [ 
	count = 0 ifTrue: [ ^self ].
	^ rest drop: count-1
]

{ #category : 'accessing' }
ASStackTemporary >> id [

	^ offset
]

{ #category : 'API' }
ASStackTemporary >> makeContextRequired: aBlock [

	^ (self class fromStack: (rest makeContextRequired: aBlock))
		  value: value;
		  source: source
]

{ #category : 'data flow instructions' }
ASStackTemporary >> markReferenced: n [

	n == 0 ifTrue: [ ^ self ].
	source markReferenced.
	^ rest markReferenced: n - 1
]

{ #category : 'adding' }
ASStackTemporary >> nameStack: parameterNamesLeftToRight offset: anInteger forBlock: basicBlock [

	| name |
	name := parameterNamesLeftToRight removeLast.
	^ (parameterNamesLeftToRight isEmpty
		   ifTrue: [ (ASStackNamed fromStack: rest) name: #self]
		   ifFalse: [
			   (ASStackNamed fromStack: (rest
						     nameStack: parameterNamesLeftToRight
						     offset: anInteger + 1
						     forBlock: basicBlock)) name: name ])
		  value: value;
		  source: source
]

{ #category : 'data flow instructions' }
ASStackTemporary >> newElementFor: aBasicBlock [

	| new |
	new := aBasicBlock classForTemporaries new id: self id.
	aBasicBlock addSource: new.
	^ (self class fromStack: (rest newElementFor: aBasicBlock))
		  value: value;
		  source: new
]

{ #category : 'accessing' }
ASStackTemporary >> offset [

	^ offset
]

{ #category : 'initialization' }
ASStackTemporary >> reset [

	^ rest reset
]

{ #category : 'accessing' }
ASStackTemporary >> source [

	^ source
]

{ #category : 'accessing' }
ASStackTemporary >> source: anObject [

	source := anObject
]

{ #category : 'evaluating' }
ASStackTemporary >> value [

	^ value
]

{ #category : 'accessing' }
ASStackTemporary >> value: v [

	value := v
]
