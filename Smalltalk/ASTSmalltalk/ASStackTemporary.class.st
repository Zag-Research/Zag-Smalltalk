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

{ #category : 'displaying' }
ASStackTemporary >> displayStringOn: stream [

	self printNameOn: stream.
	stream
		nextPutAll: ' (';
		print: offset;
		nextPutAll: ') = ';
		print: value;
		nextPutAll: ' >';
		print: source
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

{ #category : 'rewriting' }
ASStackTemporary >> dropUnder: n [

	^ self replicate: (rest drop: n)
]

{ #category : 'accessing' }
ASStackTemporary >> id [

	^ offset
]

{ #category : 'API' }
ASStackTemporary >> makeContextRequired: aBlock [

	^ self replicate: (rest makeContextRequired: aBlock)
]

{ #category : 'data flow instructions' }
ASStackTemporary >> makeStackTheSource [

	source := source makeStackTheSource.
	^ rest makeStackTheSource
]

{ #category : 'data flow instructions' }
ASStackTemporary >> markReferenced [

	^ source markReferenced
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
		   ifTrue: [ basicBlock selfElementClass fromStack: rest ]
		   ifFalse: [
			   (ASParameterNamed fromStack: (rest
						     nameStack: parameterNamesLeftToRight
						     offset: anInteger + 1
						     forBlock: basicBlock)) name: name ])
		  value: value;
		  source: source
]

{ #category : 'as yet unclassified' }
ASStackTemporary >> namedContext: contextBlock stack: stackBlock other: otherBlock [

	source use: stackBlock offset: offset name: 'temporary'
]

{ #category : 'data flow instructions' }
ASStackTemporary >> newElementFor: aBasicBlock [

	| new |
	new := aBasicBlock classForTemporaries new id: self id.
	^ (self replicate: (rest newElementFor: aBasicBlock)) source: new
]

{ #category : 'API' }
ASStackTemporary >> replicate: aStack [

	^ (self class fromStack: aStack)
		  value: value;
		  source: source
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

	anObject isFlowSource ifFalse: [ ^ self error: 'must be source' ].
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
