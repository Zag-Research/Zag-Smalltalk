"
I reperesent an element of a stack within a method.

Initially this will simply retain the values.

Eventually it will implement Phi nodes for SSA evaluation, supporting some level of type inference

Code generators can inquire as to values and types
"
Class {
	#name : 'ASStackTemporary',
	#superclass : 'ASStackElement',
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'accessing' }
ASStackTemporary >> at: nameOrIndex [
	0 = nameOrIndex ifTrue: [ ^ self ].
	nameOrIndex isInteger ifTrue: [ ^ next at: nameOrIndex - 1 ].
	^ next at: nameOrIndex
]

{ #category : 'displaying' }
ASStackTemporary >> displayStringOn: stream [

	self printNameOn: stream.
	stream
		nextPutAll: ' (';
		print: offset;
		nextPutAll: ') >';
		print: location
]

{ #category : 'accessing' }
ASStackTemporary >> drop [

	^ next
]

{ #category : 'removing' }
ASStackTemporary >> drop: count [ 
	count = 0 ifTrue: [ ^self ].
	^ next drop: count-1
]

{ #category : 'instance creation' }
ASStackTemporary >> fromStack: anASStack [

	next := anASStack
]

{ #category : 'accessing' }
ASStackTemporary >> id [

	^ offset
]

{ #category : 'API' }
ASStackTemporary >> intoContext: anInteger [

	^ next intoContext: anInteger
]

{ #category : 'as yet unclassified' }
ASStackTemporary >> makeManifestFor: anInteger [

	location markReferenced.
	anInteger <= 1 ifTrue: [ ^ self ].
	^ next makeManifestFor: anInteger - 1
]

{ #category : 'data flow instructions' }
ASStackTemporary >> markReferenced: n [

	n == 0 ifTrue: [ ^ self ].
	location markReferenced.
	^ next markReferenced: n - 1
]

{ #category : 'operations' }
ASStackTemporary >> mergeWith: aStackElement [

	location stackValue: (location stackValue mergeWith: aStackElement stackValue)
]

{ #category : 'adding' }
ASStackTemporary >> nameStack: parameterNamesLeftToRight offset: anInteger forBlock: basicBlock [

	| name |
	name := parameterNamesLeftToRight removeLast.
	^ (parameterNamesLeftToRight isEmpty
		   ifTrue: [ basicBlock selfElementClass fromStack: next ]
		   ifFalse: [
			   (ASParameterNamed fromStack: (next
					     nameStack: parameterNamesLeftToRight
					     offset: anInteger + 1
					     forBlock: basicBlock)) name: name ]) location: location
]

{ #category : 'as yet unclassified' }
ASStackTemporary >> offsetForTemps [

	^ offset
]

{ #category : 'displaying' }
ASStackTemporary >> printNameOn: stream [

	stream nextPutAll: 'temp'
]

{ #category : 'initialization' }
ASStackTemporary >> reset [

	^ next reset
]
