"
I reperesent an element of a stack within a method.

Initially this will simply retain the values.

Eventually it will implement Phi nodes for SSA evaluation, supporting some level of type inference

Code generators can inquire as to values and types
"
Class {
	#name : 'ASStackTemporary',
	#superclass : 'ASStackNonContext',
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
		nextPutAll: ' >';
		print: source
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

{ #category : 'rewriting' }
ASStackTemporary >> dropUnder: n [

	^ self replicate: (next drop: n)
]

{ #category : 'as yet unclassified' }
ASStackTemporary >> getNameOn: anASLLMethodOutput [

	^ source idFor: self on: anASLLMethodOutput
]

{ #category : 'accessing' }
ASStackTemporary >> id [

	^ offset
]

{ #category : 'API' }
ASStackTemporary >> makeContextRequired: aBlock [

	^ self replicate: (next makeContextRequired: aBlock)
]

{ #category : 'as yet unclassified' }
ASStackTemporary >> makeManifestFor: anInteger [

	source markReferenced.
	anInteger <= 1 ifTrue: [ ^ self ].
	^ next makeManifestFor: anInteger - 1
]

{ #category : 'data flow instructions' }
ASStackTemporary >> makeStackTheSource [

	source := ASFlowOnDemand new.
	^ next makeStackTheSource
]

{ #category : 'data flow instructions' }
ASStackTemporary >> markReferenced [

	^ source markReferenced
]

{ #category : 'data flow instructions' }
ASStackTemporary >> markReferenced: n [

	n == 0 ifTrue: [ ^ self ].
	source markReferenced.
	^ next markReferenced: n - 1
]

{ #category : 'operations' }
ASStackTemporary >> mergeWith: aCollection [

	source := source mergeWith: aCollection source
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
						     forBlock: basicBlock)) name: name ])
		  source: source
]

{ #category : 'as yet unclassified' }
ASStackTemporary >> namedContext: contextBlock stack: stackBlock other: otherBlock [

	source use: stackBlock offset: offset name: 'temporary'
]

{ #category : 'data flow instructions' }
ASStackTemporary >> newElementFor: aBasicBlock [

	| new |
	new := aBasicBlock classForTemporaries new.
	^ (self replicate: (next newElementFor: aBasicBlock)) source: new
]

{ #category : 'API' }
ASStackTemporary >> replicate: aStack [

	^ (self class fromStack: aStack) source: source
]

{ #category : 'initialization' }
ASStackTemporary >> reset [

	^ next reset
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
