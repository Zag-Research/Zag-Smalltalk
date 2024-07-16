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
		nextPutAll: ') >';
		print: data
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

{ #category : 'accessing' }
ASStackTemporary >> id [

	^ offset
]

{ #category : 'as yet unclassified' }
ASStackTemporary >> makeManifestFor: anInteger [

	data markReferenced.
	anInteger <= 1 ifTrue: [ ^ self ].
	^ next makeManifestFor: anInteger - 1
]

{ #category : 'data flow instructions' }
ASStackTemporary >> markReferenced [

	^ data markReferenced
]

{ #category : 'data flow instructions' }
ASStackTemporary >> markReferenced: n [

	n == 0 ifTrue: [ ^ self ].
	data markReferenced.
	^ next markReferenced: n - 1
]

{ #category : 'operations' }
ASStackTemporary >> mergeWith: aStackElement [

	data := data mergeWith: aStackElement data
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
					     forBlock: basicBlock)) name: name ]) data: data
]

{ #category : 'data flow instructions' }
ASStackTemporary >> newElementFor: aBasicBlock [

	^ (self replicate: (next newElementFor: aBasicBlock)) data:
		  (aBasicBlock flowForTemporaries new basicBlock: aBasicBlock)
]

{ #category : 'displaying' }
ASStackTemporary >> printNameOn: stream [

	stream nextPutAll: 'temp'
]

{ #category : 'initialization' }
ASStackTemporary >> reset [

	^ next reset
]
