"
I am a named stack element (i.e. a local).
"
Class {
	#name : 'ASStackNamed',
	#superclass : 'ASStackElement',
	#instVars : [
		'name',
		'isInContext'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'enumerating' }
ASStackNamed >> addToCollection: aCollection [

	aCollection addLast: self.
	#self = name ifTrue: [^ aCollection].
	^ next addToCollection: aCollection

]

{ #category : 'accessing' }
ASStackNamed >> at: aSymbol [

	aSymbol = name ifTrue: [ ^ self ].
	^ next at: aSymbol
]

{ #category : 'displaying' }
ASStackNamed >> displayStringOn: stream [

	self printNameOn: stream.
	stream
		nextPutAll: ' (';
		print: offset;
		nextPutAll: (isInContext
				 ifTrue: [ 'C' ]
				 ifFalse: [ '' ]);
		nextPut: $);
		nextPutAll: ' >';
		print: location
]

{ #category : 'accessing' }
ASStackNamed >> id [

	^ name
]

{ #category : 'adding' }
ASStackNamed >> importedSource: anInteger [

]

{ #category : 'initialization' }
ASStackNamed >> initialize [

	super initialize.
	isInContext := false
]

{ #category : 'API' }
ASStackNamed >> intoContext: anInteger [

	isInContext := true.
	offset := anInteger.
	^ next intoContext: anInteger + 1
]

{ #category : 'testing' }
ASStackNamed >> isInContext [

	^ isInContext
]

{ #category : 'operations' }
ASStackNamed >> mergeWith: aCollection [
]

{ #category : 'accessing' }
ASStackNamed >> name [
	^ name
]

{ #category : 'accessing' }
ASStackNamed >> name: aSymbol [
	name := aSymbol
]

{ #category : 'as yet unclassified' }
ASStackNamed >> offsetForTemps [

	isInContext ifTrue: [ ^ 0 ].
	^ offset
]

{ #category : 'displaying' }
ASStackNamed >> printNameOn: stream [

	stream nextPutAll: name
]

{ #category : 'output' }
ASStackNamed >> pushTheVariable: anASLLMethodOutput [

	isInContext ifTrue: [^ location pushFromContext: offset with: anASLLMethodOutput].
	^ location pushFromStack: offset with: anASLLMethodOutput
]

{ #category : 'API' }
ASStackNamed >> replicate: aStack [

	^ (super replicate: aStack) name: name
]
