"
I am a named stack element (i.e. a local).
"
Class {
	#name : 'ASStackNamed',
	#superclass : 'ASStackNonContext',
	#instVars : [
		'name',
		'isInContext'
	],
	#category : 'ASTSmalltalk-Stack',
	#package : 'ASTSmalltalk',
	#tag : 'Stack'
}

{ #category : 'accessing' }
ASStackNamed >> at: aSymbol [

	aSymbol = name ifTrue: [ ^ self ].
	^ next at: aSymbol
]

{ #category : 'displaying' }
ASStackNamed >> displayStringOn: stream [

	self printNameOn: stream.
	stream
		nextPut: Character space;
		nextPutAll: name;
		nextPutAll: ' (';
		print: offset;
		nextPutAll: (isInContext
				 ifTrue: [ 'C' ]
				 ifFalse: [ '' ]);
		nextPut: $);
		nextPutAll: ' >';
		print: source
]

{ #category : 'as yet unclassified' }
ASStackNamed >> getNameOn: anASLLMethodOutput [

	^ source getNameFor: self on: anASLLMethodOutput
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

{ #category : 'API' }
ASStackNamed >> makeContextRequired: aBlock [

	| context |
	self intoContext: 0.
	context := ASStackContext fromStack: self.
	aBlock cull: context.
	^ context
]

{ #category : 'data flow instructions' }
ASStackNamed >> makeStackTheSource [

	source := ASFlowOnDemand new.
	^ next makeStackTheSource
]

{ #category : 'data flow instructions' }
ASStackNamed >> markReferenced [

	^ source markReferenced
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
ASStackNamed >> namedContext: contextBlock stack: stackBlock other: otherBlock [

	isInContext
		ifTrue: [ source use: contextBlock offset: offset name: name ]
		ifFalse: [ source use: stackBlock offset: offset name: name ]
]

{ #category : 'data flow instructions' }
ASStackNamed >> newElementFor: aBasicBlock [
	"nothing to do - use the global named source"

]

{ #category : 'API' }
ASStackNamed >> replicate: aStack [

	^ (self class fromStack: aStack)
		  name: name;
		  source: source
]

{ #category : 'accessing' }
ASStackNamed >> source [
	^ source
]

{ #category : 'accessing' }
ASStackNamed >> source: anASFlowNew [ 
	source := anASFlowNew
]
