"
I am a named stack element (i.e. a local).
"
Class {
	#name : 'ASStackNamed',
	#superclass : 'ASStackTemporary',
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
	^ rest at: aSymbol
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
	rest intoContext: anInteger + 1
]

{ #category : 'testing' }
ASStackNamed >> isInContext [

	^ isInContext
]

{ #category : 'API' }
ASStackNamed >> makeContextRequired: aBlock [

	| context |
	context := ASStackContext fromStack: (self intoContext: 0).
	aBlock cull: context.
	^ context
]

{ #category : 'accessing' }
ASStackNamed >> name [
	^ name
]

{ #category : 'accessing' }
ASStackNamed >> name: aSymbol [
	name := aSymbol
]

{ #category : 'data flow instructions' }
ASStackNamed >> sourceFor: anASCInlineBlock [ 
	"nothing to do - use the global named source"
]
