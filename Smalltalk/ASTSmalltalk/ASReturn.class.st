"
I am the only non-expression statement type
"
Class {
	#name : 'ASReturn',
	#superclass : 'ASStatement',
	#instVars : [
		'expression'
	],
	#category : 'ASTSmalltalk-AST',
	#package : 'ASTSmalltalk',
	#tag : 'AST'
}

{ #category : 'instance creation' }
ASReturn class >> expression: anASNode [
^self new expression: anASNode
]

{ #category : 'comparing' }
ASReturn >> = other [

	^ self class = other class and: [ self expression = other expression ]
]

{ #category : 'accessing' }
ASReturn >> blocks [
	^expression blocks
]

{ #category : 'accessing' }
ASReturn >> children [

	^ OrderedCollection with: expression
]

{ #category : 'accessing' }
ASReturn >> expression [

	^ expression
]

{ #category : 'accessing' }
ASReturn >> expression: anASNode [
	expression := anASNode
]

{ #category : 'testing' }
ASReturn >> isNonLocalThunk [
	^ expression isNonLocalThunkable
]

{ #category : 'testing' }
ASReturn >> isReturn [
	^ true
]

{ #category : 'accessing' }
ASReturn >> maxDepth [
	^expression maxDepth
]

{ #category : 'printing' }
ASReturn >> printOn: s [

	s << self className << ' expression: ('.
	expression printOn: s.
	s << ')'
]

{ #category : 'accessing' }
ASReturn >> tell: aCompiler youAreSent: aSymbol [

	^ expression tell: aCompiler youAreSent: aSymbol
]

{ #category : 'compiling' }
ASReturn >> zigWalk: aGenerator [

	^ ASTInvalid signal: 'return can only be at statement level'
]

{ #category : 'compiling' }
ASReturn >> zigWalkBlockReturn: anASCodeGenerator [

	(expression
		zigWalkForReturn: anASCodeGenerator)
		ifFalse: [ ^ anASCodeGenerator returnNonLocal ]
]

{ #category : 'compiling' }
ASReturn >> zigWalkDiscard: aGenerator [

	(expression zigWalkForReturn: aGenerator) ifFalse: [
	^ aGenerator returnTop]
]

{ #category : 'as yet unclassified' }
ASReturn >> zigWalkInlineBlockReturn: anASCodeGenerator [

	self shouldBeImplemented 
]

{ #category : 'compiling' }
ASReturn >> zigWalkInlineDiscard: aGenerator [

	^ (expression zigWalkForReturn: aGenerator) ifFalse: [
		aGenerator returnTopInline]
]

{ #category : 'compiling' }
ASReturn >> zigWalkInlineMethodReturn: aGenerator [ 
	^ self zigWalkInlineDiscard: aGenerator
]

{ #category : 'compiling' }
ASReturn >> zigWalkMethodReturn: aGenerator [ 
	^ self zigWalkDiscard: aGenerator
]
