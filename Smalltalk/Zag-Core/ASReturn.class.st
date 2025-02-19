"
I am the only non-expression statement type
"
Class {
	#name : 'ASReturn',
	#superclass : 'ASStatement',
	#instVars : [
		'expression'
	],
	#category : 'Zag-Core-AST',
	#package : 'Zag-Core',
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
ASReturn >> isReturn [
	^ true
]

{ #category : 'accessing' }
ASReturn >> maxDepth [
	^expression maxDepth
]

{ #category : 'compiling' }
ASReturn >> newInlineBlock: aDispatchableBlock [
	^ aDispatchableBlock newInlineBlockNonLocalReturn
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
ASReturn >> zigWalkBlockFinal: aGenerator [

	"(expression zigWalkForNonLocal: aGenerator) ifFalse: ["
	(expression zigWalkForReturn: aGenerator) ifFalse: [
		^ aGenerator returnNonLocal ]
]

{ #category : 'compiling' }
ASReturn >> zigWalkDiscard: aGenerator [

	^ ASTInvalid signal: 'return can''t be in the middle of a body'
]

{ #category : 'compiling' }
ASReturn >> zigWalkFinal: aGenerator [

	(expression zigWalkForReturn: aGenerator) ifFalse: [
		^ aGenerator returnTop ]
]

{ #category : 'compiling' }
ASReturn >> zigWalkForReturn: aGenerator [

	^ ASTInvalid signal: 'return can''t be returned'
]

{ #category : 'compiling' }
ASReturn >> zigWalkTarget: aGenerator [

	^ ASTInvalid signal: 'return can''t be sent a message'
]
