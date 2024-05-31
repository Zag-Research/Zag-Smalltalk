"
for inlining
"
Class {
	#name : 'ASCInlineBlock',
	#superclass : 'ASCBasicBlock',
	#instVars : [
		'sourceBlocks'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCInlineBlock >> addSource: aBasicBlock [

	self namedMethod == aBasicBlock namedMethod ifFalse: [
		| returnBlock |
		returnBlock := ASCReturnBlock for: method.
		returnBlock hash: hash.
		^ self becomeForward: returnBlock ].
	sourceBlocks add: aBasicBlock
]

{ #category : 'initialization' }
ASCInlineBlock >> initialize [

	super initialize.
	sourceBlocks := Set new
]

{ #category : 'initialization' }
ASCInlineBlock >> label [

	^ label ifNil: [ label := method nextSequence asString ]
]

{ #category : 'printing' }
ASCInlineBlock >> locals: locals arguments: arguments [

	arguments reversed do: [ :each | self pushParameterNamed: each ].
	locals do: [ :each | self pushLocalNamed: each ].
	stack := stack pushPlaceholderContext.
	maxDepth := 0
]

{ #category : 'adding' }
ASCInlineBlock >> nameStack: parameterNamesLeftToRight [

	| names |
	names := OrderedCollection with: #self.
	names addAll: parameterNamesLeftToRight.
	stack := stack nameStack: names
]

{ #category : 'as yet unclassified' }
ASCInlineBlock >> namedMethod [
	^ method namedMethod
]

{ #category : 'successor sequences' }
ASCInlineBlock >> nextSequence [

	^ method nextSequence
]

{ #category : 'writing' }
ASCInlineBlock >> outputTo: output [

	output outputMethodLabel: self label.
	^ self outputRestTo: output
]

{ #category : 'printing' }
ASCInlineBlock >> printOn: aStream [

	aStream nextPutAll: self label
]
