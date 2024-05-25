"
I am the first block in a method.

All the other blocks that are created are ASCBasicBlock or ASCReturnBlock
"
Class {
	#name : 'ASCMethodBlock',
	#superclass : 'ASCReturnBlock',
	#instVars : [
		'primitive',
		'returnBlocks'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCMethodBlock >> addReturnBlock: anASCReturnBlock [

	returnBlocks add: anASCReturnBlock
]

{ #category : 'initialization' }
ASCMethodBlock >> initialize [

	super initialize.
	returnBlocks := OrderedCollection new
]

{ #category : 'accessing' }
ASCMethodBlock >> locals: locals arguments: arguments [

	arguments reversed do: [ :each | self pushParameterNamed: each ].
	locals do: [ :each | self pushLocalNamed: each ].
	stack := stack pushPlaceholderContext.
	maxDepth := 0
]

{ #category : 'accessing' }
ASCMethodBlock >> makeBlockClosure [
	stack := ASStackBlockClosure from: stack
]

{ #category : 'as yet unclassified' }
ASCMethodBlock >> namedMethod [

	^ self
]

{ #category : 'writing' }
ASCMethodBlock >> outputTo: output [

	output
		outputMethodDefinition: self label
		primitive: primitive
		methods: { self } , returnBlocks.
	output outputMethodStart: label.
	stack requiredContextDo: [ output visitPushContext ].
	^ self outputRestTo: output
]

{ #category : 'accessing' }
ASCMethodBlock >> primitive: anInteger [

	primitive := anInteger.

]

{ #category : 'accessing' }
ASCMethodBlock >> pushSelfNamed: selfName [

	stack := self pushParameterNamed: selfName
]

{ #category : 'accessing' }
ASCMethodBlock >> selector: aString [

	selector := aString.
	label := aString
]
