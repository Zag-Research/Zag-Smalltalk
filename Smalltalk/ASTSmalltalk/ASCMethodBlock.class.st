"
I am the first block in a method.

All the other blocks that are created are ASCBasicBlock or ASCReturnBlock
"
Class {
	#name : 'ASCMethodBlock',
	#superclass : 'ASCReturnBlock',
	#instVars : [
		'primitive',
		'returnBlocks',
		'parameters'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCMethodBlock >> addReturnBlock: anASCReturnBlock [

	^ returnBlocks
		  at: anASCReturnBlock
		  ifAbsentPut: [ returnBlocks size + 1 ]
]

{ #category : 'initialization' }
ASCMethodBlock >> initialize [

	super initialize.
	returnBlocks := Dictionary new.
	method := self.
	parameters := Set with: #self
]

{ #category : 'accessing' }
ASCMethodBlock >> locals: locals parameters: arguments [

	arguments reversed do: [ :each | self pushParameterNamed: each ].
	parameters addAll: arguments.
	locals do: [ :each | self pushLocalNamed: each ].
	maxDepth := 0
]

{ #category : 'accessing' }
ASCMethodBlock >> makeBlockClosure [
	stack := ASStackBlockClosure from: stack
]

{ #category : 'accessing' }
ASCMethodBlock >> namedMethod [

	^ self
]

{ #category : 'writing' }
ASCMethodBlock >> outputTo: output [

	output
		outputMethodDefinition: self label
		primitive: primitive
		methods: { self } , returnBlocks keys.
	output outputMethodStart: label.
	stack requiredContextDo: [ output visitPushContext ].
	^ self outputRestTo: output
]

{ #category : 'accessing' }
ASCMethodBlock >> primitive: anInteger [

	primitive := anInteger.

]

{ #category : 'accessing' }
ASCMethodBlock >> provides: aSymbol [

	^ (provides includes: aSymbol) or: [ parameters includes: aSymbol ]
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
