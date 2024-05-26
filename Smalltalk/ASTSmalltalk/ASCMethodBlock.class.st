"
I am the first block in a method.

All the other blocks that are created are ASCBasicBlock or ASCReturnBlock
"
Class {
	#name : 'ASCMethodBlock',
	#superclass : 'ASCReturnBlock',
	#instVars : [
		'primitive',
		'selector'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

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

	output outputMethodDefinition: self label primitive: primitive.
	stack requiredContextDo: [ output visitPushContext ].
	output outputMethodLabel: label.
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
ASCMethodBlock >> selector [
	^ selector
]

{ #category : 'accessing' }
ASCMethodBlock >> selector: aString [

	selector := aString.
	label := aString.
	sequence := 0
]
