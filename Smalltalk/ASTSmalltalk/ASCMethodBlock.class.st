"
I am the first block in a method.

All the other blocks that are created are ASCBasicBlock or ASCReturnBlock
"
Class {
	#name : 'ASCMethodBlock',
	#superclass : 'ASCBasicBlock',
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

{ #category : 'writing' }
ASCMethodBlock >> outputTo: output [
	output outputMethodStart: label.
	primitive
		ifNotNil: [ output primitive: primitive ].
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
ASCMethodBlock >> selector: aString [

	selector := label := aString
]
