"
I am the first block in a method.

All the other blocks that are created are ASCBasicBlock or ASCReturnBlock
"
Class {
	#name : #ASCMethodBlock,
	#superclass : #ASCBasicBlock,
	#instVars : [
		'primitive',
		'selector'
	],
	#category : 'ASTSmalltalk-CodeGen'
}

{ #category : #initialization }
ASCMethodBlock >> initialize [

	super initialize.
	self stack: ASEmptyStack new
]

{ #category : #printing }
ASCMethodBlock >> inspectZagASTOn: aStream [

	super inspectZagASTOn: aStream.
	aStream << $(.
	aStream print: selector.
	aStream << $)
]

{ #category : #accessing }
ASCMethodBlock >> locals: locals arguments: arguments [

	arguments reversed do: [ :each | self pushParameterNamed: each ].
	locals do: [ :each | self pushLocalNamed: each ].
	stack := stack pushPlaceholderContext.
	maxDepth := 0
]

{ #category : #writing }
ASCMethodBlock >> outputTo: output [

	output outputMethodStart: label.
	primitive
		ifNil: [ output verifySelector: selector ]
		ifNotNil: [ output primitive: primitive ].
	stack requiredContextDo: [ :context |
		output visitPushContext: context ].
	output outputMethodLabel: label.
	^ self outputRestTo: output
]

{ #category : #accessing }
ASCMethodBlock >> primitive: anInteger [

	primitive := anInteger.

]

{ #category : #accessing }
ASCMethodBlock >> pushSelfNamed: selfName [

	stack := self pushParameterNamed: selfName
]

{ #category : #accessing }
ASCMethodBlock >> selector: aString [ 
	selector := aString
]
