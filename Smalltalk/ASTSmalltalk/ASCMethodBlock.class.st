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
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #'instance creation' }
ASCMethodBlock class >> nonlocalNameLookupObject: aGenerator [
	^ self new nonlocalNameLookupObject: aGenerator 
]

{ #category : #accessing }
ASCMethodBlock >> children [

	| children |
	children := super children.
	children addAll: self blocks.
	^ children
]

{ #category : #initialization }
ASCMethodBlock >> initialize [

	super initialize.
	self stack: ASEmptyStack new 
]

{ #category : #accessing }
ASCMethodBlock >> locals: locals arguments: arguments [

	stack isEmpty
		ifTrue: [ self pushParameterNamed: #self ]
		ifFalse: [ stack := stack stackNames: #( self ) ].
	arguments reversed do: [ :each | self pushParameterNamed: each ].
	locals do: [ :each | self pushLocalNamed: each ].
	stack := stack pushPlaceholderContext.
	maxDepth := 0
]

{ #category : #'instance initialization' }
ASCMethodBlock >> nonlocalNameLookupObject: aGenerator [

	stack := ASEmptyStack new nonlocalNameLookupObject: aGenerator 
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
ASCMethodBlock >> selector: aString [ 
	selector := aString
]
