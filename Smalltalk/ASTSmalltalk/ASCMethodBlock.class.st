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

{ #category : #accessing }
ASCMethodBlock >> children [

	| children |
	children :=  super children.
	children addAll: self blocks.
	^ children
]

{ #category : #initialization }
ASCMethodBlock >> initialize [

	super initialize.
	self stack: ASStack new
]

{ #category : #accessing }
ASCMethodBlock >> locals: locals arguments: arguments [
	stack isEmpty
		ifTrue: [ self pushUnknownNamed: #self ]
		ifFalse: [ stack := stack stackNames: #( self ) ].
	arguments reversed do: [ :each | self pushUnknownNamed: each ].
	locals do: [ :each | self pushUnknownNamed: each ].
	stack := stack pushPlaceholderContext
]

{ #category : #wriiting }
ASCMethodBlock >> outputTo: output [

	output outputMethodStart: label.
	primitive
		ifNil: [ output verifySelector: selector ]
		ifNotNil: [ output primitive: primitive ].
stack requiredContextDo: [ : context |
		                 output visitPushContext: context ].
	output outputMethodLabel: label.
	self outputRestTo: output
]

{ #category : #accessing }
ASCMethodBlock >> primitive: anInteger selector: aSymbol [

	primitive := anInteger.
	selector := aSymbol
]

{ #category : #accessing }
ASCMethodBlock >> selector: aString [ 
	selector := aString
]
