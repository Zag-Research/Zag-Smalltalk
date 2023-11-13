"
for inlining
"
Class {
	#name : #ASCInlineBlock,
	#superclass : #ASCBasicBlock,
	#instVars : [
		'primitive',
		'selector'
	],
	#category : #'ASTSmalltalk-CodeGen'
}

{ #category : #accessing }
ASCInlineBlock >> locals: locals arguments: arguments [

	stack isEmpty
		ifTrue: [ self pushUnknownNamed: #self ]
		ifFalse: [ stack := stack names: #( self ) ].
			arguments reversed do: [ :each | self pushUnknownNamed: each ].
	locals do: [ :each | self pushUnknownNamed: each ]
]

{ #category : #writing }
ASCInlineBlock >> outputTo: output [

	output outputMethodStart: label.
	primitive
		ifNil: [ output verifySelector: selector ]
		ifNotNil: [ output primitive: primitive ].
	output outputMethodLabel: label.
	^ self outputRestTo: output
]

{ #category : #accessing }
ASCInlineBlock >> primitive: anInteger selector: aSymbol [

	primitive := anInteger.
	selector := aSymbol
]

{ #category : #accessing }
ASCInlineBlock >> selector: aString [ 
	selector := aString
]
