Class {
	#name : 'ASCInlineMethodBlock',
	#superclass : 'ASCMethodBlock',
	#instVars : [
		'numArgs',
		'returnBlock'
	],
	#category : 'ASTSmalltalk-CodeGen',
	#package : 'ASTSmalltalk',
	#tag : 'CodeGen'
}

{ #category : 'adding' }
ASCInlineMethodBlock >> nameStack: parameterNamesLeftToRight [

	| names |
	names := OrderedCollection with: #self.
	names addAll: parameterNamesLeftToRight.
	stack := stack nameStack: names offset: 0 forBlock: self
]

{ #category : 'API' }
ASCInlineMethodBlock >> numArgs: anInteger [

	numArgs := anInteger
]

{ #category : 'as yet unclassified' }
ASCInlineMethodBlock >> returnBlock: aBasicBlock [

	returnBlock := aBasicBlock
]

{ #category : 'API' }
ASCInlineMethodBlock >> selfElementClass [

	^ ASStackSelf
]
