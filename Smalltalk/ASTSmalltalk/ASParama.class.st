Class {
	#name : #ASParama,
	#superclass : #ASStatement,
	#traits : 'ASValue',
	#classTraits : 'ASValue classTrait',
	#instVars : [
		'args'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #'as yet unclassified' }
ASParama class >> args: pramaArgs [
	^ASParama new args: pramaArgs 
]

{ #category : #accessing }
ASParama >> args: paramaArgs [
	args := paramaArgs 
]
