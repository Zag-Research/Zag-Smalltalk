Class {
	#name : 'ASPragma',
	#superclass : 'Object',
	#instVars : [
		'args'
	],
	#category : 'ASTSmalltalk-AST',
	#package : 'ASTSmalltalk',
	#tag : 'AST'
}

{ #category : 'as yet unclassified' }
ASPragma class >> args: pramaArgs [
	^ASPragma new args: pramaArgs 
]

{ #category : 'accessing' }
ASPragma >> args: paramaArgs [
	args := paramaArgs 
]
