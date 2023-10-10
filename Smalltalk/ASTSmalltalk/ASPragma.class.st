Class {
	#name : #ASPragma,
	#superclass : #ASStatement,
	#traits : 'ASValue',
	#classTraits : 'ASValue classTrait',
	#instVars : [
		'args'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #'as yet unclassified' }
ASPragma class >> args: pramaArgs [
	^ASPragma new args: pramaArgs 
]

{ #category : #accessing }
ASPragma >> args: paramaArgs [
	args := paramaArgs 
]
