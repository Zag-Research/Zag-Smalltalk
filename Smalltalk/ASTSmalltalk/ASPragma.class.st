"
I contain pragmas for a method - the only one we currently care about is <primitive: 1>
"
Class {
	#name : #ASPragma,
	#superclass : #ASStatement,
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
