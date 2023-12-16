"
I contain pragmas for a method - the only one we currently care about is <primitive: 1>
"
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

{ #category : 'printing' }
ASPragma >> inspectZagASTOn: aStream [
	super inspectZagASTOn: aStream.
	aStream nextPut: $(.
	args do: [ :arg | aStream print: arg literal ].
	aStream nextPut: $)
]
