Class {
	#name : 'ASOpenParenToken',
	#superclass : 'ASSimple',
	#category : 'ASTSmalltalk-Tokens',
	#package : 'ASTSmalltalk',
	#tag : 'Tokens'
}

{ #category : 'parsing' }
ASOpenParenToken >> parseWith: anASCompiler [
	| result |
	anASCompiler nextToken. " discard ("
	result := anASCompiler peek assignExpression: anASCompiler.
	anASCompiler peek expectCloseParen.
	^ result
]
