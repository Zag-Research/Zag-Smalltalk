Class {
	#name : 'ASOpenParenToken',
	#superclass : 'ASSimple',
	#category : 'Zag-Core-Tokens',
	#package : 'Zag-Core',
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
