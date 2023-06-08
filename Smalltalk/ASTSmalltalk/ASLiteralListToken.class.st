Class {
	#name : #ASLiteralListToken,
	#superclass : #ASSimple,
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #parsing }
ASLiteralListToken >> parseWith: anASCompiler [
		| statements |
	statements := OrderedCollection new.
	anASCompiler peek value = $( | anASCompiler peek value = '#(' ifTrue: [anASCompiler nextToken]. " discard #("
	[anASCompiler peek value = $) ] whileFalse: [ "would like to use ASCloseParenToken"
		statements add: (anASCompiler nextToken parseWith: anASCompiler).
	].
   anASCompiler nextToken.  "discard #)"
	^ ASArray statements: statements.
]
