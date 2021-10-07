Class {
	#name : #ASLiteral,
	#superclass : #ASSimple,
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #parsing }
ASLiteral >> assignmentExpression: anASCompiler [ 
	| target |
	target := anASCompiler nextToken.
	^ anASCompiler peek keywordExpression: anASCompiler target: target
]
