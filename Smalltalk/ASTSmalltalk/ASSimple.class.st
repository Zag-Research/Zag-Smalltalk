Class {
	#name : #ASSimple,
	#superclass : #ASToken,
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #parsing }
ASSimple >> expression: anASCompiler [ 
	| target |
	target := anASCompiler nextToken.
	^ anASCompiler peek keywordExpression: anASCompiler target: target
]
