Class {
	#name : #ASSimple,
	#superclass : #ASToken,
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #parsing }
ASSimple >> binaryExpression: anASCompiler [ 
	| target |
	target := anASCompiler nextToken.
	^ anASCompiler peek binaryExpression: anASCompiler target: target
]

{ #category : #parsing }
ASSimple >> keywordExpression: anASCompiler [ 
	| target |
	target := anASCompiler nextToken.
	^ anASCompiler peek keywordExpression: anASCompiler target: target
]

{ #category : #parsing }
ASSimple >> unaryExpression: anASCompiler [ 
	| target |
	target := anASCompiler nextToken.
	^ anASCompiler peek unaryExpression: anASCompiler target: target
]
