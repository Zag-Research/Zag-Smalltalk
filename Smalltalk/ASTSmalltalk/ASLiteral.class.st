Class {
	#name : #ASLiteral,
	#superclass : #ASSimple,
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #parsing }
ASLiteral >> assignExpression: anASCompiler [ 
	^ self expression: anASCompiler
]
