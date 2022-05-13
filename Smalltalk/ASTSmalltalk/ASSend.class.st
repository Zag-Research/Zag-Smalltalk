Class {
	#name : #ASSend,
	#superclass : #ASExpression,
	#instVars : [
		'target',
		'selector',
		'args'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #parsing }
ASSend >> target: token1 selector: token2 args: aCollection [ 
	target := token1.
	selector := token2.
	args := aCollection.
	
]
