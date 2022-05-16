"
I create an array from a sequence of expressions { axp1 . exp2 }
"
Class {
	#name : #ASArray,
	#superclass : #ASExpression,
	#instVars : [
		'statements'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #'instance creation' }
ASArray class >> statements: aCollection [ 
	^ self new statements: aCollection
]

{ #category : #accessing }
ASArray >> statements: aCollection [ 
	statements := aCollection
]
