"
I represent an inline choice.

I am only generated as part of inlining and compilation
"
Class {
	#name : #ASSwitch,
	#superclass : #ASExpression,
	#instVars : [
		'map'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #testing }
ASSwitch >> isClass [
	^ self subclassResponsibility
]
