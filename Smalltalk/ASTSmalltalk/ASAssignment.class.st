Class {
	#name : #ASAssignment,
	#superclass : #ASExpression,
	#instVars : [
		'target',
		'value'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #accessing }
ASAssignment >> target: t value: v [ 
	target := t.
	value := v.

]
