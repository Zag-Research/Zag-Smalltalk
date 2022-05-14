"
I represent an assignment expression. This stores the value of an expression in a variable (local, inherited local, instance, class, shared).
"
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
