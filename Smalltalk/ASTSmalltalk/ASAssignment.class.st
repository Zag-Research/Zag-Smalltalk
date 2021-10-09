Class {
	#name : #ASAssignment,
	#superclass : #Object,
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
