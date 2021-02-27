Class {
	#name : #ASLiteral,
	#superclass : #ASToken,
	#instVars : [
		'value'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #comparing }
ASLiteral >> = other [
	^ value = other value
]

{ #category : #accessing }
ASLiteral >> value [
	^ value
]

{ #category : #accessing }
ASLiteral >> value: aString [
	value := aString.

]
