Class {
	#name : #ASSyntax,
	#superclass : #ASToken,
	#instVars : [
		'value'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #comparing }
ASSyntax >> = other [
	^ value = other value
]

{ #category : #accessing }
ASSyntax >> value [
	^ value
]

{ #category : #accessing }
ASSyntax >> value: aString [
	value := aString.

]
