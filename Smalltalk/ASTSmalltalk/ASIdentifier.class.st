Class {
	#name : #ASIdentifier,
	#superclass : #Object,
	#instVars : [
		'value'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #comparing }
ASIdentifier >> = other [
	^ value = other value
]

{ #category : #accessing }
ASIdentifier >> value [
	^ value
]

{ #category : #accessing }
ASIdentifier >> value: aString [
	value := aString.

]
