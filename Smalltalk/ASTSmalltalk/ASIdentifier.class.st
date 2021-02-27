Class {
	#name : #ASIdentifier,
	#superclass : #ASToken,
	#instVars : [
		'value'
	],
	#category : #'ASTSmalltalk-AST'
}

{ #category : #comparing }
ASIdentifier >> = other [
	^ value = other value
]

{ #category : #testing }
ASIdentifier >> isBinary [
	^ value first isSpecial
]

{ #category : #testing }
ASIdentifier >> isIdentifier [
	^ true
]

{ #category : #testing }
ASIdentifier >> isKeyword [
	^ value last = $:
]

{ #category : #testing }
ASIdentifier >> isUnary [
	^ self isKeyword not and: [ self isBinary not ]
]

{ #category : #accessing }
ASIdentifier >> value [
	^ value
]

{ #category : #accessing }
ASIdentifier >> value: aString [
	value := aString.

]
