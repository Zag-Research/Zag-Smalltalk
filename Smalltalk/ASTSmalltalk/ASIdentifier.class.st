Class {
	#name : #ASIdentifier,
	#superclass : #ASToken,
	#category : #'ASTSmalltalk-Tokens'
}

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
