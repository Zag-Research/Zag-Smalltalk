Class {
	#name : #ASIdentifier,
	#superclass : #ASToken,
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #accessing }
ASIdentifier >> arity [
	| last |
	last := value last.
	last tokenish ifFalse: [ ^ 2 ].
	(value includes: $:) ifFalse: [ ^ 1 ].
	last = $: ifFalse: [ ^ 0 ].
	^ 1+(value count: [: c | c=$:])
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
ASIdentifier >> isMethodIntro [
	^ self arity between: 1 and: 2
]

{ #category : #testing }
ASIdentifier >> isUnary [
	^ self isKeyword not and: [ self isBinary not ]
]
