"
I am the scanner used by the ASCompile compiler

I accept Unicode and will treat many special characters as binary message name characters, and most letters in various scripts as unary or keyword message names.
"
Class {
	#name : #ASScanner,
	#superclass : #Object,
	#instVars : [
		'aStream',
		'peek'
	],
	#category : #'ASTSmalltalk-Compiler'
}

{ #category : #conversion }
ASScanner >> asArray [
	^ Array streamContents: [: s |
		[ self atEnd ] whileFalse: [
			s << self next
		]
	]
]

{ #category : #conversion }
ASScanner >> atEnd [
	^ self peek isNil
]

{ #category : #scanning }
ASScanner >> binaryIdentifier [ 
	^ (String streamContents: [: s | | char |
			[
				s << aStream next.
				char := aStream peek.
				aStream atEnd not and: [ '=<>+-*/\!@%&|~?' includes: char ]] whileTrue
		]) asASIdentifier
]

{ #category : #scanning }
ASScanner >> identifier [ 
	^ (String streamContents: [: s | | char |
			[
				s << aStream next.
				char := aStream peek.
				aStream atEnd not and: [ char = $_ or: [ char isLetter or: [ char isDigit ]]]] whileTrue
		]) asASIdentifier
]

{ #category : #scanning }
ASScanner >> next [
	^ self peek ifNotNil: [: t | peek := nil. t ].

]

{ #category : #scanning }
ASScanner >> number [ 
	^ (String streamContents: [: s |
			[
				s << aStream next.
				aStream atEnd not and: [aStream peek isDigit]
			] whileTrue
		]) asNumber asASLiteral
]

{ #category : #scanning }
ASScanner >> peek [
	peek ifNotNil: [ ^ peek ].
	aStream atEnd ifTrue: [ ^ nil ].
	^ peek := self token
]

{ #category : #scanning }
ASScanner >> scanCode: aStringOrStream [
	aStream := aStringOrStream readStream.

]

{ #category : #scanning }
ASScanner >> token [

	| char |
	[ aStream atEnd ifTrue: [ ^ nil ].
		aStream peek isSeparator ] whileTrue: [ aStream next ].
	char := aStream peek.
	('^.[]{}():|' includes: char) ifTrue: [ ^ aStream next asASSyntax ].
	char isLetter ifTrue: [ ^ self identifier ].
	char isDigit ifTrue: [ ^ self number ].
	char = $" ifTrue: [ ^ self comment ].
	char = $# ifTrue: [ ^ self symbol ].
	char = $' ifTrue: [ ^ self string ].
	char isSpecial ifTrue: [ ^ self binaryIdentifier ].
	^ self error: 'Illegal character:',char asString
]
