"
I am the scanner used by the ASCompile compiler

I accept Unicode and will treat many special characters as binary message name characters, and most letters in various scripts as unary or keyword message names.
"
Class {
	#name : 'ASScanner',
	#superclass : 'Object',
	#instVars : [
		'aStream',
		'peek'
	],
	#category : 'Zag-Core-Compiler',
	#package : 'Zag-Core',
	#tag : 'Compiler'
}

{ #category : 'conversion' }
ASScanner >> asArray [
	^ Array streamContents: [: s |
		[ self atEnd ] whileFalse: [
			s << self next
		]
	]
]

{ #category : 'conversion' }
ASScanner >> atEnd [
	^ self peek atEnd
]

{ #category : 'scanning' }
ASScanner >> binaryIdentifier [ 
	^ (String streamContents: [: s | | char |
			[
				s << aStream next.
				char := aStream peek.
				aStream atEnd not and: [ char isSpecial ]] whileTrue
		]) asASIdentifier
]

{ #category : 'scanning' }
ASScanner >> identifier [
	^ self identifierString asASIdentifier
]

{ #category : 'scanning' }
ASScanner >> identifierString [
	^ (String streamContents: [: s | | char prevWasColon |
			prevWasColon := false.
			[
				s << aStream next.
				char := aStream peek.
				aStream atEnd not and: [ char tokenish or: [ prevWasColon and: [ ('=>' includes: char) and: [ s setFrom: 1 to: s size-1. s position: s size. false ] ] ]]
			] whileTrue: [
				prevWasColon := char = $:
			]
		])
]

{ #category : 'scanning' }
ASScanner >> next [
	^ self peek ifNotNil: [: t | peek := nil. t ].

]

{ #category : 'scanning' }
ASScanner >> number [ 
	^ (String streamContents: [: s |
			[
				s << aStream next.
				aStream atEnd not and: [aStream peek isDigit]
			] whileTrue
		]) asNumber asASLiteral
]

{ #category : 'scanning' }
ASScanner >> peek [
	peek ifNotNil: [ ^ peek ].
	^ peek := self token
]

{ #category : 'scanning' }
ASScanner >> scanCode: aStringOrStream [
	aStream := aStringOrStream readStream.

]

{ #category : 'scanning' }
ASScanner >> string [
	aStream next.
	^ (String streamContents: [: s | | char |
			[
				char := aStream next.
				char ~= $' or: [
					aStream peek = $' and: [aStream next.true]].
			] whileTrue: [ s << char ]
		]) asASString
]

{ #category : 'scanning' }
ASScanner >> symbol [
	| char |
	aStream next.
	char := aStream peek.
	char isLetter ifTrue: [ ^ self identifierString asASSymbol ].
	char = $( ifTrue: [ aStream next. ^ '#(' asASSyntax ].
	char = $' ifTrue: [ ^ self string value asASSymbol].
	char isDigit ifTrue: [ ^ self number ].
^ self error: 'invalid symbol'
]

{ #category : 'scanning' }
ASScanner >> token [

	| char |
	[ aStream atEnd ifTrue: [ ^ ASEnd new ].
		aStream peek isSeparator ] whileTrue: [ aStream next ].
	char := aStream peek.
	('^.[]{}();' includes: char) ifTrue: [ ^ aStream next asASSyntax ].
	char isLetter ifTrue: [ ^ self identifier ].
	char isDigit ifTrue: [ ^ self number ].
	char = $" ifTrue: [ ^ self comment ].
	char = $# ifTrue: [ ^ self symbol ].
	char = $' ifTrue: [ ^ self string ].
	char = $: ifTrue: [
		aStream next.
		('=>' includes: aStream peek) ifTrue: [
			^ (':',aStream next asString) asASSyntax
		] ifFalse: [
			^ $: asASSyntax
	]].
	char isSpecial ifTrue: [ ^ self binaryIdentifier ].
	^ self error: 'Illegal character:',char asString
]
