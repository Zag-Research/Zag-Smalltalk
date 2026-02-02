"
I am the scanner used by the ASCompile compiler

I accept Unicode and will treat many special characters as binary message name characters, and most letters in various scripts as unary or keyword message names.
"
Class {
	#name : 'ASScanner',
	#superclass : 'Object',
	#instVars : [
		'aStream',
		'peek',
		'separator'
	],
	#category : 'Zag-Compiler-Core',
	#package : 'Zag-Compiler',
	#tag : 'Core'
}

{ #category : 'public api' }
ASScanner class >> scan: aStringOrStream [

	^ self new scanCode: aStringOrStream
]

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

{ #category : 'private' }
ASScanner >> binaryIdentifier [ 
	^ (String streamContents: [: s | | char |
			[
				s << aStream next.
				char := aStream peek.
				aStream atEnd not and: [ char isSpecial ]] whileTrue
		]) asASIdentifier
]

{ #category : 'private' }
ASScanner >> comment [

	aStream next.
	^ (String streamContents: [ :s |
			   | char |
			   [
				   char := aStream next.
				   char ~= $" or: [
						   aStream peek = $" and: [
								   aStream next.
								   true ] ] ] whileTrue: [ s << char ] ])
]

{ #category : 'private' }
ASScanner >> identifier [
	^ self identifierString asASIdentifier
]

{ #category : 'private' }
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

{ #category : 'initialization' }
ASScanner >> initialize [

	super initialize.
	separator := ''
]

{ #category : 'private' }
ASScanner >> next [
	^ self peek ifNotNil: [: t | peek := nil. t ].

]

{ #category : 'private' }
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

{ #category : 'private' }
ASScanner >> possibleSeparator [

	| whitespace |
	whitespace := ''.
	[ aStream atEnd ] whileFalse: [
			| in |
			in := aStream peek asInteger.
			34 = in
				ifTrue: [
						separator := separator , whitespace , '"' , self comment , '"'.
						whitespace := '' ]
				ifFalse: [
						(in = 13 or: [ in = 10 or: [ in = 12 ] ])
							ifTrue: [
									separator := separator , aStream next asString.
									whitespace := '' ]
							ifFalse: [
									(in = 32 or: [ in = 9 ])
										ifTrue: [ whitespace := whitespace , aStream next asString ]
										ifFalse: [ ^ self ] ] ] ]
]

{ #category : 'scanning' }
ASScanner >> scanCode: aStringOrStream [
	aStream := aStringOrStream readStream.

]

{ #category : 'accessing' }
ASScanner >> separator [
	^ separator
]

{ #category : 'private' }
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

{ #category : 'private' }
ASScanner >> symbol [

	| char |
	aStream next.
	char := aStream peek.
	char isLetter ifTrue: [ ^ self identifierString asASSymbol ].
	char = $( ifTrue: [
			aStream next.
			^ '#(' asASSyntax ].
	char = $' ifTrue: [ ^ self string value asASSymbol ].
	char = $_ ifTrue: [ ^ self identifierString asASSymbol ].
	^ self error: 'invalid symbol'
]

{ #category : 'private' }
ASScanner >> token [

	| char |
	self possibleSeparator.
	aStream atEnd ifTrue: [ ^ ASEnd new ].
	char := aStream peek.
	('^.[]{}();' includes: char) ifTrue: [ ^ aStream next asASSyntax ].
	char isLetter ifTrue: [ ^ self identifier ].
	char isDigit ifTrue: [ ^ self number ].
	char = $# ifTrue: [ ^ self symbol ].
	char = $' ifTrue: [ ^ self string ].
	char = $: ifTrue: [
			aStream next.
			('=>' includes: aStream peek)
				ifTrue: [ ^ (':' , aStream next asString) asASSyntax ]
				ifFalse: [ ^ $: asASSyntax ] ].
	char isSpecial ifTrue: [ ^ self binaryIdentifier ].
	char = $_ ifTrue: [ ^ self identifier ].
	^ self error: 'Illegal character:' , char asString
]
