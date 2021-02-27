"
I am the scanner used by the ASCompile compiler

I accept Unicode and will treat many special characters as binary message name characters, and most letters in various scripts as unary or keyword message names.
"
Class {
	#name : #ASScanner,
	#superclass : #Object,
	#instVars : [
		'result'
	],
	#category : #'ASTSmalltalk-Compiler'
}

{ #category : #conversion }
ASScanner >> asArray [
	^ result contents
]

{ #category : #scanning }
ASScanner >> identifierFrom: aStream [ 
	result << (String streamContents: [: s | | char |
			[
				s << aStream next.
				char := aStream peek.
				char = $_ or: [ char isLetter or: [ char isDigit ] ] ] whileTrue
		]) asASIdentifier
]

{ #category : #initialization }
ASScanner >> initialize [
	super initialize.
	result := WriteStream on: (Array new).

]

{ #category : #scanning }
ASScanner >> numberFrom: aStream [ 
	result << (String streamContents: [: s |
			[
				s << aStream next.
				aStream peek isDigit
			] whileTrue
		]) asNumber
]

{ #category : #scanning }
ASScanner >> scanCode: aStringOrStream [
	| aStream |
	aStream := aStringOrStream readStream.
	[aStream atEnd] whileFalse: [
		self tokenFrom: aStream
	]
]

{ #category : #scanning }
ASScanner >> token [
	self shouldBeImplemented.
]

{ #category : #scanning }
ASScanner >> tokenFrom: aStream [ 
	| char |
	[' 	
' includes: aStream peek] whileTrue: [ aStream next ].
	char := aStream peek.
	('^.[]{}():|' includes: char) ifTrue: [ result << aStream next. ^ self ].
	aStream atEnd ifTrue: [ ^ self ].
	char isLetter ifTrue: [ ^ self identifierFrom: aStream ].
	char isDigit ifTrue: [ ^ self numberFrom: aStream ].
	char = $" ifTrue: [ ^ self commentFrom: aStream ].
	char = $# ifTrue: [ ^ self symbolFrom: aStream ].
	char = $' ifTrue: [ ^ self stringFrom: aStream ].
	^ self error: 'Unexpected input: ',char asString
]
