"
I am the tokens for the AST scanner
"
Class {
	#name : #ASToken,
	#superclass : #Object,
	#instVars : [
		'value'
	],
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #comparing }
ASToken >> = other [
	^ self class = other class and: [value = other value]
]

{ #category : #scanning }
ASToken >> asASSymbol [
	^ ASSymbol new value: value asSymbol
]

{ #category : #testing }
ASToken >> isIdentifier [
	^ false
]

{ #category : #testing }
ASToken >> isLiteral [
	^ false
]

{ #category : #testing }
ASToken >> isSelf [
	^ false
]

{ #category : #testing }
ASToken >> isSpecial [
	^ false
]

{ #category : #printing }
ASToken >> printOn: s [
	s << $< << value asString << $>

]

{ #category : #accessing }
ASToken >> value [
	^ value
]

{ #category : #accessing }
ASToken >> value: aString [
	value := aString.

]
