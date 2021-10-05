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
ASToken >> atEnd [
	^ false
]

{ #category : #testing }
ASToken >> isArrayBuild [
	^ false
]

{ #category : #testing }
ASToken >> isAssignment [
	^ false
]

{ #category : #testing }
ASToken >> isBar [
	^ false
]

{ #category : #testing }
ASToken >> isBlockStart [
	^ false
]

{ #category : #testing }
ASToken >> isColon [
	^ false
]

{ #category : #testing }
ASToken >> isIdentifier [
	^ false
]

{ #category : #testing }
ASToken >> isKeyword [
	^ false
]

{ #category : #testing }
ASToken >> isLiteral [
	^ false
]

{ #category : #testing }
ASToken >> isMethodIntro [
	^ false
]

{ #category : #testing }
ASToken >> isOpenBracket [
	^ false
]

{ #category : #testing }
ASToken >> isParen [
	^ false
]

{ #category : #testing }
ASToken >> isParrot [
	^ false
]

{ #category : #testing }
ASToken >> isReturn [
	^ false
]

{ #category : #testing }
ASToken >> isSelf [
	^ false
]

{ #category : #testing }
ASToken >> isSeparator [
	^ false
]

{ #category : #testing }
ASToken >> isSpecial [
	^ false
]

{ #category : #testing }
ASToken >> matchesBracket: unused [
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
