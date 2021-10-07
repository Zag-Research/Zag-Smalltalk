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
ASToken >> isLiteral [
	^ false
]

{ #category : #printing }
ASToken >> printOn: s [
	s << $< << value asString << $>

]

{ #category : #parsing }
ASToken >> processKeyword: anASCompiler target: anASLiteral [ 
	^ nil
]

{ #category : #accessing }
ASToken >> value [
	^ value
]

{ #category : #accessing }
ASToken >> value: aString [
	value := aString.

]
