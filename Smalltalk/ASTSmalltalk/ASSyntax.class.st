Class {
	#name : #ASSyntax,
	#superclass : #ASToken,
	#category : #'ASTSmalltalk-Tokens'
}

{ #category : #testing }
ASSyntax >> isArrayBuild [
	^ value = ${
]

{ #category : #testing }
ASSyntax >> isAssignment [
	^ value = ':='
]

{ #category : #testing }
ASSyntax >> isBar [
	^ value = $|
]

{ #category : #testing }
ASSyntax >> isBlockStart [
	^ value = $[
]

{ #category : #testing }
ASSyntax >> isColon [
	^ value = $:
]

{ #category : #testing }
ASSyntax >> isOpenBracket [
	^ '[{(' includes: value
]

{ #category : #testing }
ASSyntax >> isParen [
	^ value = $(
]

{ #category : #testing }
ASSyntax >> isParrot [
	^ value = ':>'
]

{ #category : #testing }
ASSyntax >> isReturn [
	^ value = $^
]

{ #category : #testing }
ASSyntax >> isSeparator [
	^ value = $.
]

{ #category : #testing }
ASSyntax >> matchesBracket: other [
	| ov |
	ov := other value.
	^ (value=$] and: [ov=$[]) or: [(value=$} and: [ov=${]) or: [value=$) and: [ov=$(]]]
]
