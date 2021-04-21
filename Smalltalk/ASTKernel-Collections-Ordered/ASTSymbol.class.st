Class {
	#name : #ASTSymbol,
	#superclass : #ASTString,
	#type : #bytes,
	#category : #'ASTKernel-Collections-Ordered'
}

{ #category : #'as yet unclassified' }
ASTSymbol class >> new: size [
	"Symbols are unique. You can create a new Symbol from a String using 'asSymbol'."

	self cannotInstantiate
]

{ #category : #'as yet unclassified' }
ASTSymbol >> = anObject [
	^ self == anObject
]

{ #category : #'as yet unclassified' }
ASTSymbol >> asString [
	"Answer a string containing my characters."

	| sz result |
	sz := self size.
	result := ASTString new: sz.
	result
		replaceFrom: 1
		to: sz
		with: self
		startingAt: 1.
	^ result
]

{ #category : #'as yet unclassified' }
ASTSymbol >> asSymbol [
	^ self
]

{ #category : #'as yet unclassified' }
ASTSymbol >> at: anInteger put: anObject [
	"You cannot modify the receiver."

	self errorNoModification
]

{ #category : #'as yet unclassified' }
ASTSymbol >> errorNoModification [
	self error: 'Symbols can not be modified.'
]

{ #category : #'as yet unclassified' }
ASTSymbol >> hash [
	^ self identityHash
]

{ #category : #'as yet unclassified' }
ASTSymbol >> initFrom: aString [
	"Warning! Use only to initialize new Symbols. Symbols are assumed to be immutable there after."

	self size = aString size
		ifFalse: [ self error: 'size mismatch' ].
	super
		replaceFrom: 1
		to: self size
		with: aString
		startingAt: 1
]

{ #category : #'as yet unclassified' }
ASTSymbol >> printOn: aStream [
	aStream nextPutAll: self
]

{ #category : #'as yet unclassified' }
ASTSymbol >> replaceFrom: start to: stop with: replacement startingAt: repStart [
	self errorNoModification
]

{ #category : #'as yet unclassified' }
ASTSymbol >> shallowCopy [
	"Answer myself because Symbols are unique."

	
]

{ #category : #'as yet unclassified' }
ASTSymbol >> species [
	^ ASTString
]
