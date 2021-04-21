Class {
	#name : #PCSymbol,
	#superclass : #PCString,
	#type : #bytes,
	#category : #'ASTKernel-Collections-Ordered'
}

{ #category : #'as yet unclassified' }
PCSymbol class >> new: size [
	"Symbols are unique. You can create a new Symbol from a String using 'asSymbol'."

	self cannotInstantiate
]

{ #category : #'as yet unclassified' }
PCSymbol >> = anObject [
	^ self == anObject
]

{ #category : #'as yet unclassified' }
PCSymbol >> asString [
	"Answer a string containing my characters."

	| sz result |
	sz := self size.
	result := PCString new: sz.
	result
		replaceFrom: 1
		to: sz
		with: self
		startingAt: 1.
	^ result
]

{ #category : #'as yet unclassified' }
PCSymbol >> asSymbol [
	^ self
]

{ #category : #'as yet unclassified' }
PCSymbol >> at: anInteger put: anObject [
	"You cannot modify the receiver."

	self errorNoModification
]

{ #category : #'as yet unclassified' }
PCSymbol >> basicCopy [
	"Answer myself because Symbols are unique."

	
]

{ #category : #'as yet unclassified' }
PCSymbol >> errorNoModification [
	self error: 'Symbols can not be modified.'
]

{ #category : #'as yet unclassified' }
PCSymbol >> hash [
	^ self identityHash
]

{ #category : #'as yet unclassified' }
PCSymbol >> initFrom: aString [
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
PCSymbol >> printOn: aStream [
	aStream nextPutAll: self
]

{ #category : #'as yet unclassified' }
PCSymbol >> replaceFrom: start to: stop with: replacement startingAt: repStart [
	self errorNoModification
]

{ #category : #'as yet unclassified' }
PCSymbol >> species [
	^ PCString
]
