Class {
	#name : 'ASTSymbol',
	#superclass : 'ASTString',
	#type : 'bytes',
	#category : 'ASTKernel-Collections-Ordered',
	#package : 'ASTKernel-Collections-Ordered'
}

{ #category : 'instance creation' }
ASTSymbol class >> intern: aString [
	<primitive: #symbolIntern>
]

{ #category : 'as yet unclassified' }
ASTSymbol class >> new: size [
	"Symbols are unique. You can create a new Symbol from a String using 'asSymbol'."

	self cannotInstantiate
]

{ #category : 'comparing' }
ASTSymbol >> = anObject [
	^ self == anObject
]

{ #category : 'converting' }
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

{ #category : 'as yet unclassified' }
ASTSymbol >> asSymbol [
	^ self
]

{ #category : 'accessing' }
ASTSymbol >> at: anInteger put: anObject [
	"You cannot modify the receiver."

	self errorNoModification
]

{ #category : 'reflective operations' }
ASTSymbol >> basicIdentityHash [
	"Answer the immediate hashcode."

	<primitive: 171>
	
]

{ #category : 'as yet unclassified' }
ASTSymbol >> errorNoModification [
	self error: 'Symbols can not be modified.'
]

{ #category : 'comparing' }
ASTSymbol >> hash [
	^ self identityHash
]

{ #category : 'as yet unclassified' }
ASTSymbol >> numArgs [
	^ self identityHash >> 25
]

{ #category : 'printing' }
ASTSymbol >> printOn: aStream [
	aStream nextPutAll: self
]

{ #category : 'system primitives' }
ASTSymbol >> replaceFrom: start to: stop with: replacement startingAt: repStart [
	self errorNoModification
]

{ #category : 'copying' }
ASTSymbol >> shallowCopy [
	"Answer myself because Symbols are unique."

	
]

{ #category : 'as yet unclassified' }
ASTSymbol >> species [
	^ ASTString
]
