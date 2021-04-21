Class {
	#name : #PCAssociation,
	#superclass : #PCMagnitude,
	#instVars : [
		'key',
		'value'
	],
	#category : #'ASTKernel-Collections-Unordered'
}

{ #category : #'as yet unclassified' }
PCAssociation class >> key: newKey value: newValue [
	"Answer a new Association with the given key and value."

	^ self new key: newKey value: newValue
]

{ #category : #'as yet unclassified' }
PCAssociation >> < aLookupKey [
	"Sort by keys."

	^ key < aLookupKey key
]

{ #category : #'as yet unclassified' }
PCAssociation >> = anAssociation [
	"True if the receiver and argument have equal keys."

	self species = anAssociation species
		ifTrue: [ ^ key = anAssociation key ]
		ifFalse: [ ^ false ]
]

{ #category : #'as yet unclassified' }
PCAssociation >> hash [
	"Hash is reimplemented because = is implemented."

	^ key hash
]

{ #category : #'as yet unclassified' }
PCAssociation >> key [
	^ key
]

{ #category : #'as yet unclassified' }
PCAssociation >> key: anObject [
	key := anObject
]

{ #category : #'as yet unclassified' }
PCAssociation >> key: aKey value: anObject [
	key := aKey.
	value := anObject
]

{ #category : #'as yet unclassified' }
PCAssociation >> printOn: aStream [
	"Print in the format (key->value)."

	aStream nextPut: $(.
	key printOn: aStream.
	aStream nextPutAll: '->'.
	value printOn: aStream.
	aStream nextPut: $)
]

{ #category : #'as yet unclassified' }
PCAssociation >> value [
	^ value
]

{ #category : #'as yet unclassified' }
PCAssociation >> value: anObject [
	value := anObject
]
