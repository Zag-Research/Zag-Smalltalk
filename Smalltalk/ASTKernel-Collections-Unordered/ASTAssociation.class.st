Class {
	#name : #ASTAssociation,
	#superclass : #ASTMagnitude,
	#instVars : [
		'key',
		'value'
	],
	#category : #'ASTKernel-Collections-Unordered'
}

{ #category : #'as yet unclassified' }
ASTAssociation class >> key: newKey value: newValue [
	"Answer a new Association with the given key and value."

	^ self new key: newKey value: newValue
]

{ #category : #'as yet unclassified' }
ASTAssociation >> < aLookupKey [
	"Sort by keys."

	^ key < aLookupKey key
]

{ #category : #'as yet unclassified' }
ASTAssociation >> = anAssociation [
	"True if the receiver and argument have equal keys."

	self species = anAssociation species
		ifTrue: [ ^ key = anAssociation key ]
		ifFalse: [ ^ false ]
]

{ #category : #'as yet unclassified' }
ASTAssociation >> hash [
	"Hash is reimplemented because = is implemented."

	^ key hash
]

{ #category : #'as yet unclassified' }
ASTAssociation >> key [
	^ key
]

{ #category : #'as yet unclassified' }
ASTAssociation >> key: anObject [
	key := anObject
]

{ #category : #'as yet unclassified' }
ASTAssociation >> key: aKey value: anObject [
	key := aKey.
	value := anObject
]

{ #category : #printing }
ASTAssociation >> printOn: aStream [
	"Print in the format (key->value)."

	aStream nextPut: $(.
	key printOn: aStream.
	aStream nextPutAll: '->'.
	value printOn: aStream.
	aStream nextPut: $)
]

{ #category : #'as yet unclassified' }
ASTAssociation >> value [
	^ value
]

{ #category : #'as yet unclassified' }
ASTAssociation >> value: anObject [
	value := anObject
]
