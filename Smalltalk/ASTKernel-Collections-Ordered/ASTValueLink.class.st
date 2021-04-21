Class {
	#name : #ASTValueLink,
	#superclass : #ASTObject,
	#instVars : [
		'nextLink',
		'value'
	],
	#category : #'ASTKernel-Collections-Ordered'
}

{ #category : #'as yet unclassified' }
ASTValueLink class >> value: aValue [
	^ self new value: aValue
]

{ #category : #'as yet unclassified' }
ASTValueLink >> = anotherObject [
	^ self species == anotherObject species
		and: [ self value = anotherObject value
				and: [ self nextLink == anotherObject nextLink ] ]
]

{ #category : #'as yet unclassified' }
ASTValueLink >> asLink [
	^ self
]

{ #category : #'as yet unclassified' }
ASTValueLink >> hash [
	^ self value hash bitXor: nextLink identityHash
]

{ #category : #'as yet unclassified' }
ASTValueLink >> nextLink [
	^ nextLink
]

{ #category : #'as yet unclassified' }
ASTValueLink >> nextLink: aLink [
	nextLink := aLink
]

{ #category : #'as yet unclassified' }
ASTValueLink >> printOn: aStream [
	aStream nextPutAll: 'ValueLink('.
	value printOn: aStream.
	aStream nextPut: $)
]

{ #category : #'as yet unclassified' }
ASTValueLink >> value [
	^ value
]

{ #category : #'as yet unclassified' }
ASTValueLink >> value: aValue [
	value := aValue
]
