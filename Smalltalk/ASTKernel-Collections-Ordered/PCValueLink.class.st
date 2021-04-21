Class {
	#name : #PCValueLink,
	#superclass : #PCObject,
	#instVars : [
		'nextLink',
		'value'
	],
	#category : #'ASTKernel-Collections-Ordered'
}

{ #category : #'as yet unclassified' }
PCValueLink class >> value: aValue [
	^ self new value: aValue
]

{ #category : #'as yet unclassified' }
PCValueLink >> = anotherObject [
	^ self species == anotherObject species
		and: [ self value = anotherObject value
				and: [ self nextLink == anotherObject nextLink ] ]
]

{ #category : #'as yet unclassified' }
PCValueLink >> asLink [
	^ self
]

{ #category : #'as yet unclassified' }
PCValueLink >> hash [
	^ self value hash bitXor: nextLink identityHash
]

{ #category : #'as yet unclassified' }
PCValueLink >> nextLink [
	^ nextLink
]

{ #category : #'as yet unclassified' }
PCValueLink >> nextLink: aLink [
	nextLink := aLink
]

{ #category : #'as yet unclassified' }
PCValueLink >> printOn: aStream [
	aStream nextPutAll: 'ValueLink('.
	value printOn: aStream.
	aStream nextPut: $)
]

{ #category : #'as yet unclassified' }
PCValueLink >> value [
	^ value
]

{ #category : #'as yet unclassified' }
PCValueLink >> value: aValue [
	value := aValue
]
