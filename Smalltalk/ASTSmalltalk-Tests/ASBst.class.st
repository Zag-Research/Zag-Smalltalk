Class {
	#name : #ASBst,
	#superclass : #Object,
	#instVars : [
		'value',
		'right',
		'left'
	],
	#classInstVars : [
		'table',
		'root'
	],
	#category : #'ASTSmalltalk-Tests-Explore'
}

{ #category : #'key table' }
ASBst class >> addToTable: aValue [
	table add: (self new with: aValue).
	root := self insert: table size root: root.
	
]

{ #category : #'as yet unclassified' }
ASBst class >> insert: insert root: parent [ 
	| parentElement |
	parent = 0 ifTrue: [  ^ insert ].
	parentElement := table at: parent.
	(table at: insert) value < parentElement value ifTrue: [
		parentElement left: (self insert: insert root: parentElement left)
	] ifFalse: [
		parentElement right: (self insert: insert root: parentElement right)
	].
	^ parent
]

{ #category : #accessing }
ASBst class >> statistics [

	^ (ASBstStats on: table root: root) asString
]

{ #category : #statistics }
ASBst class >> statsWith: aCollection [ 
	table := OrderedCollection new.
	root := 0.
	aCollection do: [: each |
		self addToTable: each
	].
	^ self statistics
]

{ #category : #accessing }
ASBst >> left [
	^ left
]

{ #category : #accessing }
ASBst >> left: anInteger [ 
	left := anInteger
]

{ #category : #printing }
ASBst >> printOn: aStream [
	value printOn: aStream.
	aStream nextPutAll: ': '.
	left printOn: aStream.
	aStream nextPutAll: ' - '.
	right printOn: aStream.

]

{ #category : #accessing }
ASBst >> right [
	^ right
]

{ #category : #accessing }
ASBst >> right: anInteger [ 
	right := anInteger
]

{ #category : #accessing }
ASBst >> value [
	^ value
]

{ #category : #'instance creation' }
ASBst >> with: aValue [
	value := aValue.
	left := right := 0.

]
