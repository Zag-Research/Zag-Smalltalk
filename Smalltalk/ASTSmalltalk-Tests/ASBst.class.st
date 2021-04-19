"
I represent a simple Binary Search Tree. If I am balanced, I am the fastest comparison-based search structure.

Unfortunately, in the worst case of un-balance, I become a linear search.

I look like:
               key
              /   \
           left   right
where all the nodes less than key are in the left BST and all the nodes greater than key are in the right BST.
"
Class {
	#name : #ASBst,
	#superclass : #Object,
	#instVars : [
		'right',
		'left',
		'key'
	],
	#classInstVars : [
		'root'
	],
	#category : #'ASTSmalltalk-Tests-Explore'
}

{ #category : #'key table' }
ASBst class >> addToTable: aValue [
	table add: (self new with: aValue).
	root := self insert: table size root: root.
	
]

{ #category : #'class initialization' }
ASBst class >> initialize [
	root := nil.

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
	aStream nextPut: $(.
	key printOn: aStream.
	aStream nextPut: $ .
	left printOn: aStream.
	aStream nextPut: $ .
	right printOn: aStream.
	aStream nextPut: $).

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
	^ key
]

{ #category : #'instance creation' }
ASBst >> with: aValue [
	key := aValue.
	left := right := 0.

]
