"
I represent a simple Binary Search Tree. If I am balanced, I am the fastest comparison-based search structure.

Unfortunately, in the worst case of un-balance, I become a linear search.

I look like:
```
               key
              /   \
           left   right
```
where all the nodes less than key are in the left BST and all the nodes greater than key are in the right BST.
"
Class {
	#name : #ASBst,
	#superclass : #Object,
	#instVars : [
		'key',
		'left',
		'right'
	],
	#classInstVars : [
		'root'
	],
	#category : 'ASTSmalltalk-Tests-Explore'
}

{ #category : #'instance creation' }
ASBst class >> withAll: data [
	root := nil.
	data do: [: key |
		root := root insert: (self new key: key).
	].
	^ root
]

{ #category : #accessing }
ASBst >> insert: anASBst [ 
	anASBst key < key ifTrue: [
		left := left insert: anASBst
	] ifFalse: [
		right := right insert: anASBst
	]
]

{ #category : #accessing }
ASBst >> key [
	^ key
]

{ #category : #accessing }
ASBst >> key: aKey [ 
	key := aKey
]

{ #category : #accessing }
ASBst >> left [
	^ left
]

{ #category : #accessing }
ASBst >> left: anASTreap [ 
	left := anASTreap
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
ASBst >> right: anASTreap [ 
	right := anASTreap
]
