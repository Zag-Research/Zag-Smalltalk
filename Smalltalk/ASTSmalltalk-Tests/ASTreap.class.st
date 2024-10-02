"
I represent a special kind of BST where my nodes are randomly ordered by maintaining a heap structure.
Each node is allocated a random priority, where ""heapness"" means a root has a higher priority than its subtrees.

Keys are looked up exactly the same as a plain BST, but insertion and deletion may rotate the local structure.

Rotations look like:
```language=text
  T1, T2 and T3 are subtrees of the tree rooted with y
  (on left side) or x (on right side)
                y                               x
               / \     Right Rotation          / \
              x   T3   – – – – – – – >        T1  y
             / \       < - - - - - - -           / \
            T1  T2     Left Rotation           T2  T3
```
Upon insertion in the left tree of y (in the left diagram), if the heap property isn't maintained, then a right rotation is required that transforms the tree into the right diagram.

Similarly, upon insertion in the right tree of x (in the right diagram), a left rotation may be required.
"
Class {
	#name : 'ASTreap',
	#superclass : 'ASBst',
	#instVars : [
		'priority'
	],
	#classInstVars : [
		'randomGenerator'
	],
	#category : 'ASTSmalltalk-Tests-Explore',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Explore'
}

{ #category : 'random' }
ASTreap class >> random [

	^ randomGenerator nextInteger: 16r100000000 " 32 bit random integer "
]

{ #category : 'instance creation' }
ASTreap class >> withAll: data [
	root := nil.
	randomGenerator := Random new.
	data do: [: key |
		root := root insert: (self new key: key;priority: self random).
	].
	^ root
]

{ #category : 'accessing' }
ASTreap >> insert: anASBst [ 
	anASBst key < key ifTrue: [
		left := left insert: anASBst.
		^ self maybeRightRotate
	] ifFalse: [
		right := right insert: anASBst.
		^ self maybeLeftRotate
	]
]

{ #category : 'insertion' }
ASTreap >> maybeLeftRotate [
	| t2 root |
	self priority > right priority ifTrue: [ ^ self ].
	t2 := right left.
	root := right.
	root left: self.
	right := t2.
	^ root
]

{ #category : 'insertion' }
ASTreap >> maybeRightRotate [
	| t2 root |
	self priority > left priority ifTrue: [ ^ self ].
	t2 := left right.
	root := left.
	root right: self.
	left := t2.
	^ root
]

{ #category : 'accessing' }
ASTreap >> priority [
	^ priority
]

{ #category : 'accessing' }
ASTreap >> priority: anInteger [ 
	priority := anInteger
]
