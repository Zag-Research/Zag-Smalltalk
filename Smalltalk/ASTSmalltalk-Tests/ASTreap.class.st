"
I represent a special kind of BST where my nodes are randomly ordered by maintaining a heap structure.

Keys are looked up exactly the same as a plain BST, but insertion and deletion may rotate the local structure.

Rotations look like:
  T1, T2 and T3 are subtrees of the tree rooted with y
  (on left side) or x (on right side)
                y                               x
               / \     Right Rotation          / \
              x   T3   – – – – – – – >        T1  y
             / \       < - - - - - - -           / \
            T1  T2     Left Rotation           T2  T3

Upon insertion in the left tree of y (in the left diagram), if the heap property isn't maintained, then a right rotation is required that transforms the tree into the right diagram.

Similarly, upon insertion in the right tree of x (in the right diagram), a left rotation may be required.
"
Class {
	#name : #ASTreap,
	#superclass : #ASBst,
	#instVars : [
		'priority'
	],
	#classInstVars : [
		'randomGenerator'
	],
	#category : #'ASTSmalltalk-Tests-Explore'
}

{ #category : #random }
ASTreap class >> random [
	^ randomGenerator nextInt: 16r100000000 " 32 bit random integer "
]
