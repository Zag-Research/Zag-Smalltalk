"
I represent a Treap, but rather than using a high quality pseudo-random number for the priority for each node, I use an easily calculated ""pseudo-random number"" based on the order of insertion.
"
Class {
	#name : #ASTreapPseudo,
	#superclass : #ASTreap,
	#classInstVars : [
		'index'
	],
	#category : #'ASTSmalltalk-Tests-Explore'
}

{ #category : #'class initialization' }
ASTreapPseudo class >> initialize [
	index := 0.
	root := nil.

]

{ #category : #random }
ASTreapPseudo class >> random [
	index := index + 1.
	^ index*1999999973 bitAnd: 16rffffffff " multiply by a large prime and truncate to 32 bits "
]
