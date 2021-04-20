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

{ #category : #random }
ASTreapPseudo class >> random [
	randomGenerator := randomGenerator + 1.
	^ randomGenerator*1999999973 bitAnd: 16rffffffff " multiply by a large prime and truncate to 32 bits "
]
