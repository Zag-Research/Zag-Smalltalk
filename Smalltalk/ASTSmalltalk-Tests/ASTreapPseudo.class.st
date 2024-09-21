"
I represent a Treap, but rather than using a high quality pseudo-random number for the priority for each node, I use an easily calculated ""pseudo-random number"" based on the order of insertion.

The priority is simply `insertionNumber*1999999973 bitAnd: 16rffffffff` which is easy to calculate in Rust as a 32-bit unsigned multiplication with wrap-around overflow. This means, in our application, since symbols are allocated in sequence in an array that the `insertionNumber` is simply the index, so we don't need to store the priority in the node as we can re-create it trivially.

In experiments this works at least as well as using the system's more sophisticated pseudo-random generator.
"
Class {
	#name : 'ASTreapPseudo',
	#superclass : 'ASTreap',
	#classInstVars : [
		'index'
	],
	#category : 'ASTSmalltalk-Tests-Explore',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Explore'
}

{ #category : 'random' }
ASTreapPseudo class >> random [
	randomGenerator := randomGenerator + 1.
	^ randomGenerator*1999999973 bitAnd: 16rffffffff " multiply by a large prime and truncate to 32 bits "
]

{ #category : 'instance creation' }
ASTreapPseudo class >> withAll: data [
	root := nil.
	randomGenerator := 0.
	data do: [: key |
		root := root insert: (self new key: key;priority: self random).
	].
	^ root
]
