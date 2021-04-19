"
Explore various options for Binary Search Trees for use in symbol table.
"
Class {
	#name : #ASExploreBST,
	#superclass : #Object,
	#category : #'ASTSmalltalk-Tests-Explore'
}

{ #category : #'as yet unclassified' }
ASExploreBST class >> optimalOrder: aCollection [ 
	| size median |
	size := aCollection size.
	size = 0 ifTrue: [ ^ #() ].
	median := (size+1)//2.
	^ { aCollection at: median },
		(self optimalOrder: (aCollection first: median-1)),
		(self optimalOrder: (aCollection last: size-median))
]

{ #category : #accessing }
ASExploreBST class >> stats [
	" self stats "
	| data results |
	data := #(3 345 234 123 65 5 789 4435 4657 78 34 24 5667 345 76 09 8 909).
	results := Dictionary new.
	{ ASBst } do: [: class |
		results at: class name,' random' put: (class statsWith: data).
		results at: class name,' sorted' put: (class statsWith: data sorted).
		results at: class name,' optimal' put: (class statsWith: (self optimalOrder: data sorted)).
	].
	^ results
]
