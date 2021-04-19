"
Explore various options for Binary Search Trees for use in symbol table.
"
Class {
	#name : #ASExploreBST,
	#superclass : #Object,
	#instVars : [
		'nObjects',
		'totalDepth'
	],
	#category : #'ASTSmalltalk-Tests-Explore'
}

{ #category : #utility }
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
	data := ByteSymbol allInstances first: 8191.
	results := Dictionary new.
	{ ASBst ". ASTreap . ASTreapPseudo" } do: [: class |
		results at: class name,' random' put: (self statsOn: class statsWith: data).
		results at: class name,' sorted' put: (self statsOn: class statsWith: data sorted).
		results at: class name,' optimal' put: (self statsOn: class statsWith: (self optimalOrder: data sorted)).
	].
	^ results
]

{ #category : #accessing }
ASExploreBST class >> statsOn: aClass statsWith: data [
	| aBST |
	aBST := aClass withAll: data.
	^ self new
		on: aBST depth: 0;
		result
]

{ #category : #initialization }
ASExploreBST >> initialize [
	super initialize.
	nObjects := 0.
	totalDepth := 0.

]

{ #category : #statistics }
ASExploreBST >> on: tree depth: depth [
	tree ifNil: [ ^ self ].
	nObjects := nObjects + 1.
	totalDepth := totalDepth + depth.
	self on: tree left depth: depth+1.
	self on: tree right depth: depth+1.

]

{ #category : #statistics }
ASExploreBST >> result [
	^ 'average depth:',(totalDepth/nObjects asFloat) asString
]
