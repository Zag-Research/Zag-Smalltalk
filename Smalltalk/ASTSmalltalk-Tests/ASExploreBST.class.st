"
Explore various options for Binary Search Trees for use in the symbol table for AST Smalltalk.

The SymbolTable will be implemented in Rust as will the rest of the VM.
However, it's a lot easier to explore and run experiments in Smalltalk.

Although hashing would likely be somewhat faster, Symbols are allocated in an array (a Rust Vec<Symbol>) and can't be moved once allocated, so a BST makes more sense. Also, while we want it to be fairly fast, it isn't on a critical execution path, really only being used when String>>#asSymbol is called (i.e. when a string has to be 'interned').
"
Class {
	#name : 'ASExploreBST',
	#superclass : 'Object',
	#instVars : [
		'nObjects',
		'totalDepth'
	],
	#category : 'ASTSmalltalk-Tests-Explore',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Explore'
}

{ #category : 'utility' }
ASExploreBST class >> optimalOrder: aCollection [ 
	| size median |
	size := aCollection size.
	size = 0 ifTrue: [ ^ #() ].
	median := (size+1)//2.
	^ { aCollection at: median },
		(self optimalOrder: (aCollection first: median-1)),
		(self optimalOrder: (aCollection last: size-median))
]

{ #category : 'accessing' }
ASExploreBST class >> stats [
	" self stats "
	| data results |
	"data := #(3 345 234 123 65 5 789 4435 4657 78 34 24 5667 345 76 09 8 909)."
	data := ByteSymbol allInstances.
	results := Dictionary new.
	{ ASBst . ASTreap . ASTreapPseudo } do: [: class |
		results at: class name,' random' put: (self statsOn: class data: data).
		results at: class name,' optimal' put: (self statsOn: class data: (self optimalOrder: data sorted)).
		results at: class name,' sorted' put: (self statsOn: class data: (class=ASBst ifTrue:[data first:819"1"] ifFalse:[data]) sorted).
	].
	^ results
]

{ #category : 'accessing' }
ASExploreBST class >> statsOn: aClass data: data [
	| aBST ms |
	ms := [ aBST := aClass withAll: data ] timeToRun.
	^ (self new
		on: aBST depth: 0;
		result),', in ',ms humanReadablePrintString
]

{ #category : 'initialization' }
ASExploreBST >> initialize [
	super initialize.
	nObjects := 0.
	totalDepth := 0.

]

{ #category : 'statistics' }
ASExploreBST >> on: tree depth: depth [
	tree ifNil: [ ^ self ].
	nObjects := nObjects + 1.
	totalDepth := totalDepth + depth.
	self on: tree left depth: depth+1.
	self on: tree right depth: depth+1.

]

{ #category : 'statistics' }
ASExploreBST >> result [
	^ 'average depth: ',(totalDepth/nObjects printShowingDecimalPlaces: 2)
]
