"
I have the knowledge of how to encode values for SpurPlus images
"
Class {
	#name : 'ASSpurPlusImage',
	#superclass : 'ASImage',
	#classVars : [
		'GroupHeap',
		'GroupImmediates',
		'GroupSmallInt'
	],
	#category : 'ASTSmalltalk-Image',
	#package : 'ASTSmalltalk',
	#tag : 'Image'
}

{ #category : 'adding' }
ASSpurPlusImage class >> double: aFloat [

	| anASSpurPlusImage |
	anASSpurPlusImage := self new.

	anASSpurPlusImage halt.
	^ ((aFloat at: 1) bitShift: 32) + (aFloat at: 2)
]

{ #category : 'class initialization' }
ASSpurPlusImage class >> initialize [

	GroupHeap := 0.
	GroupSmallInt := 1.
	GroupImmediates := 2
]

{ #category : 'adding' }
ASSpurPlusImage class >> literal: l class: c [

	^ (c bitShift: 3) + (l bitShift: 19) + GroupImmediates
]

{ #category : 'adding' }
ASSpurPlusImage class >> pointer: p class: c [

	^ p
]

{ #category : 'adding' }
ASSpurPlusImage class >> smallInteger: anInteger [

	^ anInteger << 3 + GroupSmallInt
]
