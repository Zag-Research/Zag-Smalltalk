"
I have the knowledge of how to encode values for SpurPlus images
"
Class {
	#name : 'ASSpurPlusImage',
	#superclass : 'ASImage',
	#classVars : [
		'GroupHeap',
		'GroupImmediates',
		'GroupSmallInt0'
	],
	#category : 'ASTSmalltalk-Image',
	#package : 'ASTSmalltalk',
	#tag : 'Image'
}

{ #category : 'class initialization' }
ASSpurPlusImage class >> initialize [

	GroupHeap := 0.
	GroupSmallInt0 := 1.
	GroupImmediates := 2
]

{ #category : 'adding' }
ASSpurPlusImage >> double: aFloat [

	self halt.
	^ ((aFloat at: 1) bitShift: 32) + (aFloat at: 2)
]

{ #category : 'adding' }
ASSpurPlusImage >> literal: l class: c [

	^ (c bitShift: 3) + (l bitShift: 19) + GroupImmediates
]

{ #category : 'adding' }
ASSpurPlusImage >> pointer: p class: c [

	^ p
]

{ #category : 'adding' }
ASSpurPlusImage >> smallInteger: anInteger [

	^ anInteger << 3 + GroupSmallInt0
]
