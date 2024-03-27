"
I write out a NaN-encoded image
"
Class {
	#name : 'ASTagImage',
	#superclass : 'ASImage',
	#classVars : [
		'GroupHeap',
		'GroupHeapThunk',
		'GroupImmediates',
		'GroupNonLocalThunk',
		'GroupSmallInt0',
		'SpecialClassGroups'
	],
	#category : 'ASTSmalltalk-Image',
	#package : 'ASTSmalltalk',
	#tag : 'Image'
}

{ #category : 'class initialization' }
ASTagImage class >> initialize [

	GroupImmediates := 2.
	GroupHeap := 0.
	GroupSmallInt0 := 1.

]

{ #category : 'adding' }
ASTagImage >> literal: l class: c [

	^ (c bitShift: 3) + (l bitShift: 19) + GroupImmediates
]

{ #category : 'adding' }
ASTagImage >> pointer: p class: c [

	^ p
]

{ #category : 'adding' }
ASTagImage >> putFloat: aFloat [
self halt.
	^ self putRawInt: ((aFloat at: 1) bitShift: 32) + (aFloat at: 2)
]

{ #category : 'adding' }
ASTagImage >> putSmallInteger: anInteger [

	^ self putRawInt: anInteger << 3 + GroupSmallInt0
]
