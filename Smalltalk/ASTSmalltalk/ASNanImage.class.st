"
I write out a NaN-encoded image
"
Class {
	#name : 'ASNanImage',
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

{ #category : 'as yet unclassified' }
ASNanImage class >> convertNumber: aNumber [

	^ self smallInteger: aNumber
]

{ #category : 'class initialization' }
ASNanImage class >> initialize [

	GroupImmediates := 16rfff0000000000000.
	GroupHeapThunk := 16rfff5000000000000.
	GroupNonLocalThunk := 16rfff6000000000000.
	GroupHeap := 16rfff7000000000000.
	GroupSmallInt0 := 16rfffc000000000000.
	SpecialClassGroups := {
		                      "(ClassThunkReturnSelf
		                       -> [ :p | GroupNonLocalThunk + p ]).
		                      (ClassThunkReturnNil
		                       -> [ :p | GroupNonLocalThunk + p + 3 ])" }
		                      asDictionary
]

{ #category : 'as yet unclassified' }
ASNanImage class >> smallInteger: anInteger [

	^ GroupSmallInt0 + anInteger
]

{ #category : 'adding' }
ASNanImage >> literal: l class: c [

	^ GroupImmediates + (c bitShift: 32) + l
]

{ #category : 'adding' }
ASNanImage >> pointer: p class: c [

	SpecialClassGroups at: c ifPresent: [ :aBlock | ^ aBlock value: p ].
	^ GroupHeap + p
]

{ #category : 'adding' }
ASNanImage >> putFloat: aFloat [

	^ self putRawInt: ((aFloat at: 1) bitShift: 32) + (aFloat at: 2)
]

{ #category : 'adding' }
ASNanImage >> putSmallInteger: anInteger [

	^ self putRawInt: (self class smallInteger: anInteger)
]
