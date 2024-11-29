"
I have the knowledge of how to encode values for SpurPlus images
"
Class {
	#name : 'ASZagImage',
	#superclass : 'ASImageExporter',
	#classVars : [
		'GroupHeap',
		'GroupImmediates',
		'TagSmallInteger'
	],
	#category : 'ASTSmalltalk-Image',
	#package : 'ASTSmalltalk',
	#tag : 'Image'
}

{ #category : 'accessing' }
ASZagImage class >> boolean: value [

	^ self literal: 0 class: ClassFalse + value asBit
]

{ #category : 'converting' }
ASZagImage class >> double: aFloat for: anImage [

	| fullFloat tag |
	fullFloat := ((aFloat at: 1) bitShift: 32) + (aFloat at: 2).
	tag := fullFloat >> 60 + 2.
	tag & 6 = 0 ifTrue: [ ^ anImage doubleAsObject: fullFloat ].
	^ fullFloat & 16rfffffffffffffff << 4 + tag
]

{ #category : 'class initialization' }
ASZagImage class >> initialize [

	GroupHeap := 0.
	GroupImmediates := 1.
	TagSmallInteger := ClassSmallInteger << 3 + GroupImmediates .
]

{ #category : 'adding' }
ASZagImage class >> literal: l class: c [

	^ (l bitShift: 8) + (c bitShift: 3) + GroupImmediates
]

{ #category : 'adding' }
ASZagImage class >> pointer: p class: c [

	^ p
]

{ #category : 'adding' }
ASZagImage class >> smallInteger: anInteger [

	^ (anInteger bitAnd: 16r00ffffffffffffff) << 8 + TagSmallInteger
]

{ #category : 'instance creation' }
ASZagImage >> put: anObject [

	anObject astImageObject: self
]
