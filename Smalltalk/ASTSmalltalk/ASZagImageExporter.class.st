"
I have the knowledge of how to encode values for SpurPlus images
"
Class {
	#name : 'ASZagImageExporter',
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
ASZagImageExporter class >> boolean: value [

	^ self literal: 0 class: ClassFalse + value asBit
]

{ #category : 'converting' }
ASZagImageExporter class >> double: aFloat for: anImage [

	| fullFloat tag |
	fullFloat := ((aFloat at: 1) bitShift: 32) + (aFloat at: 2).
	tag := fullFloat >> 60 + 2.
	tag & 6 = 0 ifTrue: [ ^ anImage objectFloat: fullFloat ].
	^ fullFloat & 16rfffffffffffffff << 4 + tag
]

{ #category : 'class initialization' }
ASZagImageExporter class >> initialize [

	GroupHeap := 0.
	GroupImmediates := 1.
	TagSmallInteger := ClassSmallInteger << 3 + GroupImmediates .
]

{ #category : 'adding' }
ASZagImageExporter class >> literal: l class: c [

	^ (l bitShift: 8) + (c bitShift: 3) + GroupImmediates
]

{ #category : 'converting' }
ASZagImageExporter class >> pointer: p [

	^ p
]

{ #category : 'converting' }
ASZagImageExporter class >> smallInteger: anInteger [

	^ (anInteger bitAnd: 16r00ffffffffffffff) << 8 + TagSmallInteger
]

{ #category : 'converting' }
ASZagImageExporter class >> undefinedObject [

	^ 0
]
