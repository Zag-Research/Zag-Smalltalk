"
I have the knowledge of how to encode values for SpurPlus images
"
Class {
	#name : 'ZagImageEncodeZag',
	#superclass : 'ZagImageEncoder',
	#classInstVars : [
		'constants'
	],
	#category : 'Zag-Exporter-Encoding',
	#package : 'Zag-Exporter',
	#tag : 'Encoding'
}

{ #category : 'accessing' }
ZagImageEncodeZag class >> boolean: value [

	^ self literal: 0 class: self classFalse + value asBit
]

{ #category : 'private - temporary' }
ZagImageEncodeZag class >> classCharacter [

	^ 14
]

{ #category : 'private - temporary' }
ZagImageEncodeZag class >> classFalse [

	^ 10
]

{ #category : 'private - temporary' }
ZagImageEncodeZag class >> classSmallInteger [

	^ 12
]

{ #category : 'private - temporary' }
ZagImageEncodeZag class >> classString [

	^ 31
]

{ #category : 'private - temporary' }
ZagImageEncodeZag class >> classSymbol [

	^ 13
]

{ #category : 'class initialization' }
ZagImageEncodeZag class >> constants [

	^ constants
]

{ #category : 'converting' }
ZagImageEncodeZag class >> double: aFloat for: anImage [

	| fullFloat tag |
	fullFloat := ((aFloat at: 1) bitShift: 32) + (aFloat at: 2).
	tag := fullFloat >> 60 + 2.
	tag & 6 = 0 ifTrue: [ ^ anImage objectFloat: fullFloat ].
	^ fullFloat & 16rfffffffffffffff << 4 + tag
]

{ #category : 'private - temporary' }
ZagImageEncodeZag class >> groupImmediates [

	^ 1
]

{ #category : 'adding' }
ZagImageEncodeZag class >> literal: l class: c [

	^ (l bitShift: 8) + (c bitShift: 3) + self groupImmediates
]

{ #category : 'converting' }
ZagImageEncodeZag class >> pointer: p [

	^ p
]

{ #category : 'class initialization' }
ZagImageEncodeZag class >> saveConstants [

	^ constants := super zagConstants: self
]

{ #category : 'converting' }
ZagImageEncodeZag class >> smallInteger: anInteger [

	^ self
		  literal: (anInteger bitAnd: 16r00ffffffffffffff)
		  class: self classSmallInteger
]

{ #category : 'converting' }
ZagImageEncodeZag class >> undefinedObject [

	^ 0
]
