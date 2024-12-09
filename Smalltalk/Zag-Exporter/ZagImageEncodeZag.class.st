Class {
	#name : 'ZagImageEncodeZag',
	#superclass : 'ZagImageEncoder',
	#category : 'Zag-Exporter-Encoding',
	#package : 'Zag-Exporter',
	#tag : 'Encoding'
}

{ #category : 'converting' }
ZagImageEncodeZag class >> boolean: value [

	^ self literal: 0 class: self classFalse + value asBit
]

{ #category : 'accessing' }
ZagImageEncodeZag class >> constants [

	^ self
]

{ #category : 'converting' }
ZagImageEncodeZag class >> double: aFloat for: anImage [

	| fullFloat tag |
	fullFloat := ((aFloat at: 1) bitShift: 32) + (aFloat at: 2).
	tag := fullFloat >> 60 + 2.
	tag & 6 = 0 ifTrue: [ ^ anImage objectFloat: fullFloat ].
	^ fullFloat & 16rfffffffffffffff << 4 + tag
]

{ #category : 'private - temporary - header' }
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

	super zagConstants: self
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
