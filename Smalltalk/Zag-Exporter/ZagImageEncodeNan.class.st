"
I encode values the NaN way
"
Class {
	#name : 'ZagImageEncodeNan',
	#superclass : 'ZagImageEncoder',
	#category : 'Zag-Exporter-Encoding',
	#package : 'Zag-Exporter',
	#tag : 'Encoding'
}

{ #category : 'converting' }
ZagImageEncodeNan class >> boolean: value [

	^ self literal: value asBit class: ClassFalse + value asBit
]

{ #category : 'converting' }
ZagImageEncodeNan class >> double: n for: ignored [

	^ ('16r' , n hex) asNumber
]

{ #category : 'private - constants' }
ZagImageEncodeNan class >> groupHeap [

	^ 16rfff7000000000000
]

{ #category : 'private - constants' }
ZagImageEncodeNan class >> groupImmediates [

	^ 16rfff0000000000000
]

{ #category : 'private - constants' }
ZagImageEncodeNan class >> groupSmallInt0 [

	^ 16rfffc000000000000
]

{ #category : 'converting' }
ZagImageEncodeNan class >> literal: l class: c [

	^ self groupImmediates + l + (c bitShift: 32)
]

{ #category : 'converting' }
ZagImageEncodeNan class >> pointer: p [

	^ self groupHeap + p
]

{ #category : 'converting' }
ZagImageEncodeNan class >> smallInteger: n [

	^ self groupSmallInt0 + n
]

{ #category : 'converting' }
ZagImageEncodeNan class >> undefinedObject [

	^ self literal: 16rffffffff class: ClassUndefinedObject
]
