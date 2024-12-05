"
I have the knowledge of how to encode values for NaN images
"
Class {
	#name : 'ZagImageEncodeNan',
	#superclass : 'ZagImageEncoder',
	#category : 'Zag-Exporter-Encoding',
	#package : 'Zag-Exporter',
	#tag : 'Encoding'
}

{ #category : 'accessing' }
ZagImageEncodeNan class >> boolean: value [

	^ self literal: value asBit class: self classFalse + value asBit
]

{ #category : 'converting' }
ZagImageEncodeNan class >> double: n for: ignored [

	^ ('16r' , n hex) asNumber
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

	^ self literal: 16rffffffff class: self classUndefinedObject
]
