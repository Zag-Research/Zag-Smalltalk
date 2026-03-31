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

	^ self literal: 0 class: ClassFalse + value asBit
]

{ #category : 'converting' }
ZagImageEncodeNan class >> double: n for: ignored [

	^ ('16r' , n hex) asNumber
]

{ #category : 'converting' }
ZagImageEncodeNan class >> literal: l class: c [

	^ 16r7fef000000000000 + l + (c bitShift: 48)
]

{ #category : 'magic' }
ZagImageEncodeNan class >> magic [

	^ 16r614E616D4967615A " ZagImaNa"
]

{ #category : 'converting' }
ZagImageEncodeNan class >> pointer: p [

	^ 16r7fff000000000000 + p
]

{ #category : 'converting' }
ZagImageEncodeNan class >> smallInteger: n [

	^ 16rfff8000000000000 + (n bitAnd: 16r7ffffffffffff)
]

{ #category : 'converting' }
ZagImageEncodeNan class >> symbolArity: arity hash: hash [ 
	^ self literal: (hash bitShift: 8) + arity class: ClassSymbol
]

{ #category : 'converting' }
ZagImageEncodeNan class >> undefinedObject [

	^ self literal: 0 class: 16
]
