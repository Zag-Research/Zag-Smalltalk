"
I encode values the Zag way, which is an extension of SPUR
"
Class {
	#name : 'ZagImageEncodeZag',
	#superclass : 'ZagImageEncoder',
	#category : 'Zag-Exporter-Encoding',
	#package : 'Zag-Exporter',
	#tag : 'Encoding'
}

{ #category : 'converting' }
ZagImageEncodeZag class >> boolean: value [

	^ self literal: 0 class: ClassFalse + value asBit
]

{ #category : 'converting' }
ZagImageEncodeZag class >> double: aFloat for: anImage [

	| fullFloat tagged |
	fullFloat := ((aFloat at: 1) bitShift: 32) + (aFloat at: 2).
	tagged := (fullFloat bitSize: 64 rotate: 5) + 5.
	tagged & 6 = 4 ifTrue: [ ^ tagged ].
	^ anImage objectHeapFloat: fullFloat
]

{ #category : 'converting' }
ZagImageEncodeZag class >> literal: l class: c [

	^ (l bitShift: 3) + (c bitShift: 59)
]

{ #category : 'magic' }
ZagImageEncodeZag class >> magic [

	^ 16r615A616D4967615A " ZagImaZa"
]

{ #category : 'converting' }
ZagImageEncodeZag class >> pointer: p [

	^ p
]

{ #category : 'converting' }
ZagImageEncodeZag class >> smallInteger: anInteger [

	^ ((anInteger bitAnd: 16r3fffffffffffffff) bitShift: 2) + 2
]

{ #category : 'convertin' }
ZagImageEncodeZag class >> symbolArity: arity hash: hash [ 
	^ self literal: (hash bitShift: 5) + arity + arity class: ClassSymbol
]

{ #category : 'converting' }
ZagImageEncodeZag class >> undefinedObject [

	^ 0
]
