"
I encode values the Zag way, which is an extension of SPUR
"
Class {
	#name : 'ZagImageEncodeMix',
	#superclass : 'ZagImageEncoder',
	#category : 'Zag-Exporter-Encoding',
	#package : 'Zag-Exporter',
	#tag : 'Encoding'
}

{ #category : 'converting' }
ZagImageEncodeMix class >> double: aFloat for: anImage [

	| fullFloat tag |
	fullFloat := ((aFloat at: 1) bitShift: 32) + (aFloat at: 2).
	tag := (fullFloat bitSize: 64 rotate: 5) - 3.
	(tag & 6) = 4 ifTrue: [ ^ tag ].
	^ anImage objectHeapFloat: fullFloat
]

{ #category : 'class initialization' }
ZagImageEncodeMix class >> initialize [

	self initializeAllConstants
]

{ #category : 'converting' }
ZagImageEncodeMix class >> literal: l class: c [

	^ (l bitShift: 3) + (c bitShift: 59)
]

{ #category : 'magic' }
ZagImageEncodeMix class >> magic [

	^ 16r694D616D4967615A " ZagImaMi"
]

{ #category : 'converting' }
ZagImageEncodeMix class >> smallInteger: anInteger [

	^ ((anInteger bitAnd: 16r3fffffffffffffff) bitShift: 2) + 2
]

{ #category : 'convertin' }
ZagImageEncodeMix class >> symbolArity: arity hash: hash [

	^ self literal: (hash bitShift: 5) + arity class: classSymbol
]

{ #category : 'converting' }
ZagImageEncodeMix class >> undefinedObject [

	^ 0
]
