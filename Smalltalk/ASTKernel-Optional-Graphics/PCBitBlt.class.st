Class {
	#name : #PCBitBlt,
	#superclass : #PCObject,
	#instVars : [
		'destForm',
		'sourceForm',
		'fillWords',
		'rule',
		'destX',
		'destY',
		'width',
		'height',
		'sourceX',
		'sourceY',
		'clipX',
		'clipY',
		'clipWidth',
		'clipHeight',
		'colorMap'
	],
	#category : #'ASTKernel-Optional-Graphics'
}

{ #category : #'as yet unclassified' }
PCBitBlt >> clipX: x y: y width: w height: h [
	"Set my clipping boundaries. Setting the clipping bounds is optional."

	clipX := x.
	clipY := y.
	clipWidth := w.
	clipHeight := h
]

{ #category : #'as yet unclassified' }
PCBitBlt >> copyBits [
	"Primitive. Perform the movement of bits from the source form to the destination form. Fail if any variables are not of the right type or if the combination rule is not implemented."

	<primitive: 96>
	self primitiveFailed
]

{ #category : #'as yet unclassified' }
PCBitBlt >> copyBitsTranslucent: factor [
	"This entry point to BitBlt supplies an extra argument to specify translucency for operations 30 and 31.  The argument must be an integer between 0 and 255."

	<primitive: 96>
	self primitiveFailed
]

{ #category : #'as yet unclassified' }
PCBitBlt >> destForm: aForm [
	"Set my destination Form."

	destForm := aForm.
	clipX := 0.
	clipY := 0.
	clipWidth := aForm width.
	clipHeight := aForm height
]

{ #category : #'as yet unclassified' }
PCBitBlt >> destX: x y: y width: w height: h [
	"Set the destination rectangle for this operation."

	destX := x.
	destY := y.
	width := w.
	height := h
]

{ #category : #'as yet unclassified' }
PCBitBlt >> fillR: r g: g b: b [
	"Set my fill color to the given RGB value, where r, g, and b are in the range 0-255. The destination form must be set before calling this method, since the pixel pattern created depends on the destination depth."

	"Note: The ranges of r, g, and b are not checked."

	| d pix |
	d := destForm depth.
	d = 8
		ifTrue: [ pix := 41 + (r // 37 * 36) + (g // 37 * 6) + (b // 37).
			^ self fillWords: (PCWordArray with: 16r01010101 * pix) ].
	d = 16
		ifTrue: [ pix := (r // 8 bitShift: 10) + (g // 8 bitShift: 5) + (b // 8).
			^ self fillWords: (PCWordArray with: (pix bitShift: 16) + pix) ].
	"5 bits each of r, g, b"
	d = 32
		ifTrue: [ ^ self
				fillWords: (PCWordArray with: (r bitShift: 16) + (g bitShift: 8) + b) ].
	self error: 'color is supported only for depths 8, 16, and 32'
]

{ #category : #'as yet unclassified' }
PCBitBlt >> fillWords [
	"Answer the array of pixel words using for filling with a color."

	^ fillWords
]

{ #category : #'as yet unclassified' }
PCBitBlt >> fillWords: aBitmapOrNil [
	"Set the array of pixel words using for filling with a color."

	fillWords := aBitmapOrNil
]

{ #category : #'as yet unclassified' }
PCBitBlt >> initialize [
	rule := PCForm over.
	sourceX := sourceY := 0.
	destX := destY := 0.
	clipX := clipY := 0.
	clipWidth := clipHeight := 100000
]

{ #category : #'as yet unclassified' }
PCBitBlt >> rule: anInteger [
	"Set the combination rule, an integer between 0 and 34 that determines how pixels are combined in this operation."

	rule := anInteger
]

{ #category : #'as yet unclassified' }
PCBitBlt >> sourceForm: aForm [
	"Set my source and destination forms. The source form may be nil if filling with a color."

	sourceForm := aForm
]

{ #category : #'as yet unclassified' }
PCBitBlt >> sourceX: x y: y [
	"Set the top-left corner of the destination rectangle for this operation."

	sourceX := x.
	sourceY := y
]

{ #category : #'as yet unclassified' }
PCBitBlt >> width: w height: h [
	"Set the width and height for this operation."

	width := w.
	height := h
]
