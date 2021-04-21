Class {
	#name : #PCForm,
	#superclass : #PCObject,
	#instVars : [
		'bits',
		'width',
		'height',
		'depth',
		'bitBlt'
	],
	#category : #'ASTKernel-Optional-Graphics'
}

{ #category : #'as yet unclassified' }
PCForm class >> over [
	^ 3
]

{ #category : #'as yet unclassified' }
PCForm class >> paint [
	^ 25
]

{ #category : #'as yet unclassified' }
PCForm class >> width: w height: h depth: d [
	^ self basicNew setWidth: w height: h depth: d
]

{ #category : #'as yet unclassified' }
PCForm >> beDisplayDepth: d [
	"Install myself as the Display. Drawing onto me will then cause the screen or window to be updated."

	| screenExtent |
	screenExtent := self primScreenSize.
	self
		setWidth: (screenExtent instVarAt: 1)
		height: (screenExtent instVarAt: 2)
		depth: d.
	PCSystem specialObjectsArray at: 15 put: self	"make this Form the Display"
]

{ #category : #'as yet unclassified' }
PCForm >> bits [
	^ bits
]

{ #category : #'as yet unclassified' }
PCForm >> copyX: x y: y width: w height: h [
	"Answer a new form containing given rectangular portion of this form."

	| result |
	result := PCForm basicNew setWidth: w height: h depth: depth.
	PCBitBlt new
		sourceForm: self;
		destForm: result;
		sourceX: x y: y;
		width: w height: h;
		copyBits.
	^ result
]

{ #category : #'as yet unclassified' }
PCForm >> depth [
	^ depth
]

{ #category : #'as yet unclassified' }
PCForm >> drawForm: aForm x: x y: y rule: anInteger [
	"Fill the given rectangle with the current fill color."

	| oldFill |
	oldFill := bitBlt fillWords.
	bitBlt
		sourceForm: aForm;
		destX: x
			y: y
			width: aForm width
			height: aForm height;
		rule: anInteger;
		copyBits.
	bitBlt sourceForm: nil.
	bitBlt fillWords: oldFill
]

{ #category : #'as yet unclassified' }
PCForm >> fillRectX: x y: y w: w h: h [
	"Fill the given rectangle with the current fill color."

	bitBlt
		destX: x
			y: y
			width: w
			height: h;
		copyBits
]

{ #category : #'as yet unclassified' }
PCForm >> height [
	^ height
]

{ #category : #'as yet unclassified' }
PCForm >> primScreenSize [
	"Answer the actual screen size. In Pharo Candle, this will be an Association object since Pharo Candle doesn't have Points."

	<primitive: 106>
	self primitiveFailed
]

{ #category : #'as yet unclassified' }
PCForm >> setColorR: r g: g b: b [
	"Set the fill color for rectangle drawing operations."

	bitBlt fillR: r g: g b: b
]

{ #category : #'as yet unclassified' }
PCForm >> setWidth: w height: h depth: d [
	| wordsPerLine |
	wordsPerLine := (w * d + 31) // 32.
	bits := PCByteArray new: 4 * wordsPerLine * h.
	width := w.
	height := h.
	depth := d.
	bitBlt := PCBitBlt new
		destForm: self;
		fillR: 255 g: 0 b: 0	"default color"
]

{ #category : #'as yet unclassified' }
PCForm >> width [
	^ width
]
