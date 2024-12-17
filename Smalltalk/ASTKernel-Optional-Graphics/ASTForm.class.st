Class {
	#name : #ASTForm,
	#superclass : #ASTObject,
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
ASTForm class >> over [
	^ 3
]

{ #category : #'as yet unclassified' }
ASTForm class >> paint [
	^ 25
]

{ #category : #'as yet unclassified' }
ASTForm class >> width: w height: h depth: d [
	^ self basicNew setWidth: w height: h depth: d
]

{ #category : #'as yet unclassified' }
ASTForm >> beDisplayDepth: d [
	"Install myself as the Display. Drawing onto me will then cause the screen or window to be updated."

	| screenExtent |
	screenExtent := self primScreenSize.
	self
		setWidth: (screenExtent instVarAt: 1)
		height: (screenExtent instVarAt: 2)
		depth: d.
	ASTSystem specialObjectsArray at: 15 put: self	"make this Form the Display"
]

{ #category : #'as yet unclassified' }
ASTForm >> bits [
	^ bits
]

{ #category : #'as yet unclassified' }
ASTForm >> copyX: x y: y width: w height: h [
	"Answer a new form containing given rectangular portion of this form."

	| result |
	result := ASTForm basicNew setWidth: w height: h depth: depth.
	ASTBitBlt new
		sourceForm: self;
		destForm: result;
		sourceX: x y: y;
		width: w height: h;
		copyBits.
	^ result
]

{ #category : #'as yet unclassified' }
ASTForm >> depth [
	^ depth
]

{ #category : #'as yet unclassified' }
ASTForm >> drawForm: aForm x: x y: y rule: anInteger [
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
ASTForm >> fillRectX: x y: y w: w h: h [
	"Fill the given rectangle with the current fill color."

	bitBlt
		destX: x
			y: y
			width: w
			height: h;
		copyBits
]

{ #category : #'as yet unclassified' }
ASTForm >> height [
	^ height
]

{ #category : #'as yet unclassified' }
ASTForm >> primScreenSize [
	"Answer the actual screen size. In Pharo Candle, this will be an Association object since Pharo Candle doesn't have Points."

	<primitive: 106>
	self primitiveFailed
]

{ #category : #'as yet unclassified' }
ASTForm >> setColorR: r g: g b: b [
	"Set the fill color for rectangle drawing operations."

	bitBlt fillR: r g: g b: b
]

{ #category : #'as yet unclassified' }
ASTForm >> setWidth: w height: h depth: d [
	| wordsPerLine |
	wordsPerLine := (w * d + 31) // 32.
	bits := ASTByteArray new: 4 * wordsPerLine * h.
	width := w.
	height := h.
	depth := d.
	bitBlt := ASTBitBlt new
		destForm: self;
		fillR: 255 g: 0 b: 0	"default color"
]

{ #category : #'as yet unclassified' }
ASTForm >> width [
	^ width
]
