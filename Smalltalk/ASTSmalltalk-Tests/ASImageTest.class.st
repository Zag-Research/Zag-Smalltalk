Class {
	#name : 'ASImageTest',
	#superclass : 'TestCase',

	#instVars : [
		'imageWriter',
    'minImageSize'
  ],
	#pools : [
		'ASZagConstants'
	],
	#category : 'ASTSmalltalk-Tests-Tests',
	#package : 'ASTSmalltalk-Tests',
	#tag : 'Tests'
}

{ #category : 'tests' }
ASImageTest >> array: aBlock [
	^ (self stream: aBlock) asArray
]

{ #category : 'tests' }
ASImageTest >> image [

	^ ByteArray streamContents: [ :s | imageWriter writeImageOn: s ]
]

{ #category : 'running' }
ASImageTest >> setUp [
	super setUp.
	imageWriter := ASImage new.
	minImageSize := 2686.

]

{ #category : 'tests' }
ASImageTest >> stream: aBlock [
	^ ByteArray streamContents: aBlock
]

{ #category : 'tests' }
ASImageTest >> testAdditions [
	| a1 a1o a2 a2o |
	self assert: (imageWriter imageObject: 42) equals: 16r7ff8000000000155.
	self assert: (imageWriter imageObject: 1) equals: 16r7ff800000000000d.
	self assert: (imageWriter imageObject: true) equals: 16r7ff800000000002b.
	self assert: (imageWriter imageObject: false) equals: 16r7ff800000000001a.
	self assert: (imageWriter imageObject: nil) equals: 16r7ff800000000003c.
	self assert: (imageWriter imageObject: $A) equals: 16r7ff800000000020f.
	self assert: (imageWriter imageObject: #value) equals: 16r7ff8000000000036.
	self assert: (imageWriter imageObject: #value:) equals: 16r7ff800001000003e.
	self assert: (imageWriter imageObject: #value:value:) equals: 16r7ff8000020000046.
	self assert: (imageWriter imageObject: 42.0) equals: 16r4045000000000000.
	self assert: (imageWriter imageObject: 2.0) equals: 16r4000000000000000.
	self assert: (imageWriter imageObject: 6.25e-2) equals: 16r3fb0000000000000.
	self assert: (imageWriter imageObject: #==) equals: 16r7ff8000010000066.
	self assert: imageWriter offset equals: 16.
	a1 := #(1 true nil value).
	a1o := imageWriter imageObject: a1.
	self assert: a1o equals: 16r7ff8001000000010.
	self assert: imageWriter offset equals: 56.
	self assert: (imageWriter imageObject: a1) equals: a1o.
	self assert: imageWriter offset equals: 56.
	self assert: (self image at64: 24) equals: 16r7ff800000000000d.
	self assert: (self image at64: 32) equals: 16r7ff800000000002b.
	self assert: (self image at64: 40) equals: 16r7ff800000000003c.
	self assert: (self image at64: 48) equals: 16r7ff800000000000e.
	a2 := { a1 . #value: }.
	a2o := imageWriter imageObject: a2.
	self assert: a2o equals: 16r7ff8001000000038.
	self assert: (self image at64: 64) equals: a1o.
	self assert: (self image at64: 72) equals: 16r7ff8000010000016.


]

{ #category : 'tests' }
ASImageTest >> testClasses [
	| system object symbol class |
	system := imageWriter class: ASTSystem.
	self assert: (imageWriter class: ASTSystem) equals: system.
	object := imageWriter class: ASTObject.
	self assert: (imageWriter class: Object) equals: object.
	symbol := imageWriter class: Symbol.
	self assert: (imageWriter class: ASTSymbol) equals: symbol.
	class := imageWriter class: Class class.
	self assert: (imageWriter class: ASTClass class) equals: class.


]

{ #category : 'tests' }
ASImageTest >> testCountMethodArgSizes [

	| argSizes |
	argSizes := ASExploreImage argumentsPerMethod.
	Transcript
		show: argSizes;
		cr
]

{ #category : 'tests' }
ASImageTest >> testEmptyImage [
	self assert: self image size equals: minImageSize
]

{ #category : 'tests' }
ASImageTest >> testHeader [
	self assert: (imageWriter header: #(isImmutable)) equals: 16r800000000000.

]

{ #category : 'tests' }
ASImageTest >> testMethods [
	| start |
	start := imageWriter method: ASTSystem class >> #start.
	self assert: imageWriter methods size equals: 1.
	imageWriter method: ASTSystem class >> #start.
	self assert: imageWriter methods size equals: 1.
	self assert: start class equals: ASImage."ASTCode."
]
{ #category : 'running' }
ASImageTest >> image: aBlock equals: value [

	| result |
	result := ByteArray streamContents: [ :s |
		          aBlock value: (ASNanImage test on: s) ].
	value isInteger
		ifTrue: [
			self
				assert: (self u64From: result) first hexString
				equals: value hexString ]
		ifFalse: [
			self
				assert: ((self u64From: result) collect: #hexString)
				equals: (value collect: #hexString) ]
]

{ #category : 'running' }
ASImageTest >> setUp [

	super setUp.

	ASAllocationInfo reset
]

{ #category : 'tests' }
ASImageTest >> testRepeatedImage [
	self assert: self image equals: self image.

]

{ #category : 'tests' }
ASImageTest >> testWriteRawOn [
	self assert: (self array: [: s | 1 writeRawOn: s ]) equals: #(1 0 0 0 0 0 0 0).
	self assert: (self array: [: s | -1 writeRawOn: s ]) equals: #(255 255 255 255 255 255 255 255).
	self assert: (self array: [: s | 256 writeRawOn: s ]) equals: #(0 1 0 0 0 0 0 0).
	self assert: (self array: [: s | -256 writeRawOn: s ]) equals: #(0 255 255 255 255 255 255 255).
	self assert: (self array: [: s | 258*256+3*256+4*256+5*256+6*256+7*256+8 writeRawOn: s ]) equals: #(8 7 6 5 4 3 2 1).

ASImageTest >> threaded: ast equals: converted [

	| image map |
	map := ASZagConstants constantMap.
	image := self imageClass test on: (ReadWriteStream on: #[  ]).
	ASImageThreadedOutput new
		context: (ASImageFileContext with: image);
		generateCode: ast.
	converted withIndexDo: [ :each :idx |
		| code |
		code := image rawIntAt: idx.
		each isSymbol
			ifTrue: [
				self
					assert: code = (map at: each)
					description: [
					'expected: ' , each , ' but found: ' , code hexString ] ]
			ifFalse: [
				each isNumber
					ifTrue: [
					self assert: code equals: (self imageClass convertNumber: each) ]
					ifFalse: [ self assert: code equals: each ] ] ]
]

{ #category : 'running' }
ASImageTest >> u64From: aByteArray [

	^ ((aByteArray groupsOf: 8) collect: [ :group |
		  group reversed
			  inject: 0
			  into: [ :t :each | (t bitShift: 8) + each ] ]) asArray
]
