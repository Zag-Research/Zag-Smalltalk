Class {
	#name : #ASImageTest,
	#superclass : #TestCase,
	#instVars : [
		'imageWriter'
	],
	#category : #'ASTSmalltalk-Tests-Tests'
}

{ #category : #tests }
ASImageTest >> array: aBlock [
	^ (self stream: aBlock) asArray
]

{ #category : #tests }
ASImageTest >> setUp [
	super setUp.
	imageWriter := ASImage new
]

{ #category : #tests }
ASImageTest >> stream: aBlock [
	^ ByteArray streamContents: aBlock
]

{ #category : #tests }
ASImageTest >> testAdditions [
	| a1 a1o |
	self assert: (imageWriter object: 42) equals: 16r7ff8000000000155.
	self assert: (imageWriter object: true) equals: 16r7ff800000000000c.
	self assert: (imageWriter object: false) equals: 16r7ff8000000000003.
	self assert: (imageWriter object: nil) equals: 16r7ff800000000000a.
	self assert: (imageWriter object: $A) equals: 16r7ff800000000020f.
	self assert: (imageWriter object: #foo) equals: 16r7ff800000000009e.
	self assert: (imageWriter object: #value:) equals: 16r7ff800001000000e.
	self assert: (imageWriter object: #value:value:) equals: 16r7ff8000020000016.
	self assert: (imageWriter object: 42.0) equals: 16r4045000000000000.
	self assert: imageWriter offset equals: 0.
	a1 := #(1 2 3).
	a1o := imageWriter object: a1.
	self assert: imageWriter offset equals: 40.
	self assert: (imageWriter object: a1) equals: a1o.
	self assert: imageWriter offset equals: 40.


]

{ #category : #tests }
ASImageTest >> testBasicImage [
	| image |
	image := ByteArray streamContents: [ :s | imageWriter writeImageOn: s ].
	self assert: image size equals: 364.

]

{ #category : #tests }
ASImageTest >> testWriteRawOn [
	self assert: (self array: [: s | 1 writeRawOn: s ]) equals: #(1 0 0 0 0 0 0 0).
	self assert: (self array: [: s | -1 writeRawOn: s ]) equals: #(255 255 255 255 255 255 255 255).
	self assert: (self array: [: s | 256 writeRawOn: s ]) equals: #(0 1 0 0 0 0 0 0).
	self assert: (self array: [: s | -256 writeRawOn: s ]) equals: #(0 255 255 255 255 255 255 255).

]
