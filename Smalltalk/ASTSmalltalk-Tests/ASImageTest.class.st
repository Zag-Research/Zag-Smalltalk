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
ASImageTest >> testBasicImage [
	| image |
	image := ByteArray streamContents: [ :s | imageWriter writeImageOn: s ].
	self assert: image size equals: 312.

]

{ #category : #tests }
ASImageTest >> testWriteRawOn [
	self assert: (self array: [: s | 1 writeRawOn: s ]) equals: #(1 0 0 0 0 0 0 0).
	self assert: (self array: [: s | -1 writeRawOn: s ]) equals: #(255 255 255 255 255 255 255 255).
	self assert: (self array: [: s | 256 writeRawOn: s ]) equals: #(0 1 0 0 0 0 0 0).
	self assert: (self array: [: s | -256 writeRawOn: s ]) equals: #(0 255 255 255 255 255 255 255).

]
