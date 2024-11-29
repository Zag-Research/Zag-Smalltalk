"
I represemt a sub-heap. I am created with a DoubleWordArray that holds the values to end up in memory.

I know how to add values to my heap, and how to output to a file with the proper name.
"
Class {
	#name : 'ASStorage',
	#superclass : 'Object',
	#instVars : [
		'storage',
		'address',
		'maxSize',
		'exporter',
		'freeIndex'
	],
	#category : 'ASTSmalltalk-Image',
	#package : 'ASTSmalltalk',
	#tag : 'Image'
}

{ #category : 'accessing' }
ASStorage class >> extension [

	^ self subclassResponsibility
]

{ #category : 'instance creation' }
ASStorage class >> new [

	^ self error: 'use new:at:exporter:'
]

{ #category : 'instance creation' }
ASStorage class >> new: size at: address exporter: anExporter [

	^ self basicNew exporter: anExporter address: address size: size
]

{ #category : 'accessing' }
ASStorage >> add: iBlock arrayBlock: aBlock ivSize: ivSize arraySize: arraySize class: class [

	| save size |
	size := ivSize + arraySize.
	size > maxSize ifTrue: [
		^ self
			  addBig: iBlock
			  arrayBlock: aBlock
			  instVars: ivSize
			  array: arraySize
			  class: class ].
	save := freeIndex.
	freeIndex := freeIndex + size + 1.
	storage at: freeIndex put: (self
			 headerSize: size
			 format:
			 (self formatClass: class ivSize: ivSize arraySize: arraySize)
			 hash: save * 8 + address
			 class: class).
	1 to: ivSize do: [ :i | storage at: save + i put: (iBlock cull: i) ].
	1 to: arraySize do: [ :i |
	storage at: save + i + ivSize put: (aBlock cull: i) ].
	^ save - 1 * 8 + address
]

{ #category : 'accessing' }
ASStorage >> add: size do: aBlock [

	| save |
	save := freeIndex.
	freeIndex := freeIndex + size.
	1 to: size do: [ :i | storage at: save + i put: (aBlock cull: i) ].
	^ save * 8 + address
]

{ #category : 'accessing' }
ASStorage >> addBig: iBlock arrayBlock: aBlock instVars: ivSize array: arraySize class: class [

	self halt
]

{ #category : 'adding' }
ASStorage >> addU64: anInteger [

	storage at: freeIndex put: anInteger.
	freeIndex := freeIndex + 1
]

{ #category : 'accessing' }
ASStorage >> address [

	^ address
]

{ #category : 'accessing' }
ASStorage >> exporter: anImageExporter address: anInteger size: size [

	exporter := anImageExporter.
	address := anInteger.
	storage := DoubleWordArray new: size.
	self initializeStorage
]

{ #category : 'accessing' }
ASStorage >> formatClass: class ivSize: ivSize arraySize: arraySize [

	self halt
]

{ #category : 'accessing' }
ASStorage >> headerSize: size format: format hash: hash class: class [

	self halt
]

{ #category : 'initialization' }
ASStorage >> initialize [

	super initialize.
	maxSize := 4093
]

{ #category : 'initialization' }
ASStorage >> initializeStorage [

	^ self subclassResponsibility
]

{ #category : 'output' }
ASStorage >> prepareForOutput [

]

{ #category : 'output' }
ASStorage >> toStream: aStream [

	1 to: freeIndex - 1 do: [ :i | | word|
		word := storage at: i.
		1 to: 8 do: [ :b | aStream nextPut: (word byteAt: b) ] ]
]
