"
I am storage that will be exported to a .heap file that will be loaded by a Zag runtime.
I have a small header, followed by standard heap objects.
"
Class {
	#name : 'ASSubHeap',
	#superclass : 'ASStorage',
	#instVars : [
		'largeHeapObjects'
	],
	#pools : [
		'ASImageExporter'
	],
	#category : 'ASTSmalltalk-Image',
	#package : 'ASTSmalltalk',
	#tag : 'Image'
}

{ #category : 'instance creation' }
ASSubHeap class >> address: anExporter [

	^ anExporter nextSubheapAddress
]

{ #category : 'adding' }
ASSubHeap class >> defaultAddress [

	^ 16r100000000
]

{ #category : 'adding' }
ASSubHeap class >> defaultSize [ 

	^ 16r20000
]

{ #category : 'accessing' }
ASSubHeap class >> extension [

	^ '.heap'
]

{ #category : 'instance creation' }
ASSubHeap class >> size: anExporter [

	^ 128 * 1024 / 8
]

{ #category : 'output' }
ASSubHeap >> at: location with: aCollection do: aBlock [

	| offset |
	offset := location - address / 8.
	1 to: aCollection size do: [ :i |
		storage at: offset + i put: (aBlock value: (aCollection at: i)) ]
]

{ #category : 'testing' }
ASSubHeap >> hasSpaceFor: anInteger [

	^ freeIndex + anInteger < storage size
]

{ #category : 'output' }
ASSubHeap >> header: header size: size [

	| headerLocation |
	headerLocation := freeIndex.
	freeIndex := freeIndex + size + 1.
	storage at: headerLocation put: header.
	^ headerLocation * 8 + address
]

{ #category : 'initialization' }
ASSubHeap >> initializeStorage [

	storage at: 1 put: address.
	freeIndex := 4.
	largeHeapObjects := OrderedCollection new
]

{ #category : 'initialization' }
ASSubHeap >> nextHeap: anAddress [

	storage at: 2 put: anAddress
]

{ #category : 'output' }
ASSubHeap >> prepareForOutput [

	largeHeapObjects size = 0 ifTrue: [ ^ self ].
	self
		register: [ : addr | storage at: 3 put: addr ]
		ivBlock: [  ]
		arrayBlock: [ :i | largeHeapObjects at: i ]
		ivSize: 0
		arraySize: largeHeapObjects size
		class: ClassDoubleWordArray
]
