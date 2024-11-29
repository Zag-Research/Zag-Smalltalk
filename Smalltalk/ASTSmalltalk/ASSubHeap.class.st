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

{ #category : 'accessing' }
ASSubHeap class >> extension [

	^ '.heap'
]

{ #category : 'instance creation' }
ASSubHeap class >> size: anExporter [

	^ 128 * 1024 / 8
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

	largeHeapObjects size = 0 ifTrue: [ ^self ].
	self
		add: [  ]
		arrayBlock: [ :i | largeHeapObjects at: i ]
		ivSize: 0
		arraySize: largeHeapObjects size
		class: ClassDoubleWordArray
]
