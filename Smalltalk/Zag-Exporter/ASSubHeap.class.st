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
	#category : 'Zag-Exporter-Exporting',
	#package : 'Zag-Exporter',
	#tag : 'Exporting'
}

{ #category : 'adding' }
ASSubHeap class >> defaultAddress [

	^ 16r10000000000
]

{ #category : 'adding' }
ASSubHeap class >> defaultSize [

	^ 16r4000
]

{ #category : 'accessing' }
ASSubHeap class >> extension [

	^ '.heap'
]

{ #category : 'adding' }
ASSubHeap >> addStorage [

	| size nextAddress |
	size := storage size.
	nextAddress := size * 8 + address.
	storage at: 2 put: nextAddress.
	^ self class new exporter: exporter
]

{ #category : 'initialization' }
ASSubHeap >> initializeStorage [

	storage at: 1 put: address.
	freeIndex := 33
]

{ #category : 'output' }
ASSubHeap >> prepareForOutput [

	largeHeapObjects ifNil: [ ^ self ].
	self halt
]
