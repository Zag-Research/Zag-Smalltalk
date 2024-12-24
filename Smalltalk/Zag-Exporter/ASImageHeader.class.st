"
I am storage that will be exported to a .image file that will be loaded by a Zag runtime
"
Class {
	#name : 'ASImageHeader',
	#superclass : 'ASStorage',
	#category : 'Zag-Exporter-Exporting',
	#package : 'Zag-Exporter',
	#tag : 'Exporting'
}

{ #category : 'adding' }
ASImageHeader class >> defaultAddress [

	^ ASSubHeap defaultAddress - 16r100
]

{ #category : 'adding' }
ASImageHeader class >> defaultSize [

	^ 7
]

{ #category : 'accessing' }
ASImageHeader class >> extension [

	^ '.header'
]

{ #category : 'as yet unclassified' }
ASImageHeader >> codeAddresses: pointer [

	storage at: 7 put: pointer
]

{ #category : 'as yet unclassified' }
ASImageHeader >> dispatchTable: pointer [

	storage at: 6 put: pointer
]

{ #category : 'initialization' }
ASImageHeader >> initializeStorage [

	freeIndex := storage size + 1.
	storage at: 1 put: 16r6567616D4967615A " magic "
]

{ #category : 'private - testing' }
ASImageHeader >> send: selector to: object [

	storage at: 2 put: object.
	storage at: 3 put: selector
]

{ #category : 'as yet unclassified' }
ASImageHeader >> symbolTable: pointer [

	storage at: 5 put: pointer
]
