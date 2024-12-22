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

{ #category : 'accessing' }
ASImageHeader class >> extension [

	^ '.image'
]

{ #category : 'initialization' }
ASImageHeader >> initializeStorage [

	freeIndex := 1
]
