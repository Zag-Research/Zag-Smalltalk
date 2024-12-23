"
I am storage that will be exported to a .process file that will be loaded by a Zag runtime.
I have a small header, followed by a Process struct.
"
Class {
	#name : 'ASProcess',
	#superclass : 'ASStorage',
	#category : 'Zag-Exporter-Exporting',
	#package : 'Zag-Exporter',
	#tag : 'Exporting'
}

{ #category : 'accessing' }
ASProcess class >> extension [

	^ '.process'
]

{ #category : 'initialization' }
ASProcess >> initializeStorage [

	storage at: 1 put: address.
	freeIndex := 3
]
