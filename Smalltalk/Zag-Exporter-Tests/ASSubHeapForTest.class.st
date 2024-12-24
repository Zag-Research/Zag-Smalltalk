"
I am a slight modification of ASSubHeap for test purposes.
"
Class {
	#name : 'ASSubHeapForTest',
	#superclass : 'ASSubHeap',
	#category : 'Zag-Exporter-Tests-Support',
	#package : 'Zag-Exporter-Tests',
	#tag : 'Support'
}

{ #category : 'adding' }
ASSubHeapForTest class >> defaultAddress [

	^ 16r100000
]

{ #category : 'adding' }
ASSubHeapForTest class >> defaultSize [

	^ 4096
]

{ #category : 'private' }
ASSubHeapForTest >> array [

	^ storage first: freeIndex -1
]
