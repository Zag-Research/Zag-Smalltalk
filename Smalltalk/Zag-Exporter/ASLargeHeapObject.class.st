"
I am storage that will be exported to a .lho file that will be loaded by a Zag runtime.
I contain a Large Heap Object that has no header - it's all data.
"
Class {
	#name : 'ASLargeHeapObject',
	#superclass : 'ASStorage',
	#category : 'Zag-Exporter-Exporting',
	#package : 'Zag-Exporter',
	#tag : 'Exporting'
}

{ #category : 'accessing' }
ASLargeHeapObject class >> extension [

	^ '.lho'
]

{ #category : 'instance creation' }
ASLargeHeapObject class >> new: words [

	^ self new allocate: words
]

{ #category : 'instance creation' }
ASLargeHeapObject class >> size: anExporter [

	^ self shouldNotBeRedefined
]

{ #category : 'initialization' }
ASLargeHeapObject >> initializeStorage [

	freeIndex := 1
]
