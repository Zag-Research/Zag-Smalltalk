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
		'exporter',
		'freeIndex'
	],
	#pools : [
		'ZagConstants'
	],
	#category : 'Zag-Exporter-Exporting',
	#package : 'Zag-Exporter',
	#tag : 'Exporting'
}

{ #category : 'adding' }
ASStorage class >> defaultAddress [

	^ self subclassResponsibility
]

{ #category : 'adding' }
ASStorage class >> defaultSize [

	^ self subclassResponsibility
]

{ #category : 'accessing' }
ASStorage class >> extension [

	^ self subclassResponsibility
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
ASStorage >> address [

	^ address
]

{ #category : 'output' }
ASStorage >> at: location size: size do: aBlock [

	| offset |
	offset := location - address / 8 + 1.
	1 to: size do: [ :i |
		storage at: offset + i put: (aBlock value: i) ]
]

{ #category : 'output' }
ASStorage >> at: location with: aCollection do: aBlock [

	^ self at: location size: aCollection size do: [ : i | aBlock value: (aCollection at: i) ]

]

{ #category : 'accessing' }
ASStorage >> exporter: anImageExporter [

	exporter := anImageExporter
]

{ #category : 'output' }
ASStorage >> filename [

	^ 'zag' , ((address + 16r1000000000000) hexString allButFirst: 4)
	  , self class extension
]

{ #category : 'testing' }
ASStorage >> hasSpaceFor: anInteger [

	^ freeIndex + anInteger <= storage size
]

{ #category : 'output' }
ASStorage >> header: header size: size [

	| headerLocation |
	headerLocation := freeIndex.
	freeIndex := freeIndex + size + 1.
	storage at: headerLocation put: header.
	^ headerLocation - 1 * 8 + address
]

{ #category : 'initialization' }
ASStorage >> initialize [

	super initialize.
	address := self class defaultAddress.
	storage := DoubleWordArray new: self class defaultSize.
	self initializeStorage
]

{ #category : 'initialization' }
ASStorage >> initializeStorage [

	^ self subclassResponsibility
]

{ #category : 'output' }
ASStorage >> prepareForOutput [

]

{ #category : 'accessing' }
ASStorage >> size [

	^ storage size * 8
]

{ #category : 'output' }
ASStorage >> toDirectory: aDirectorReference [

	| stream |
	stream := (aDirectorReference / self filename openWritable: true)
		          binaryWriteStream.
	[ self toStream: stream ] ensure: [
		stream
			flush;
			close ]
]

{ #category : 'output' }
ASStorage >> toStream: aStream [

	1 to: freeIndex - 1 do: [ :i | | word|
		word := storage at: i.
		1 to: 8 do: [ :b | aStream nextPut: (word byteAt: b) ] ]
]
