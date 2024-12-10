Extension { #name : 'ASImageExporter' }

{ #category : '*Zag-Core-Tests' }
ASImageExporter >> put: anObject [

	^ currentHeap addU64: (anObject astImageObject: self)
]

{ #category : '*Zag-Core-Tests' }
ASImageExporter class >> test [

	^ super new testOutput
]

{ #category : '*Zag-Core-Tests' }
ASImageExporter >> testOutput [

	currentHeap := ASSubHeap new: 1000 at: 16r100000 exporter: self
]
