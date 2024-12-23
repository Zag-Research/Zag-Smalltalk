Extension { #name : 'ASImageExporter' }

{ #category : '*Zag-Exporter-Tests' }
ASImageExporter >> encodingOf: anObject [

	^ anObject encodedBy: self
]

{ #category : '*Zag-Exporter-Tests' }
ASImageExporter class >> test [

	^ super new testOutput
]

{ #category : '*Zag-Exporter-Tests' }
ASImageExporter >> testOutput [

	currentHeap := ASSubHeap new: 4096 at: 16r100000 exporter: self
]
