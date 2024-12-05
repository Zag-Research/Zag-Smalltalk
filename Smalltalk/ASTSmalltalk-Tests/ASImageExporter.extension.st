Extension { #name : 'ASImageExporter' }

{ #category : '*ASTSmalltalk-Tests' }
ASImageExporter >> put: anObject [

	^ currentHeap addU64: (anObject astImageObject: self)
]

{ #category : '*ASTSmalltalk-Tests' }
ASImageExporter class >> test [

	^ super new testOutput
]

{ #category : '*ASTSmalltalk-Tests' }
ASImageExporter >> testOutput [

	currentHeap := ASSubHeap new: 1000 at: 16r100000 exporter: self
]
