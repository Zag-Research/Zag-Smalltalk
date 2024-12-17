Extension { #name : 'ByteSymbol' }

{ #category : '*Zag-Exporter' }
ByteSymbol >> encodedBy: anASImage [

	^ anASImage objectSymbol: self
]
