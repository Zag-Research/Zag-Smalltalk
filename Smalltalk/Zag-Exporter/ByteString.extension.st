Extension { #name : 'ByteString' }

{ #category : '*Zag-Exporter' }
ByteString >> encodedBy: anASImage [

	^ anASImage objectString: self
]
