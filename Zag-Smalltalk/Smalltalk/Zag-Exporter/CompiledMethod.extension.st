Extension { #name : 'CompiledMethod' }

{ #category : '*Zag-Exporter' }
CompiledMethod >> encodedBy: anASImage [

	^ anASImage objectMethod: self
]
