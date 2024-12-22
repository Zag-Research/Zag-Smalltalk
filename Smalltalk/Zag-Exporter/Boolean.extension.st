Extension { #name : 'Boolean' }

{ #category : '*Zag-Exporter' }
Boolean >> encodedBy: anASImage [

	^ anASImage objectBoolean: self
]
