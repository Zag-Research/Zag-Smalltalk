Extension { #name : 'Float' }

{ #category : '*Zag-Exporter' }
Float >> encodedBy: anASImage [

	^ anASImage objectFloat: self
]
