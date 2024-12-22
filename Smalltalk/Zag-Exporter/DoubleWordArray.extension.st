Extension { #name : 'DoubleWordArray' }

{ #category : '*Zag-Exporter' }
DoubleWordArray >> encodedBy: anASImage [

	^ anASImage objectNonObjectArray: self
]
