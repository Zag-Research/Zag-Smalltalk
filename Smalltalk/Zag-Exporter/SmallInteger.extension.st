Extension { #name : 'SmallInteger' }

{ #category : '*Zag-Exporter' }
SmallInteger >> encodedBy: anASImage [

	^ anASImage objectSmallInteger: self
]
