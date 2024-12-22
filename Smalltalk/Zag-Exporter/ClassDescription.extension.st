Extension { #name : 'ClassDescription' }

{ #category : '*Zag-Exporter' }
ClassDescription >> encodedBy: anASImage [

	^ anASImage objectClass: self
]

{ #category : '*Zag-Exporter' }
ClassDescription >> zagClass [

	^ self
]
