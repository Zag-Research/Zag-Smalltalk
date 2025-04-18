Extension { #name : 'OCMessageNode' }

{ #category : '*Zag-Exporter-Tests' }
OCMessageNode >> zagClosureArgClass: anObject [

	^ #BlockClosure
]

{ #category : '*Zag-Exporter-Tests' }
OCMessageNode >> zagClosureArgClass: anArgumentVariable assign: anOCVectorTempVariable [

	^ #BlockClosure
]

{ #category : '*Zag-Exporter-Tests' }
OCMessageNode >> zagClosureClass [

	^ #BlockClosure
]

{ #category : '*Zag-Exporter-Tests' }
OCMessageNode >> zagClosureReturnClass [

	^ #BlockClosure
]
