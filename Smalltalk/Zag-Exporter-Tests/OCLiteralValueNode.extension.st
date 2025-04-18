Extension { #name : 'OCLiteralValueNode' }

{ #category : '*Zag-Exporter-Tests' }
OCLiteralValueNode >> zagClosureArgClass: anObject [

	^ #BlockClosure:literal
]

{ #category : '*Zag-Exporter-Tests' }
OCLiteralValueNode >> zagClosureArgClass: anArgumentVariable assign: anOCVectorTempVariable [

	^ #BlockClosure
]

{ #category : '*Zag-Exporter-Tests' }
OCLiteralValueNode >> zagClosureClass [

	^ value zagClosureClass
]

{ #category : '*Zag-Exporter-Tests' }
OCLiteralValueNode >> zagClosureReturnClass [

	^ value zagClosureReturnClass
]
