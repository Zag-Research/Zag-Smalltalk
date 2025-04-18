Extension { #name : 'OCVectorTempVariable' }

{ #category : '*Zag-Exporter-Tests' }
OCVectorTempVariable >> zagBlockAssign [

	^ #BlockAssignLocal
]

{ #category : '*Zag-Exporter-Tests' }
OCVectorTempVariable >> zagClosureReturnClass [

	^ #ThunkReturnLocal
]

{ #category : '*Zag-Exporter-Tests' }
OCVectorTempVariable >> zagIsLocal [

	^ true
]
