Extension { #name : 'CopiedLocalVariable' }

{ #category : '*Zag-Exporter-Tests' }
CopiedLocalVariable >> zagClosureClass [

	^ originalVar zagClosureClass
]

{ #category : '*Zag-Exporter-Tests' }
CopiedLocalVariable >> zagClosureReturnClass [

	^ #ThunkReturnInstance
]
