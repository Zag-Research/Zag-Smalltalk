Extension { #name : 'OCReturnNode' }

{ #category : '*Zag-Exporter-Tests' }
OCReturnNode >> zagClosureArgClass: anObject [

	^ #BlockClosure
]

{ #category : '*Zag-Exporter-Tests' }
OCReturnNode >> zagClosureClass [

	^ value zagClosureReturnClass
]
