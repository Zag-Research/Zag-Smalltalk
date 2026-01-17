Extension { #name : 'Object' }

{ #category : '*Zag-Exporter-Tests' }
Object >> zagClosureReturnClass [

	(self between: -128 and: 127) ifTrue: [ ^ #ThunkReturnObject ].
	^ BlockClosure
]
