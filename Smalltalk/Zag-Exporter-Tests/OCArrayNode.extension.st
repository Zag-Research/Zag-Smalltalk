Extension { #name : 'OCArrayNode' }

{ #category : '*Zag-Exporter-Tests' }
OCArrayNode >> zagClosureArgClass: anArgumentVariable [ 

	^ #BlockClosure
]

{ #category : '*Zag-Exporter-Tests' }
OCArrayNode >> zagClosureClass [

	statements size = 0 ifTrue: [ ^ #Array ].
	^ #BlockClosure
]

{ #category : '*Zag-Exporter-Tests' }
OCArrayNode >> zagClosureReturnClass [

	^ #BlockClosure
]
