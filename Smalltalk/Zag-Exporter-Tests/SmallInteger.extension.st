Extension { #name : 'SmallInteger' }

{ #category : '*Zag-Exporter-Tests' }
SmallInteger >> zagClosureClass [

	(self between: -16r800000000000 and: 16r7fffffffffff) ifTrue: [ ^ #ThunkImmediate ].
	^  #BlockClosure
]

{ #category : '*Zag-Exporter-Tests' }
SmallInteger >> zagClosureReturnClass [

	(self between: -128 and: 127) ifTrue: [ ^ #ThunkReturnSmallInteger ].
	^ BlockClosure
]
