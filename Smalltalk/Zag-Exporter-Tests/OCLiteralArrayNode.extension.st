Extension { #name : 'OCLiteralArrayNode' }

{ #category : '*Zag-Exporter-Tests' }
OCLiteralArrayNode >> zagClosureArgClass: anArgumentVariable [

	^ #BlockClosure
]

{ #category : '*Zag-Exporter-Tests' }
OCLiteralArrayNode >> zagClosureArgClass: anArgumentVariable assign: anOCVectorTempVariable [

	^ #BlockClosure
]

{ #category : '*Zag-Exporter-Tests' }
OCLiteralArrayNode >> zagClosureClass [

	contents size = 0 ifTrue: [ ^  isByteArray ifTrue: [ #ByteArray ] ifFalse: [ #Array ] ].
	^ #BlockClosure
]

{ #category : '*Zag-Exporter-Tests' }
OCLiteralArrayNode >> zagClosureReturnClass [

	^ BlockClosure
]
