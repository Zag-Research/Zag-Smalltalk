Extension { #name : 'IRPushLiteral' }

{ #category : '*Zag-Core' }
IRPushLiteral >> threadFormatOn: stream [

	stream nextPutAll: 'p.pushLiteral,'.
	self literal printOn: stream.
	^ nil
]
