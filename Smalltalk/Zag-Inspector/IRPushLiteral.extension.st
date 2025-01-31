Extension { #name : 'IRPushLiteral' }

{ #category : '*Zag-Inspector' }
IRPushLiteral >> threadFormatOn: stream [

	stream nextPutAll: 'p.pushLiteral,'.
	self literal printOn: stream.
	^ nil
]
