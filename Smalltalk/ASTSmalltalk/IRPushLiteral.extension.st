Extension { #name : #IRPushLiteral }

{ #category : #'*ASTSmalltalk' }
IRPushLiteral >> threadFormatOn: stream [

	stream nextPutAll: 'p.pushLiteral,'.
	self literal printOn: stream.
	^ nil
]
