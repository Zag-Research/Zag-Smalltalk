Extension { #name : 'IRPushLiteralVariable' }

{ #category : '*Zag-Core' }
IRPushLiteralVariable >> threadFormatOn: threadPrinter [

	self printOn: threadPrinter.
	^ nil
]
