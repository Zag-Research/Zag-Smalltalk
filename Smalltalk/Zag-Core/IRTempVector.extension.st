Extension { #name : 'IRTempVector' }

{ #category : '*Zag-Core' }
IRTempVector >> threadFormatOn: threadPrinter [

	self printOn: threadPrinter.
	^ nil
]
