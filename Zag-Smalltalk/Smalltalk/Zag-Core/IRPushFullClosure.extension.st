Extension { #name : 'IRPushFullClosure' }

{ #category : '*Zag-Core' }
IRPushFullClosure >> threadFormatOn: threadPrinter [

	self printOn: threadPrinter.
	^ nil
]
