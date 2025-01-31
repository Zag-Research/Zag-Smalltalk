Extension { #name : 'IRPushFullClosure' }

{ #category : '*Zag-Inspector' }
IRPushFullClosure >> threadFormatOn: threadPrinter [

	self printOn: threadPrinter.
	^ nil
]
