Extension { #name : 'IRPushReceiver' }

{ #category : '*Zag-Inspector' }
IRPushReceiver >> threadFormatOn: threadPrinter [

	self printOn: threadPrinter.
	^ nil
]
