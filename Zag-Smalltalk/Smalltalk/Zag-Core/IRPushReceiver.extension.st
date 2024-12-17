Extension { #name : 'IRPushReceiver' }

{ #category : '*Zag-Core' }
IRPushReceiver >> threadFormatOn: threadPrinter [

	self printOn: threadPrinter.
	^ nil
]
