Extension { #name : 'IRPushDup' }

{ #category : '*Zag-Inspector' }
IRPushDup >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: 'p.pushDup,'.
	^ nil
]
