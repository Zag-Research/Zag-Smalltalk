Extension { #name : 'IRPushDup' }

{ #category : '*Zag-Core' }
IRPushDup >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: 'p.pushDup,'.
	^ nil
]
