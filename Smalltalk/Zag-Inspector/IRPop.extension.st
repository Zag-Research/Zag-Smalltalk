Extension { #name : 'IRPop' }

{ #category : '*Zag-Inspector' }
IRPop >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: 'p.pop,'.
	^ nil
]
