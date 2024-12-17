Extension { #name : 'IRPop' }

{ #category : '*Zag-Core' }
IRPop >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: 'p.pop,'.
	^ nil
]
