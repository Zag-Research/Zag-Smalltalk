Extension { #name : 'IRReturn' }

{ #category : '*Zag-Inspector' }
IRReturn >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: 'p.@"returnWithContext",'.
	^ nil
]
