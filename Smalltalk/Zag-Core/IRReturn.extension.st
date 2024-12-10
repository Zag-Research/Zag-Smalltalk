Extension { #name : 'IRReturn' }

{ #category : '*Zag-Core' }
IRReturn >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: 'p.@"returnWithContext",'.
	^ nil
]
