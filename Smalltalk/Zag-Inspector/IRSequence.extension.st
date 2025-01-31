Extension { #name : 'IRSequence' }

{ #category : '*Zag-Inspector' }
IRSequence >> threadFormatOn: threadPrinter [

	threadPrinter
		nextPutAll: '"label';
		nextPutAll: self orderNumber asString;
		nextPutAll: '",'
]
