Extension { #name : 'IRSequence' }

{ #category : '*Zag-Core' }
IRSequence >> threadFormatOn: threadPrinter [

	threadPrinter
		nextPutAll: '"label';
		nextPutAll: self orderNumber asString;
		nextPutAll: '",'
]
