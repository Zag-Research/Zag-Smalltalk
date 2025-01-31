Extension { #name : 'IRSend' }

{ #category : '*Zag-Inspector' }
IRSend >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: 'p.send,'.
	self selector threadFormatOn: threadPrinter.
	^ nil
]
