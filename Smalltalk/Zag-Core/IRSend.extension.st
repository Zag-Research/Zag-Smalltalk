Extension { #name : 'IRSend' }

{ #category : '*Zag-Core' }
IRSend >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: 'p.send,'.
	self selector threadFormatOn: threadPrinter.
	^ nil
]
