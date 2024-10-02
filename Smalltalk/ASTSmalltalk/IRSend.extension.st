Extension { #name : 'IRSend' }

{ #category : '*ASTSmalltalk' }
IRSend >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: 'p.send,'.
	self selector threadFormatOn: threadPrinter.
	^ nil
]
