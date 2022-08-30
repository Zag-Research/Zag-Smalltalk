Extension { #name : #IRPopIntoTemp }

{ #category : #'*ASTSmalltalk' }
IRPopIntoTemp >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: 'p.popIntoTemp,'.
	threadPrinter printTempOffset: self name.
	^ nil
]
