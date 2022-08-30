Extension { #name : #IRPushTemp }

{ #category : #'*ASTSmalltalk' }
IRPushTemp >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: 'p.pushTemp,'.
	threadPrinter printTempOffset: self name.
	^ nil
]
