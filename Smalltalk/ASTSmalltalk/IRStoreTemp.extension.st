Extension { #name : #IRStoreTemp }

{ #category : #'*ASTSmalltalk' }
IRStoreTemp >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: 'p.storeTemp,'.
	threadPrinter printTempOffset: self name.
	^ nil
]
