Extension { #name : #IRPop }

{ #category : #'*ASTSmalltalk' }
IRPop >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: 'p.pop,'.
	^ nil
]
