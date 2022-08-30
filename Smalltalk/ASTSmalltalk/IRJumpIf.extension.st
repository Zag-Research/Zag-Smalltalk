Extension { #name : #IRJumpIf }

{ #category : #'*ASTSmalltalk' }
IRJumpIf >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: 'p.ifFalse,'.
	destination threadFormatOn: threadPrinter.
	^ self otherwise
]
