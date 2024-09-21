Extension { #name : 'IRTempVector' }

{ #category : '*ASTSmalltalk' }
IRTempVector >> threadFormatOn: threadPrinter [

	self printOn: threadPrinter.
	^ nil
]
