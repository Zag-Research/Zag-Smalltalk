Extension { #name : #IRPushFullClosure }

{ #category : #'*ASTSmalltalk' }
IRPushFullClosure >> threadFormatOn: threadPrinter [

	self printOn: threadPrinter.
	^ nil
]
