Extension { #name : 'IRPushLiteralVariable' }

{ #category : '*ASTSmalltalk' }
IRPushLiteralVariable >> threadFormatOn: threadPrinter [

	self printOn: threadPrinter.
	^ nil
]
