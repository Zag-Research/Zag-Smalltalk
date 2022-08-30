Extension { #name : #IRPushReceiver }

{ #category : #'*ASTSmalltalk' }
IRPushReceiver >> threadFormatOn: threadPrinter [

	self printOn: threadPrinter.
	^ nil
]
