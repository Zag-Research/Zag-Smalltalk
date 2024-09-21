Extension { #name : 'IRPushDup' }

{ #category : '*ASTSmalltalk' }
IRPushDup >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: 'p.pushDup,'.
	^ nil
]
