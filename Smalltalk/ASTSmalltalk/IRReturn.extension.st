Extension { #name : 'IRReturn' }

{ #category : '*ASTSmalltalk' }
IRReturn >> threadFormatOn: threadPrinter [

	threadPrinter nextPutAll: 'p.@"returnWithContext",'.
	^ nil
]
