Extension { #name : 'IRPopIntoRemoteTemp' }

{ #category : '*ASTSmalltalk' }
IRPopIntoRemoteTemp >> threadFormatOn: threadPrinter [

	threadPrinter printOn: threadPrinter.
	^ nil
]
