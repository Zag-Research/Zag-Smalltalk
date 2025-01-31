Extension { #name : 'IRPopIntoRemoteTemp' }

{ #category : '*Zag-Inspector' }
IRPopIntoRemoteTemp >> threadFormatOn: threadPrinter [

	threadPrinter printOn: threadPrinter.
	^ nil
]
