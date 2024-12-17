Extension { #name : 'IRPopIntoRemoteTemp' }

{ #category : '*Zag-Core' }
IRPopIntoRemoteTemp >> threadFormatOn: threadPrinter [

	threadPrinter printOn: threadPrinter.
	^ nil
]
