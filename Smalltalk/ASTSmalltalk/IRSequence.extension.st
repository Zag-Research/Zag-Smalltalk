Extension { #name : 'IRSequence' }

{ #category : '*ASTSmalltalk' }
IRSequence >> threadFormatOn: threadPrinter [

	threadPrinter
		nextPutAll: '"label';
		nextPutAll: self orderNumber asString;
		nextPutAll: '",'
]
