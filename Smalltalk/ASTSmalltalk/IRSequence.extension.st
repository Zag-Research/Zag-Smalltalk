Extension { #name : 'IRSequence' }

{ #category : '*ASTSmalltalk' }
IRSequence >> asAST [
	^ ASTBlock new from: self
]

{ #category : '*ASTSmalltalk' }
IRSequence >> threadFormatOn: threadPrinter [

	threadPrinter
		nextPutAll: '"label';
		nextPutAll: self orderNumber asString;
		nextPutAll: '",'
]
