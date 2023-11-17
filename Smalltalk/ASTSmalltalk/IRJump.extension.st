Extension { #name : 'IRJump' }

{ #category : '*ASTSmalltalk' }
IRJump >> threadFormatOn: threadPrinter [

	^ self destination
]
