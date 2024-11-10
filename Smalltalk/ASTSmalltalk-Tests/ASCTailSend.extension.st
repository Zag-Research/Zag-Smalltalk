Extension { #name : 'ASCTailSend' }

{ #category : '*ASTSmalltalk-Tests' }
ASCTailSend >> interpretFor: anInterpreter [

	^ anInterpreter
		  restructureStack: restructure;
		  send: selector
]
